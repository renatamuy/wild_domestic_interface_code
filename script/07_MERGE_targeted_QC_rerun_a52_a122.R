# =============================================================================
# 08_merge_targeted_QC_rerun_a52_a122.R
#
# Purpose:
#   Create results_synthesis_consolidated/ by replacing the original production
#   records for a52 and a122 with the records from the clean-PDF targeted rerun.
#
# Principles:
#   * results_synthesis/ is READ ONLY.
#   * results_synthesis_rerun_a52_a122/ is READ ONLY.
#   * all new files are written only to results_synthesis_consolidated/.
#   * original human-validation/adjudication results are NOT recalculated.
#   * random audit samples from the 2-paper rerun are NOT merged.
#   * stale downstream Figure 5/manuscript products are NOT copied; regenerate
#     them from the consolidated plot_data / synthesis tables.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(fs)
  library(openxlsx)
})

# ---- PATHS -------------------------------------------------------------------
# Assumes this script is run from project/script, matching the synthesis pipeline.
root_dir <- normalizePath("..", winslash = "/", mustWork = TRUE)

original_dir <- file.path(root_dir, "results_synthesis")
rerun_dir    <- file.path(root_dir, "results_synthesis_rerun_a52_a122")
output_dir   <- file.path(root_dir, "results_synthesis_consolidated")
qc_dir       <- file.path(output_dir, "QC_a52_a122")
validation_dir <- file.path(output_dir, "validation_original")

prefix <- "evidence_first_test"
target_ids <- c("a52", "a122")

# ---- HARD SAFETY STOPS -------------------------------------------------------
if (!dir_exists(original_dir)) {
  stop("Missing original directory: ", original_dir)
}
if (!dir_exists(rerun_dir)) {
  stop("Missing rerun directory: ", rerun_dir)
}

norm <- function(x) normalizePath(x, winslash = "/", mustWork = FALSE)

if (norm(output_dir) %in% c(norm(original_dir), norm(rerun_dir), norm(root_dir))) {
  stop("SAFETY STOP: output_dir resolves to a protected source/project directory.")
}

# Deliberately refuse to overwrite an existing consolidation.
if (dir_exists(output_dir)) {
  stop(
    "SAFETY STOP: ", output_dir, " already exists.\n",
    "Review/remove/rename it manually before creating a new consolidation."
  )
}

dir_create(output_dir)
dir_create(qc_dir)
dir_create(validation_dir)

# ---- HELPERS -----------------------------------------------------------------
read_required <- function(dir, filename) {
  path <- file.path(dir, filename)
  if (!file_exists(path)) stop("Required file missing: ", path)
  read_csv(path, show_col_types = FALSE, progress = FALSE)
}

write_clean <- function(x, filename) {
  write_csv(x, file.path(output_dir, filename), na = "")
}

ids_present <- function(df, id_col) {
  if (!id_col %in% names(df)) {
    stop("Expected ID column '", id_col, "' is absent.")
  }
  unique(as.character(df[[id_col]]))
}

replace_target_rows <- function(original, rerun, id_col, label) {
  old_ids <- ids_present(original, id_col)
  new_ids <- ids_present(rerun, id_col)

  unexpected <- setdiff(new_ids, target_ids)
  if (length(unexpected) > 0) {
    stop(
      "SAFETY STOP [", label, "]: rerun contains non-target IDs: ",
      paste(unexpected, collapse = ", ")
    )
  }

  # Long-format tables may legitimately gain target rows after improved PDF
  # extraction (e.g. a disease or quantitative measure missed originally).
  # Therefore a52/a122 are not required to have existed in the original
  # version of every table. Strict one-row-per-paper invariants are checked
  # separately below for the core paper-level outputs.

  # Archive target rows before replacement.
  old_target <- original |> filter(.data[[id_col]] %in% target_ids)
  new_target <- rerun    |> filter(.data[[id_col]] %in% target_ids)

  write_csv(
    old_target,
    file.path(qc_dir, paste0(label, "__ORIGINAL_a52_a122.csv")),
    na = ""
  )
  write_csv(
    new_target,
    file.path(qc_dir, paste0(label, "__RERUN_a52_a122.csv")),
    na = ""
  )

  # bind_rows safely unions columns if a schema gained/lost a field.
  consolidated <- bind_rows(
    original |> filter(!.data[[id_col]] %in% target_ids),
    new_target
  )

  list(
    data = consolidated,
    old_target = old_target,
    new_target = new_target,
    n_original = nrow(original),
    n_rerun_target = nrow(new_target),
    n_consolidated = nrow(consolidated)
  )
}

# ---- TABLES TO CONSOLIDATE ---------------------------------------------------
# Full/substantive outputs only. Human audit samples are intentionally excluded.
specs <- tribble(
  ~filename,                                              ~id_col,      ~label,
  paste0(prefix, "_paper_summary_lean.csv"),              "article_id", "paper_summary",
  paste0(prefix, "_plot_data.csv"),                       "paper_id",   "plot_data",
  paste0(prefix, "_disease_long.csv"),                    "article_id", "disease_long",
  paste0(prefix, "_quantitative_measures_long.csv"),      "article_id", "quantitative_measures",
  paste0(prefix, "_conceptual_evidence.csv"),             "paper_id",   "conceptual_evidence",
  paste0(prefix, "_evidence_long_lean.csv"),              "article_id", "evidence_long",
  paste0(prefix, "_audit_long_lean.csv"),                 "paper_id",   "audit_long",
  paste0(prefix, "_module_audit_lean.csv"),               "article_id", "module_audit",
  paste0(prefix, "_pdf_quality.csv"),                     "article_id", "pdf_quality"
)

consolidated <- list()
merge_log <- list()

for (i in seq_len(nrow(specs))) {
  filename <- specs$filename[[i]]
  id_col   <- specs$id_col[[i]]
  label    <- specs$label[[i]]

  message("Consolidating: ", label)

  original <- read_required(original_dir, filename)
  rerun    <- read_required(rerun_dir, filename)

  z <- replace_target_rows(original, rerun, id_col, label)

  consolidated[[label]] <- z$data
  write_clean(z$data, filename)

  merge_log[[label]] <- tibble(
    table = label,
    filename = filename,
    id_column = id_col,
    original_rows = z$n_original,
    rerun_target_rows = z$n_rerun_target,
    consolidated_rows = z$n_consolidated
  )
}

merge_log <- bind_rows(merge_log)
write_csv(merge_log, file.path(qc_dir, "merge_table_log.csv"))

# ---- CRITICAL INVARIANTS -----------------------------------------------------
summary_df <- consolidated$paper_summary
plot_df    <- consolidated$plot_data
pdf_df     <- consolidated$pdf_quality

if (nrow(summary_df) != 270) {
  stop("SAFETY STOP: consolidated paper_summary has ", nrow(summary_df),
       " rows; expected exactly 270.")
}
if (n_distinct(summary_df$article_id) != 270) {
  stop("SAFETY STOP: paper_summary article_id values are not unique.")
}
if (!setequal(intersect(summary_df$article_id, target_ids), target_ids)) {
  stop("SAFETY STOP: a52/a122 missing from consolidated paper_summary.")
}

if (nrow(plot_df) != 270 || n_distinct(plot_df$paper_id) != 270) {
  stop("SAFETY STOP: consolidated plot_data must contain 270 unique papers.")
}

if (nrow(pdf_df) != 270 || n_distinct(pdf_df$article_id) != 270) {
  stop("SAFETY STOP: consolidated pdf_quality must contain 270 unique papers.")
}

target_pdf <- pdf_df |> filter(article_id %in% target_ids)
if (nrow(target_pdf) != 2 || any(target_pdf$pdf_text_status != "OK")) {
  stop("SAFETY STOP: a52/a122 are not both PDF status OK after consolidation.")
}

target_plot <- plot_df |>
  filter(paper_id %in% target_ids) |>
  select(any_of(c("paper_id", "host_orientation_score", "interface_score")))

if (nrow(target_plot) != 2) {
  stop("SAFETY STOP: expected exactly two target rows in consolidated plot_data.")
}

# ---- REGENERATE BENCHMARK-BY-PAPER FROM CONSOLIDATED SUMMARY ----------------
# This describes the records USED in the consolidated dataset. It is not claimed
# to be the historical cost/runtime of one uninterrupted 270-paper API run.
benchmark_by_paper <- summary_df |>
  transmute(
    paper_id = article_id,
    processing_time_min,
    prompt_tokens = prompt_tokens_total,
    output_tokens = output_tokens_total,
    total_tokens = prompt_tokens_total + output_tokens_total,
    estimated_api_cost_usd,
    interface_boolean_completion,
    hosts_role_completion,
    any_parse_error,
    any_boolean_evidence_inconsistency,
    any_quote_not_found,
    interface_needs_review,
    pdf_text_status
  )

write_clean(
  benchmark_by_paper,
  paste0(prefix, "_benchmark_by_paper_lean.csv")
)

benchmark_consolidated_summary <- tibble(
  dataset = "results_synthesis_consolidated",
  description = paste(
    "268 original production records plus targeted clean-PDF rerun records",
    "for a52 and a122; not a single uninterrupted API run."
  ),
  completed_papers = nrow(summary_df),
  total_runtime_hours_for_records_used =
    sum(summary_df$processing_time_sec, na.rm = TRUE) / 3600,
  total_prompt_tokens_for_records_used =
    sum(summary_df$prompt_tokens_total, na.rm = TRUE),
  total_output_tokens_for_records_used =
    sum(summary_df$output_tokens_total, na.rm = TRUE),
  estimated_api_cost_usd_for_records_used =
    sum(summary_df$estimated_api_cost_usd, na.rm = TRUE),
  papers_pdf_not_ok =
    sum(summary_df$pdf_text_status != "OK", na.rm = TRUE),
  papers_with_parse_error =
    sum(summary_df$any_parse_error, na.rm = TRUE),
  papers_with_quote_not_found =
    sum(summary_df$any_quote_not_found, na.rm = TRUE),
  papers_with_boolean_evidence_inconsistency =
    sum(summary_df$any_boolean_evidence_inconsistency, na.rm = TRUE)
)

write_clean(
  benchmark_consolidated_summary,
  paste0(prefix, "_benchmark_consolidated_summary.csv")
)

# Preserve original and rerun run-level benchmark/logs as provenance rather than
# presenting a hybrid processing log as if it came from one run.
provenance_files <- c(
  paste0(prefix, "_benchmark_run_summary_lean.csv"),
  paste0(prefix, "_processing_log.csv")
)

for (f in provenance_files) {
  src_old <- file.path(original_dir, f)
  src_new <- file.path(rerun_dir, f)
  if (file_exists(src_old)) {
    file_copy(src_old, file.path(qc_dir, paste0("ORIGINAL__", f)), overwrite = FALSE)
  }
  if (file_exists(src_new)) {
    file_copy(src_new, file.path(qc_dir, paste0("RERUN__", f)), overwrite = FALSE)
  }
}

# ---- PRESERVE ORIGINAL VALIDATION, WITHOUT RE-SAMPLING -----------------------
# These are copied as frozen validation provenance. The 87/100 human validation
# is not recalculated using the clean-PDF rerun.
validation_files <- c(
  "01_evidence_human_audit.xlsx",
  "02_evidence_human_audit.xlsx",
  paste0(prefix, "_lean_human_audit.xlsx"),
  paste0(prefix, "_audit_sample_lean.csv"),
  paste0(prefix, "_quantitative_audit_sample.csv")
)

for (f in validation_files) {
  src <- file.path(original_dir, f)
  if (file_exists(src)) {
    file_copy(src, file.path(validation_dir, f), overwrite = FALSE)
  }
}

original_adjudication <- file.path(original_dir, "adjudication")
if (dir_exists(original_adjudication)) {
  dir_copy(
    original_adjudication,
    file.path(validation_dir, "adjudication"),
    overwrite = FALSE
  )
}

# ---- QC COMPARISON TABLES ----------------------------------------------------
old_summary <- read_required(original_dir, paste0(prefix, "_paper_summary_lean.csv")) |>
  filter(article_id %in% target_ids)
new_summary <- read_required(rerun_dir, paste0(prefix, "_paper_summary_lean.csv")) |>
  filter(article_id %in% target_ids)

old_plot <- read_required(original_dir, paste0(prefix, "_plot_data.csv")) |>
  filter(paper_id %in% target_ids)
new_plot <- read_required(rerun_dir, paste0(prefix, "_plot_data.csv")) |>
  filter(paper_id %in% target_ids)

old_pdf <- read_required(original_dir, paste0(prefix, "_pdf_quality.csv")) |>
  filter(article_id %in% target_ids)
new_pdf <- read_required(rerun_dir, paste0(prefix, "_pdf_quality.csv")) |>
  filter(article_id %in% target_ids)

# Side-by-side target summaries, retaining all fields with OLD_/NEW_ prefixes.
summary_comparison <- full_join(
  old_summary |> rename_with(~ paste0("OLD_", .x), -article_id),
  new_summary |> rename_with(~ paste0("NEW_", .x), -article_id),
  by = "article_id"
)
write_csv(summary_comparison, file.path(qc_dir, "a52_a122_paper_summary_comparison.csv"))

plot_comparison <- full_join(
  old_plot |> rename_with(~ paste0("OLD_", .x), -paper_id),
  new_plot |> rename_with(~ paste0("NEW_", .x), -paper_id),
  by = "paper_id"
)
write_csv(plot_comparison, file.path(qc_dir, "a52_a122_plot_data_comparison.csv"))

pdf_comparison <- full_join(
  old_pdf |> rename_with(~ paste0("OLD_", .x), -article_id),
  new_pdf |> rename_with(~ paste0("NEW_", .x), -article_id),
  by = "article_id"
)
write_csv(pdf_comparison, file.path(qc_dir, "a52_a122_pdf_quality_comparison.csv"))

# Quantitative-measure counts before/after.
old_quant <- read_required(original_dir, paste0(prefix, "_quantitative_measures_long.csv"))
new_quant <- read_required(rerun_dir, paste0(prefix, "_quantitative_measures_long.csv"))

quant_count_comparison <- tibble(article_id = target_ids) |>
  left_join(
    old_quant |> count(article_id, name = "original_n_quantitative_measures"),
    by = "article_id"
  ) |>
  left_join(
    new_quant |> count(article_id, name = "rerun_n_quantitative_measures"),
    by = "article_id"
  ) |>
  mutate(
    across(
      c(original_n_quantitative_measures, rerun_n_quantitative_measures),
      ~ replace_na(.x, 0L)
    )
  )
write_csv(
  quant_count_comparison,
  file.path(qc_dir, "a52_a122_quantitative_measure_counts.csv")
)

# ---- CONSOLIDATED RESULTS WORKBOOK -------------------------------------------
# Do not regenerate a new random audit sample here.
readme_sheet <- tibble(
  sheet = c(
    "paper_summary",
    "plot_data",
    "disease_long",
    "quantitative_measures",
    "conceptual_evidence",
    "pdf_quality",
    "benchmark"
  ),
  purpose = c(
    "One row per paper; a52/a122 replaced by clean-PDF rerun records.",
    "Direct input for Figure 5; regenerate downstream Figure 5 products.",
    "One row per disease/transmission-mode combination.",
    "One row per interface-relevant quantitative measure.",
    "One row per paper with explicit/implicit interface statement.",
    "PDF text-extraction quality diagnostics.",
    "Per-paper runtime/token/QA metrics for records used in consolidated dataset."
  )
)

write.xlsx(
  list(
    README = readme_sheet,
    paper_summary = consolidated$paper_summary,
    plot_data = consolidated$plot_data,
    disease_long = consolidated$disease_long,
    quantitative_measures = consolidated$quantitative_measures,
    conceptual_evidence = consolidated$conceptual_evidence,
    pdf_quality = consolidated$pdf_quality,
    benchmark = benchmark_by_paper
  ),
  file = file.path(output_dir, paste0(prefix, "_lean_results_CONSOLIDATED.xlsx")),
  overwrite = FALSE
)

# ---- README / PROVENANCE -----------------------------------------------------
readme_lines <- c(
  "RESULTS SYNTHESIS CONSOLIDATED",
  "==============================",
  "",
  "Purpose",
  "-------",
  "This directory contains the consolidated evidence-synthesis dataset after",
  "targeted source-document quality control for papers a52 and a122.",
  "",
  "Source datasets",
  "---------------",
  "1. results_synthesis/ = frozen original 270-paper production run.",
  "2. results_synthesis_rerun_a52_a122/ = targeted rerun using replacement",
  "   machine-readable PDFs for a52 and a122 with the same frozen extraction",
  "   and deterministic classification workflow.",
  "",
  "Consolidation rule",
  "------------------",
  "For substantive full-corpus output tables, all original rows belonging to",
  "a52/a122 were removed and replaced with the corresponding targeted-rerun",
  "rows. All other papers remain from the original production run.",
  "",
  "Validation",
  "----------",
  "Original human validation/adjudication is preserved unchanged in",
  "validation_original/. Rerun audit samples were not merged and the original",
  "validation estimate is not recalculated.",
  "",
  "QC provenance",
  "-------------",
  "QC_a52_a122/ contains original and rerun target rows, comparison tables,",
  "the merge log, and original/rerun run-level processing provenance.",
  "",
  "Downstream products",
  "-------------------",
  "Previously generated Figure 5 tables/figures and manuscript summary products",
  "were NOT copied because they may be stale after a52/a122 replacement.",
  "Regenerate them from the consolidated synthesis outputs.",
  "",
  paste0("Created: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
)
writeLines(readme_lines, file.path(output_dir, "README_CONSOLIDATION.txt"))

# ---- FINAL REPORT ------------------------------------------------------------
message("")
message("==============================================================")
message("CONSOLIDATION COMPLETE")
message("Output: ", output_dir)
message("Papers in consolidated summary: ", nrow(summary_df))
message("Unique paper IDs: ", n_distinct(summary_df$article_id))
message("a52/a122 PDF status: ",
        paste(target_pdf$article_id, target_pdf$pdf_text_status,
              sep = "=", collapse = "; "))
message("Original results were NOT modified.")
message("Rerun results were NOT modified.")
message("Regenerate downstream Figure 5/manuscript products from consolidated data.")
message("==============================================================")
