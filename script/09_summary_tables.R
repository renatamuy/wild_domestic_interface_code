rm(list = ls())
gc()

suppressPackageStartupMessages({
  library(tidyverse)
  library(openxlsx)
  library(readxl)
  library(fs)
  library(here)
})

# ============================================================
# DERIVED OUTPUT 1
# MANUSCRIPT SUMMARY TABLES
# ============================================================

setwd(here())
setwd("script")

root_dir <- normalizePath("..", winslash = "/", mustWork = TRUE)
results_dir <- file.path(root_dir, "results_synthesis_consolidated")

test_mode <- TRUE
final_prefix <- if (test_mode) "evidence_first_test" else "evidence_first_full"

paper_file <- file.path(
  results_dir,
  paste0(final_prefix, "_paper_summary_lean.csv")
)

disease_file <- file.path(
  results_dir,
  paste0(final_prefix, "_disease_long.csv")
)

quantitative_file <- file.path(
  results_dir,
  paste0(final_prefix, "_quantitative_measures_long.csv")
)

if (!file_exists(paper_file)) {
  stop("Paper summary not found. Run the lean synthesis pipeline first.")
}

papers <- read_csv(paper_file, show_col_types = FALSE)

disease_long <- if (file_exists(disease_file)) {
  read_csv(disease_file, show_col_types = FALSE)
} else {
  tibble()
}

quantitative_long <- if (file_exists(quantitative_file)) {
  read_csv(quantitative_file, show_col_types = FALSE)
} else {
  tibble()
}

# ------------------------------------------------------------
# Main one-row-per-paper table
# ------------------------------------------------------------
paper_summary_table <- papers |>
  transmute(
    paper_id = article_id,
    master_record_id,
    author = Author,
    title = Title,
    journal = Journal,
    doi = DOI,
    metadata_join_status,
    study_type,
    study_design,
    country = countries_investigated,
    animal_focus,
    wildlife_role,
    domestic_role,
    wildlife_taxa,
    domestic_taxa,
    diseases_or_pathogens,
    disease_agent_types,
    transmission_modes,
    host_orientation,
    host_orientation_label,
    interface_operationalisation,
    interface_label,
    interface_explicit,
    quantitative_interface_measures_n,
    quantitative_interface_measure_names
  )

# ------------------------------------------------------------
# Frequency summaries
# ------------------------------------------------------------
study_type_counts <- papers |>
  count(study_type, sort = TRUE, name = "n") |>
  mutate(percent = round(100 * n / sum(n), 1))

animal_focus_counts <- papers |>
  count(animal_focus, sort = TRUE, name = "n") |>
  mutate(percent = round(100 * n / sum(n), 1))

split_semicolon_counts <- function(data, column, output_name) {
  data |>
    select(value = all_of(column)) |>
    filter(!is.na(value), nzchar(value)) |>
    separate_rows(value, sep = ";\\s*") |>
    mutate(value = str_squish(value)) |>
    filter(nzchar(value)) |>
    count(value, sort = TRUE, name = "n") |>
    rename(!!output_name := value)
}

# ------------------------------------------------------------
# Host-group classification using the frozen manual dictionary
# ------------------------------------------------------------
# The manual workbook is the single source of truth for host-group assignment.
# No taxonomic classification is inferred in this script.
#
# Expected workbook sheets:
#   wildlife_taxa_dict: wildlife_taxon_original, wildlife_host_group
#   domestic_taxa_dict: domestic_taxon_original, domestic_host_group
#
# Put WILD_DOMESTIC_MANUAL_DICTIONARY.xlsx in the project root directory.
manual_dictionary_file <- file.path(
  root_dir,
  "dictionary/WILD_DOMESTIC_MANUAL_DICTIONARY.xlsx"
)

if (!file_exists(manual_dictionary_file)) {
  stop(
    "Manual host dictionary not found: ", manual_dictionary_file,
    "\nPlace WILD_DOMESTIC_MANUAL_DICTIONARY.xlsx in the project root directory."
  )
}

wildlife_dictionary <- readxl::read_excel(
  manual_dictionary_file,
  sheet = "wildlife_taxa_dict"
) |>
  transmute(
    wildlife_taxon_original = str_squish(as.character(wildlife_taxon_original)),
    wildlife_host_group = str_squish(as.character(wildlife_host_group))
  ) |>
  filter(
    !is.na(wildlife_taxon_original),
    nzchar(wildlife_taxon_original),
    !is.na(wildlife_host_group),
    nzchar(wildlife_host_group)
  )

domestic_dictionary <- readxl::read_excel(
  manual_dictionary_file,
  sheet = "domestic_taxa_dict"
) |>
  transmute(
    domestic_taxon_original = str_squish(as.character(domestic_taxon_original)),
    domestic_host_group = str_squish(as.character(domestic_host_group))
  ) |>
  filter(
    !is.na(domestic_taxon_original),
    nzchar(domestic_taxon_original),
    !is.na(domestic_host_group),
    nzchar(domestic_host_group)
  )

# Dictionary integrity: each original label must map to one and only one group.
wildlife_dictionary_conflicts <- wildlife_dictionary |>
  distinct(wildlife_taxon_original, wildlife_host_group) |>
  count(wildlife_taxon_original, name = "groups_n") |>
  filter(groups_n > 1)

if (nrow(wildlife_dictionary_conflicts) > 0) {
  stop("Wildlife dictionary contains original labels mapped to multiple host groups.")
}

domestic_dictionary_conflicts <- domestic_dictionary |>
  distinct(domestic_taxon_original, domestic_host_group) |>
  count(domestic_taxon_original, name = "groups_n") |>
  filter(groups_n > 1)

if (nrow(domestic_dictionary_conflicts) > 0) {
  stop("Domestic dictionary contains original labels mapped to multiple host groups.")
}

wildlife_dictionary <- wildlife_dictionary |>
  distinct(wildlife_taxon_original, .keep_all = TRUE)

domestic_dictionary <- domestic_dictionary |>
  distinct(domestic_taxon_original, .keep_all = TRUE)

# One row per paper x original extracted wildlife label, then attach the manual group.
wildlife_taxa_long <- papers |>
  select(article_id, wildlife_taxa) |>
  filter(!is.na(wildlife_taxa), nzchar(wildlife_taxa)) |>
  separate_rows(wildlife_taxa, sep = ";\\s*") |>
  transmute(
    article_id,
    wildlife_taxon_original = str_squish(wildlife_taxa)
  ) |>
  filter(nzchar(wildlife_taxon_original)) |>
  distinct(article_id, wildlife_taxon_original) |>
  left_join(wildlife_dictionary, by = "wildlife_taxon_original") |>
  mutate(
    wildlife_host_group = replace_na(wildlife_host_group, "NEEDS_REVIEW")
  )

# One row per paper x original extracted domestic label, then attach the manual group.
domestic_taxa_long <- papers |>
  select(article_id, domestic_taxa) |>
  filter(!is.na(domestic_taxa), nzchar(domestic_taxa)) |>
  separate_rows(domestic_taxa, sep = ";\\s*") |>
  transmute(
    article_id,
    domestic_taxon_original = str_squish(domestic_taxa)
  ) |>
  filter(nzchar(domestic_taxon_original)) |>
  distinct(article_id, domestic_taxon_original) |>
  left_join(domestic_dictionary, by = "domestic_taxon_original") |>
  mutate(
    domestic_host_group = replace_na(domestic_host_group, "NEEDS_REVIEW")
  )

# Explicit QC tables. Unmatched labels are never guessed.
wildlife_dictionary_unmatched <- wildlife_taxa_long |>
  filter(wildlife_host_group == "NEEDS_REVIEW") |>
  distinct(wildlife_taxon_original) |>
  arrange(wildlife_taxon_original)

domestic_dictionary_unmatched <- domestic_taxa_long |>
  filter(domestic_host_group == "NEEDS_REVIEW") |>
  distinct(domestic_taxon_original) |>
  arrange(domestic_taxon_original)

if (nrow(wildlife_dictionary_unmatched) > 0) {
  warning(
    nrow(wildlife_dictionary_unmatched),
    " wildlife labels were not found in the manual dictionary; see workbook QC sheet."
  )
}

if (nrow(domestic_dictionary_unmatched) > 0) {
  warning(
    nrow(domestic_dictionary_unmatched),
    " domestic labels were not found in the manual dictionary; see workbook QC sheet."
  )
}

# Domestic dictionary contains one deliberate exclusion (currently Diprotodontia).
# Preserve it in the dictionary and long table, but exclude it from host-group summaries.
domestic_excluded_records <- domestic_taxa_long |>
  filter(str_starts(domestic_host_group, "REMOVE")) |>
  arrange(article_id, domestic_taxon_original)

# Original-label frequencies remain useful for auditability.
wildlife_taxa_counts <- wildlife_taxa_long |>
  distinct(article_id, wildlife_taxon_original) |>
  count(wildlife_taxon_original, sort = TRUE, name = "papers_n")

domestic_taxa_counts <- domestic_taxa_long |>
  filter(!str_starts(domestic_host_group, "REMOVE")) |>
  distinct(article_id, domestic_taxon_original) |>
  count(domestic_taxon_original, sort = TRUE, name = "papers_n")

# Manuscript host-group summaries: each paper contributes at most once to a group.
# Denominator is the full review corpus, so percentages across groups need not sum to 100%.
wildlife_host_group_counts <- wildlife_taxa_long |>
  distinct(article_id, wildlife_host_group) |>
  count(wildlife_host_group, sort = TRUE, name = "papers_n") |>
  mutate(
    percent_papers = round(100 * papers_n / n_distinct(papers$article_id), 1)
  )

domestic_host_group_counts <- domestic_taxa_long |>
  filter(!str_starts(domestic_host_group, "REMOVE")) |>
  distinct(article_id, domestic_host_group) |>
  count(domestic_host_group, sort = TRUE, name = "papers_n") |>
  mutate(
    percent_papers = round(100 * papers_n / n_distinct(papers$article_id), 1)
  )

# Optional manuscript-ready combined host-group table.
host_group_summary <- bind_rows(
  wildlife_host_group_counts |>
    transmute(
      host_domain = "Wildlife",
      host_group = wildlife_host_group,
      papers_n,
      percent_papers
    ),
  domestic_host_group_counts |>
    transmute(
      host_domain = "Domestic",
      host_group = domestic_host_group,
      papers_n,
      percent_papers
    )
) |>
  arrange(host_domain, desc(papers_n), host_group)

wildlife_species_counts <- split_semicolon_counts(
  papers, "wildlife_species", "wildlife_species"
)

domestic_species_counts <- split_semicolon_counts(
  papers, "domestic_species", "domestic_species"
)

# ------------------------------------------------------------
# Disease/infection-system harmonisation using the frozen manual dictionary
# ------------------------------------------------------------
# The manual disease dictionary is keyed to the `disease` field in the
# pipeline's disease_long output. Do NOT reconstruct disease terms by splitting
# papers$diseases_or_pathogens: semicolons can occur inside a single extracted
# disease description and would create artificial records.

disease_dictionary_file <- file.path(
  root_dir,
  "dictionary/DISEASE_MANUAL_DICTIONARY.xlsx"
)

if (!file_exists(disease_dictionary_file)) {
  stop(
    "Manual disease dictionary not found: ", disease_dictionary_file,
    "\nPlace DISEASE_MANUAL_DICTIONARY.xlsx in the dictionary directory."
  )
}

if (nrow(disease_long) == 0) {
  stop("Disease long table is empty or missing; cannot apply the manual disease dictionary.")
}

if (!"disease" %in% names(disease_long)) {
  stop("Expected column `disease` was not found in disease_long.")
}

disease_dictionary_raw <- readxl::read_excel(disease_dictionary_file, sheet = 1)

if (!all(c("disease_original", "disease_harmonised") %in% names(disease_dictionary_raw))) {
  stop(
    "Disease dictionary must contain columns `disease_original` and `disease_harmonised`. Found: ",
    paste(names(disease_dictionary_raw), collapse = ", ")
  )
}

disease_dictionary <- disease_dictionary_raw |>
  transmute(
    disease_original = str_squish(as.character(disease_original)),
    disease_harmonised = str_squish(as.character(disease_harmonised))
  ) |>
  filter(
    !is.na(disease_original), nzchar(disease_original),
    !is.na(disease_harmonised), nzchar(disease_harmonised)
  )

# Dictionary integrity: each original disease term must map to exactly one
# harmonised disease/infection system.
disease_dictionary_conflicts <- disease_dictionary |>
  distinct(disease_original, disease_harmonised) |>
  count(disease_original, name = "systems_n") |>
  filter(systems_n > 1)

if (nrow(disease_dictionary_conflicts) > 0) {
  stop(
    "Disease dictionary contains original terms mapped to multiple harmonised systems: ",
    paste(disease_dictionary_conflicts$disease_original, collapse = "; ")
  )
}

disease_dictionary <- disease_dictionary |>
  distinct(disease_original, .keep_all = TRUE)

# Join the dictionary directly to disease_long$disease. Only whitespace is
# normalised on both sides; no biological or lexical reclassification occurs.
disease_harmonised_long <- disease_long |>
  mutate(
    disease = str_squish(as.character(disease))
  ) |>
  filter(!is.na(disease), nzchar(disease)) |>
  left_join(
    disease_dictionary,
    by = c("disease" = "disease_original")
  )

# Complete dictionary coverage is required. Any unmatched term is a pipeline
# error to fix rather than an analytical NEEDS_REVIEW category.
unmatched_diseases <- disease_harmonised_long |>
  filter(is.na(disease_harmonised)) |>
  distinct(disease) |>
  arrange(disease)

if (nrow(unmatched_diseases) > 0) {
  stop(
    "Disease dictionary does not exactly cover disease_long$disease: ",
    nrow(unmatched_diseases),
    " unmatched original terms: ",
    paste(unmatched_diseases$disease, collapse = "; ")
  )
}

# Final manuscript/plot summary: each paper contributes at most once to each
# harmonised disease or infection system.
disease_counts <- disease_harmonised_long |>
  distinct(article_id, disease_harmonised) |>
  count(disease_harmonised, sort = TRUE, name = "papers_n") |>
  mutate(
    percent_papers = round(100 * papers_n / n_distinct(papers$article_id), 1)
  )

# Agent type and transmission mode remain attached to the same disease_long
# records and are summarised independently of disease-name harmonisation.
agent_type_counts <- disease_harmonised_long |>
  filter(!is.na(agent_type), nzchar(agent_type)) |>
  distinct(article_id, agent_type) |>
  count(agent_type, sort = TRUE, name = "papers_n")

transmission_counts <- disease_harmonised_long |>
  filter(!is.na(transmission_mode), nzchar(transmission_mode)) |>
  distinct(article_id, transmission_mode) |>
  count(transmission_mode, sort = TRUE, name = "papers_n")

# ------------------------------------------------------------
# Quantitative interface measures
# ------------------------------------------------------------
if (nrow(quantitative_long) > 0) {

  quantitative_measures_table <- quantitative_long |>
    left_join(
      papers |>
        select(
          article_id,
          Author,
          Title,
          Journal,
          DOI,
          study_type,
          animal_focus,
          host_orientation,
          interface_operationalisation
        ),
      by = "article_id"
    ) |>
    transmute(
      paper_id = article_id,
      author = Author,
      title = Title,
      journal = Journal,
      doi = DOI,
      study_type,
      animal_focus,
      host_orientation,
      interface_operationalisation,
      measure_name,
      what_it_represents,
      measurement_method,
      metric_or_unit,
      role_in_analysis,
      quantitative_result,
      evidence_quote,
      page,
      section,
      grounding_status
    ) |>
    arrange(
      desc(interface_operationalisation),
      measure_name
    )

  quantitative_measure_counts <- quantitative_long |>
    filter(!is.na(measure_name), nzchar(measure_name)) |>
    distinct(article_id, measure_name) |>
    count(measure_name, sort = TRUE, name = "papers_n")

  quantitative_role_counts <- quantitative_long |>
    filter(!is.na(role_in_analysis), nzchar(role_in_analysis)) |>
    mutate(
      role_in_analysis = str_to_lower(str_squish(role_in_analysis))
    ) |>
    distinct(article_id, role_in_analysis) |>
    count(role_in_analysis, sort = TRUE, name = "papers_n")

  quantitative_method_counts <- quantitative_long |>
    filter(
      !is.na(measurement_method),
      nzchar(measurement_method)
    ) |>
    mutate(
      measurement_method = str_squish(measurement_method)
    ) |>
    distinct(article_id, measurement_method) |>
    count(
      measurement_method,
      sort = TRUE,
      name = "papers_n"
    )

} else {
  quantitative_measures_table <- tibble()
  quantitative_measure_counts <- tibble()
  quantitative_role_counts <- tibble()
  quantitative_method_counts <- tibble()
}

# ------------------------------------------------------------
# Compact manuscript-level summary
# ------------------------------------------------------------
manuscript_summary <- tibble(
  characteristic = c(
    "Number of papers",
    "Wildlife focus",
    "Domestic focus",
    "Both wildlife and domestic"
  ),
  value = c(
    nrow(papers),
    sum(papers$animal_focus == "Wildlife", na.rm = TRUE),
    sum(papers$animal_focus == "Domestic", na.rm = TRUE),
    sum(papers$animal_focus == "Both", na.rm = TRUE)
  )
)

output_xlsx <- file.path(
  results_dir,
  paste0(final_prefix, "_manuscript_summary_tables.xlsx")
)

write.xlsx(
  list(
    manuscript_summary = manuscript_summary,
    paper_summary = paper_summary_table,
    study_type_counts = study_type_counts,
    animal_focus_counts = animal_focus_counts,
    disease_counts = disease_counts,
    disease_dictionary = disease_dictionary,
    disease_harmonised = disease_harmonised_long,
    agent_type_counts = agent_type_counts,
    transmission_counts = transmission_counts,
    quantitative_measures = quantitative_measures_table,
    quantitative_measure_counts = quantitative_measure_counts,
    quantitative_role_counts = quantitative_role_counts,
    quantitative_method_counts = quantitative_method_counts,
    host_group_summary = host_group_summary,
    wildlife_host_groups = wildlife_host_group_counts,
    domestic_host_groups = domestic_host_group_counts,
    wildlife_taxa_counts = wildlife_taxa_counts,
    domestic_taxa_counts = domestic_taxa_counts,
    wildlife_taxa_dict = wildlife_dictionary,
    domestic_taxa_dict = domestic_dictionary,
    wildlife_taxa_long = wildlife_taxa_long,
    domestic_taxa_long = domestic_taxa_long,
    wildlife_dict_unmatched = wildlife_dictionary_unmatched,
    domestic_dict_unmatched = domestic_dictionary_unmatched,
    domestic_excluded = domestic_excluded_records,
    wildlife_species_detail = wildlife_species_counts,
    domestic_species_detail = domestic_species_counts
  ),
  file = output_xlsx,
  overwrite = TRUE
)

message("Saved manuscript summary workbook: ", output_xlsx)
