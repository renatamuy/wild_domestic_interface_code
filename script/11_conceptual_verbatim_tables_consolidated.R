rm(list = ls())
gc()

suppressPackageStartupMessages({
  library(tidyverse)
  library(openxlsx)
  library(fs)
  library(here)
})

# ============================================================
# DERIVED OUTPUT 2
# CONCEPTUAL / VERBATIM INTERFACE TABLES
# ============================================================

setwd(here())
setwd("script")

root_dir <- normalizePath("..", winslash = "/", mustWork = TRUE)
results_dir <- file.path(root_dir, "results_synthesis_consolidated")

test_mode <- TRUE
final_prefix <- if (test_mode) "evidence_first_test" else "evidence_first_full"

conceptual_file <- file.path(
  results_dir,
  paste0(final_prefix, "_conceptual_evidence.csv")
)

if (!file_exists(conceptual_file)) {
  stop(
    "Conceptual evidence file not found. ",
    "Run the lean synthesis pipeline first."
  )
}

x <- read_csv(conceptual_file, show_col_types = FALSE) |>
  filter(
    !is.na(host_orientation),
    !is.na(interface_operationalisation)
  )

# These thresholds intentionally mirror Fig 5.
central_half_width <- 0.18
operational_threshold <- 0.25

# ------------------------------------------------------------
# Assign Fig 5 conceptual position
# ------------------------------------------------------------
x_ranked <- x |>
  mutate(
    synthesis_position = case_when(
      abs(host_orientation) <= central_half_width &
        interface_operationalisation >= 0.50 ~ "Bull's-eye / integrated centre",

      host_orientation < -central_half_width &
        interface_operationalisation >= operational_threshold ~
        "Operationalised wildlife-centred",

      host_orientation > central_half_width &
        interface_operationalisation >= operational_threshold ~
        "Operationalised domestic-centred",

      host_orientation < -central_half_width &
        interface_operationalisation < operational_threshold ~
        "Conceptual wildlife-centred",

      host_orientation > central_half_width &
        interface_operationalisation < operational_threshold ~
        "Conceptual domestic-centred",

      abs(host_orientation) <= central_half_width ~
        "Balanced host focus, lower interface centrality",

      TRUE ~ "Other"
    ),

    # Archetype coordinates for reproducible quadrant ranking.
    target_x = case_when(
      synthesis_position %in% c(
        "Operationalised wildlife-centred",
        "Conceptual wildlife-centred"
      ) ~ -1,
      synthesis_position %in% c(
        "Operationalised domestic-centred",
        "Conceptual domestic-centred"
      ) ~ 1,
      TRUE ~ 0
    ),

    target_y = case_when(
      synthesis_position %in% c(
        "Operationalised wildlife-centred",
        "Operationalised domestic-centred",
        "Bull's-eye / integrated centre"
      ) ~ 1,
      synthesis_position %in% c(
        "Conceptual wildlife-centred",
        "Conceptual domestic-centred"
      ) ~ -1,
      TRUE ~ interface_operationalisation
    ),

    archetype_distance = sqrt(
      (host_orientation - target_x)^2 +
        (interface_operationalisation - target_y)^2
    )
  )

# ------------------------------------------------------------
# Central papers
# ------------------------------------------------------------
central_papers <- x_ranked |>
  filter(synthesis_position == "Bull's-eye / integrated centre") |>
  arrange(
    desc(interface_operationalisation),
    abs(host_orientation),
    desc(interface_explicit)
  ) |>
  transmute(
    rank = row_number(),
    paper_id,
    title,
    doi,
    host_orientation,
    interface_operationalisation,
    interface_explicit,
    interface_term_used,
    interface_statement_kind,
    verbatim_interface_statement = interface_statement_verbatim,
    page = interface_statement_page,
    section = interface_statement_section,
    grounding = interface_statement_grounding,
    pdf_text_status
  )

# ------------------------------------------------------------
# Top papers in each of the four outer quadrants
# ------------------------------------------------------------
quadrant_names <- c(
  "Operationalised wildlife-centred",
  "Operationalised domestic-centred",
  "Conceptual wildlife-centred",
  "Conceptual domestic-centred"
)

quadrant_exemplars <- x_ranked |>
  filter(synthesis_position %in% quadrant_names) |>
  group_by(synthesis_position) |>
  arrange(archetype_distance, .by_group = TRUE) |>
  mutate(rank_within_quadrant = row_number()) |>
  filter(rank_within_quadrant <= 5) |>
  ungroup() |>
  select(
    synthesis_position,
    rank_within_quadrant,
    paper_id,
    title,
    doi,
    host_orientation,
    interface_operationalisation,
    interface_explicit,
    interface_term_used,
    interface_statement_kind,
    interface_statement_verbatim,
    interface_statement_page,
    interface_statement_section,
    interface_statement_grounding,
    archetype_distance,
    pdf_text_status
  )

# ------------------------------------------------------------
# Interface terminology frequencies
# ------------------------------------------------------------
interface_terms <- x_ranked |>
  filter(
    interface_explicit %in% TRUE,
    !is.na(interface_term_used),
    nzchar(interface_term_used)
  ) |>
  mutate(interface_term_used = str_squish(interface_term_used)) |>
  count(interface_term_used, sort = TRUE, name = "papers_n")

# ------------------------------------------------------------
# All conceptual evidence, for human inspection
# ------------------------------------------------------------
all_conceptual_evidence <- x_ranked |>
  arrange(
    desc(interface_operationalisation),
    abs(host_orientation)
  ) |>
  select(
    paper_id,
    title,
    doi,
    synthesis_position,
    host_orientation,
    host_orientation_label,
    interface_operationalisation,
    interface_label,
    host_focus_group,
    interface_explicit,
    interface_term_used,
    interface_statement_kind,
    interface_statement_verbatim,
    interface_statement_page,
    interface_statement_section,
    interface_statement_grounding,
    pdf_text_status
  )

output_xlsx <- file.path(
  results_dir,
  paste0(final_prefix, "_conceptual_verbatim_tables.xlsx")
)

write.xlsx(
  list(
    central_papers = central_papers,
    quadrant_exemplars = quadrant_exemplars,
    interface_terms = interface_terms,
    all_conceptual_evidence = all_conceptual_evidence
  ),
  file = output_xlsx,
  overwrite = TRUE
)

message("Saved conceptual/verbatim workbook: ", output_xlsx)
