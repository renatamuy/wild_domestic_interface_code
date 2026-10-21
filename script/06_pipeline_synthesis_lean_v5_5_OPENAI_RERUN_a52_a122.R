rm(list = ls())
gc()

# ============================================================
# TARGETED QC RERUN COPY — a52 + a122
# Derived from pipeline_synthesis_lean_v5_5_OPENAI_WEEKEND_SAFE_FROZEN.R
# Scientific extraction/scoring logic is unchanged.
# Changes are limited to input/output isolation, safety guards, and removal
# of the embedded API credential. Original production outputs are immutable.
# ============================================================

suppressPackageStartupMessages({
  library(pdftools)
  library(httr2)
  library(jsonlite)
  library(openxlsx)
  library(stringr)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(fs)
  library(here)
})

# ============================================================
# LEAN SYNTHESIS PIPELINE v5.5 — ROBUST INTERFACE.3 — OPENAI API
# Purpose:
#   1) generate Fig 5 coordinates
#   2) generate manuscript summary data
#   3) generate conceptual/verbatim interface evidence
#   4) retain audit + benchmark diagnostics
#
# Deliberately NOT extracted/scored:
#   - applied contribution
#   - host comprehensiveness
# ============================================================

# ============================================================
# PATHS
# ============================================================
setwd(here())
setwd("script")

script_dir <- getwd()
if (basename(script_dir) != "script") {
  stop("Run this script from the project script directory.")
}

root_dir    <- normalizePath("..", winslash = "/", mustWork = TRUE)
# TARGETED QC RERUN: replacement source PDFs for a52 and a122 only.
# The original production directories remain immutable.
pdf_dir     <- file.path(root_dir, "newdata_a52_a122")
results_dir <- file.path(root_dir, "results_synthesis_rerun_a52_a122")
original_results_dir <- file.path(root_dir, "results_synthesis")
master_table_file <- file.path(root_dir, "master/master_table.xlsx")
# Refuse any accidental write into the original production-results directory.
if (normalizePath(results_dir, winslash = "/", mustWork = FALSE) ==
    normalizePath(original_results_dir, winslash = "/", mustWork = TRUE)) {
  stop("SAFETY STOP: targeted QC rerun cannot write to results_synthesis/.")
}

if (!dir_exists(pdf_dir)) {
  stop("Targeted rerun input directory not found: ", pdf_dir)
}

dir_create(results_dir)

# ============================================================
# AUTHORITATIVE BIBLIOGRAPHIC METADATA
# ============================================================
# Put master_table.xlsx in the project root.
# The Records sheet is the sole source of truth for bibliographic metadata.
if (!file_exists(master_table_file)) {
  stop(
    "master_table.xlsx not found at: ", master_table_file,
    "\nPlace master_table.xlsx in the project root before running."
  )
}

master_records <- openxlsx::read.xlsx(
  master_table_file,
  sheet = "Records",
  check.names = FALSE
) |>
  as_tibble() |>
  mutate(paper_id = as.character(paper_id))

required_master_columns <- c(
  "ID", "paper_id", "Author", "Title", "Journal", "DOI"
)

missing_master_columns <- setdiff(
  required_master_columns,
  names(master_records)
)

if (length(missing_master_columns) > 0) {
  stop(
    "master_table.xlsx Records sheet is missing required columns: ",
    paste(missing_master_columns, collapse = ", ")
  )
}

if (any(is.na(master_records$paper_id) | !nzchar(master_records$paper_id))) {
  stop("master_table.xlsx contains missing/blank paper_id values.")
}

duplicate_master_ids <- master_records |>
  count(paper_id, name = "n") |>
  filter(n > 1)

if (nrow(duplicate_master_ids) > 0) {
  stop(
    "paper_id must be unique in master_table.xlsx. Duplicate IDs: ",
    paste(duplicate_master_ids$paper_id, collapse = ", ")
  )
}

message(
  "Loaded authoritative metadata for ",
  nrow(master_records),
  " papers from master_table.xlsx."
)

# ============================================================
# USER SETTINGS
# ============================================================
model_name <- "gpt-5.6-terra"
reasoning_effort <- "low"

# Current standard text-token prices for gpt-5.6-terra.
# Re-check before the final 270-paper run if substantial time has passed.
api_input_usd_per_million_tokens <- 2.00
api_output_usd_per_million_tokens <- 12.00

# SECURITY / REPRODUCIBILITY:
# Do not store API credentials in this script. Set OPENAI_API_KEY in the
# local R environment (for example via .Renviron) before running.

Sys.setenv(OPENAI_API_KEY = "")


openai_api_key <- Sys.getenv("OPENAI_API_KEY")
if (!nzchar(openai_api_key)) {
  stop(
    "OPENAI_API_KEY is not set. ",
    "Set it in your R environment before running this script."
  )
}

test_mode        <- TRUE
max_pdfs_test    <- 270
resume           <- TRUE
checkpoint_every <- 1  # frozen safety setting: save after every completed paper

target_full_corpus_n <- 270
chars_per_token_fallback <- 4

final_prefix <- if (test_mode) "evidence_first_test" else "evidence_first_full"

# ============================================================
# HELPERS
# ============================================================
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (length(x) == 1 && is.na(x)) return(y)
  x
}

safe_trim <- function(x) {
  if (length(x) == 0 || is.null(x)) return("")
  x <- ifelse(is.na(x), "", x)
  stringr::str_squish(x)
}

scalar_or_na <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  z <- as.character(x[1])
  if (!nzchar(z)) NA_character_ else z
}

as_logical_or_na <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA)
  if (is.logical(x)) return(x[1])
  z <- tolower(as.character(x[1]))
  if (z %in% c("true", "yes", "1")) return(TRUE)
  if (z %in% c("false", "no", "0")) return(FALSE)
  NA
}

as_number_or_na <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  suppressWarnings(as.numeric(x[1]))
}

collapse_array <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  vals <- unique(safe_trim(unlist(x)))
  vals <- vals[nzchar(vals)]
  if (length(vals) == 0) NA_character_ else paste(vals, collapse = "; ")
}

make_article_id <- function(path) {
  paste0("a", tools::file_path_sans_ext(basename(path)))
}

extract_pdf_text_safe <- function(pdf_file) {
  tryCatch(pdftools::pdf_text(pdf_file), error = function(e) character())
}

# ============================================================
# PDF TEXT QUALITY
# ============================================================
pdf_quality_metrics <- function(article_id, file_name, pages) {
  if (length(pages) == 0) {
    return(tibble(
      article_id = article_id,
      file_name = file_name,
      n_pages = 0L,
      total_chars = 0,
      median_chars_per_page = 0,
      empty_pages = 0L,
      empty_page_prop = 1,
      pdf_text_status = "UNREADABLE"
    ))
  }

  chars <- nchar(pages, type = "chars")
  empty <- chars < 50

  total_chars <- sum(chars, na.rm = TRUE)
  median_chars <- median(chars, na.rm = TRUE)
  empty_prop <- mean(empty)

  status <- case_when(
    total_chars < 1500 ~ "POSSIBLE_SCAN",
    empty_prop > 0.50 ~ "POSSIBLE_SCAN",
    median_chars < 500 ~ "SPARSE_TEXT",
    TRUE ~ "OK"
  )

  tibble(
    article_id = article_id,
    file_name = file_name,
    n_pages = length(pages),
    total_chars = total_chars,
    median_chars_per_page = median_chars,
    empty_pages = sum(empty),
    empty_page_prop = round(empty_prop, 3),
    pdf_text_status = status
  )
}

# ============================================================
# PAGE / SECTION PREPARATION
# ============================================================
detect_probable_section <- function(page_text) {
  txt <- str_to_lower(substr(page_text, 1, 2500))

  case_when(
    str_detect(txt, "\\babstract\\b") ~ "abstract",
    str_detect(txt, "\\b(keywords|key words)\\b") ~ "keywords",
    str_detect(txt, "\\b(introduction|background)\\b") ~ "introduction",
    str_detect(txt, "\\b(materials and methods|methods|methodology|study design)\\b") ~ "methods",
    str_detect(txt, "\\bresults\\b") ~ "results",
    str_detect(txt, "\\b(discussion|interpretation)\\b") ~ "discussion",
    str_detect(txt, "\\b(conclusion|conclusions|implications)\\b") ~ "conclusion",
    str_detect(txt, "\\breferences\\b") ~ "references",
    TRUE ~ "unknown"
  )
}

make_page_records <- function(pages) {
  tibble(
    page = seq_along(pages),
    text = pages,
    probable_section = map_chr(pages, detect_probable_section)
  ) |>
    mutate(
      text = str_replace_all(text, "\r", "\n"),
      text = str_replace_all(text, "\t", " "),
      text = str_replace_all(text, "[ ]{2,}", " ")
    )
}

format_pages <- function(page_tbl, max_chars = 52000) {
  if (nrow(page_tbl) == 0) return("")

  txt <- pmap_chr(
    page_tbl,
    function(page, text, probable_section) {
      paste0(
        "\n--- PAGE ", page,
        " | PROBABLE SECTION: ", probable_section,
        " ---\n", text
      )
    }
  ) |>
    paste(collapse = "\n")

  substr(txt, 1, max_chars)
}

select_module_context <- function(page_tbl, module) {
  target_sections <- switch(
    module,
    metadata = c("abstract", "introduction", "methods", "results", "unknown"),
    interface = c(
      "abstract", "introduction", "methods", "results",
      "discussion", "conclusion", "unknown"
    ),
    hosts = c("abstract", "methods", "results", "discussion", "unknown"),
    page_tbl$probable_section
  )

  selected <- page_tbl |>
    filter(probable_section %in% target_sections)

  if (nrow(selected) == 0) selected <- page_tbl
  format_pages(selected)
}

# ============================================================
# GROUNDING NORMALISATION
# ============================================================
normalise_grounding_text <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("")

  x |>
    str_replace_all("([[:alpha:]])-\\s*\\n\\s*([[:alpha:]])", "\\1\\2") |>
    str_replace_all("[\u2012\u2013\u2014\u2212]", "-") |>
    str_replace_all("[\u2018\u2019]", "'") |>
    str_replace_all("[\u201C\u201D]", "\"") |>
    str_replace_all("\\s+", " ") |>
    str_squish() |>
    str_to_lower()
}

grounding_status <- function(quote, page, page_tbl) {
  q <- scalar_or_na(quote)
  p <- suppressWarnings(as.integer(page %||% NA_integer_))

  if (is.na(q) || !nzchar(q)) return("MISSING_QUOTE")
  if (is.na(p) || !p %in% page_tbl$page) return("INVALID_PAGE")

  page_text <- page_tbl$text[match(p, page_tbl$page)]

  q_exact <- str_squish(q)
  page_exact <- str_squish(page_text)

  if (grepl(q_exact, page_exact, fixed = TRUE)) return("EXACT")

  q_norm <- normalise_grounding_text(q)
  page_norm <- normalise_grounding_text(page_text)

  if (nzchar(q_norm) && grepl(q_norm, page_norm, fixed = TRUE)) {
    return("NORMALISED_EXACT")
  }

  "NOT_FOUND"
}

# ============================================================
# OPENAI STRUCTURED OUTPUTS
# ============================================================
nullable_string <- function() list(type = c("string", "null"))
nullable_boolean <- function() list(type = c("boolean", "null"))
nullable_integer <- function() list(type = c("integer", "null"))

string_array_schema <- function() {
  list(
    type = "array",
    items = list(type = "string")
  )
}

metadata_schema <- list(
  type = "object",
  additionalProperties = FALSE,
  properties = list(
    study_type = nullable_string(),
    study_design = nullable_string(),
    countries_investigated = string_array_schema(),
    wildlife_taxa = string_array_schema(),
    domestic_taxa = string_array_schema(),
    wildlife_species = string_array_schema(),
    domestic_species = string_array_schema(),
    diseases = list(
      type = "array",
      items = list(
        type = "object",
        additionalProperties = FALSE,
        properties = list(
          name = nullable_string(),
          agent_type = nullable_string(),
          transmission_modes = string_array_schema()
        ),
        required = c(
          "name",
          "agent_type",
          "transmission_modes"
        )
      )
    )
  ),
  required = c(
    "study_type",
    "study_design",
    "countries_investigated",
    "wildlife_taxa",
    "domestic_taxa",
    "wildlife_species",
    "domestic_species",
    "diseases"
  )
)

evidence_item_schema <- list(
  type = "object",
  additionalProperties = FALSE,
  properties = list(
    field = list(type = "string"),
    quote = nullable_string(),
    page = nullable_integer(),
    section = nullable_string()
  ),
  required = c("field", "quote", "page", "section")
)

interface_schema <- list(
  type = "object",
  additionalProperties = FALSE,
  properties = list(
    interface_mentioned = nullable_boolean(),
    interface_mentioned_evidence_quote = nullable_string(),
    interface_mentioned_evidence_page = nullable_integer(),
    interface_mentioned_evidence_section = nullable_string(),
    interface_in_objectives = nullable_boolean(),
    interface_in_objectives_evidence_quote = nullable_string(),
    interface_in_objectives_evidence_page = nullable_integer(),
    interface_in_objectives_evidence_section = nullable_string(),
    conceptual_framing = nullable_boolean(),
    conceptual_framing_evidence_quote = nullable_string(),
    conceptual_framing_evidence_page = nullable_integer(),
    conceptual_framing_evidence_section = nullable_string(),
    qualitative_representation = nullable_boolean(),
    qualitative_representation_evidence_quote = nullable_string(),
    qualitative_representation_evidence_page = nullable_integer(),
    qualitative_representation_evidence_section = nullable_string(),
    quantitative_variable_present = nullable_boolean(),
    quantitative_variable_present_evidence_quote = nullable_string(),
    quantitative_variable_present_evidence_page = nullable_integer(),
    quantitative_variable_present_evidence_section = nullable_string(),
    variable_used_in_analysis = nullable_boolean(),
    variable_used_in_analysis_evidence_quote = nullable_string(),
    variable_used_in_analysis_evidence_page = nullable_integer(),
    variable_used_in_analysis_evidence_section = nullable_string(),
    interface_specific_question_addressed = nullable_boolean(),
    interface_specific_question_addressed_evidence_quote = nullable_string(),
    interface_specific_question_addressed_evidence_page = nullable_integer(),
    interface_specific_question_addressed_evidence_section = nullable_string(),
    interface_explicit = nullable_boolean(),
    interface_explicit_evidence_quote = nullable_string(),
    interface_explicit_evidence_page = nullable_integer(),
    interface_explicit_evidence_section = nullable_string(),
    interface_variable_names = string_array_schema(),
    interface_term_used = nullable_string(),
    interface_statement_kind = nullable_string(),
    interface_statement_verbatim = nullable_string(),
    interface_statement_page = nullable_integer(),
    interface_statement_section = nullable_string(),
    quantitative_measures = list(
      type = "array",
      items = list(
        type = "object",
        additionalProperties = FALSE,
        properties = list(
          measure_name = nullable_string(),
          what_it_represents = nullable_string(),
          measurement_method = nullable_string(),
          metric_or_unit = nullable_string(),
          role_in_analysis = nullable_string(),
          quantitative_result = nullable_string(),
          evidence_quote = nullable_string(),
          page = nullable_integer(),
          section = nullable_string()
        ),
        required = c(
          "measure_name",
          "what_it_represents",
          "measurement_method",
          "metric_or_unit",
          "role_in_analysis",
          "quantitative_result",
          "evidence_quote",
          "page",
          "section"
        )
      )
    )
  ),
  required = c(
    "interface_mentioned",
    "interface_mentioned_evidence_quote",
    "interface_mentioned_evidence_page",
    "interface_mentioned_evidence_section",
    "interface_in_objectives",
    "interface_in_objectives_evidence_quote",
    "interface_in_objectives_evidence_page",
    "interface_in_objectives_evidence_section",
    "conceptual_framing",
    "conceptual_framing_evidence_quote",
    "conceptual_framing_evidence_page",
    "conceptual_framing_evidence_section",
    "qualitative_representation",
    "qualitative_representation_evidence_quote",
    "qualitative_representation_evidence_page",
    "qualitative_representation_evidence_section",
    "quantitative_variable_present",
    "quantitative_variable_present_evidence_quote",
    "quantitative_variable_present_evidence_page",
    "quantitative_variable_present_evidence_section",
    "variable_used_in_analysis",
    "variable_used_in_analysis_evidence_quote",
    "variable_used_in_analysis_evidence_page",
    "variable_used_in_analysis_evidence_section",
    "interface_specific_question_addressed",
    "interface_specific_question_addressed_evidence_quote",
    "interface_specific_question_addressed_evidence_page",
    "interface_specific_question_addressed_evidence_section",
    "interface_explicit",
    "interface_explicit_evidence_quote",
    "interface_explicit_evidence_page",
    "interface_explicit_evidence_section",
    "interface_variable_names",
    "interface_term_used",
    "interface_statement_kind",
    "interface_statement_verbatim",
    "interface_statement_page",
    "interface_statement_section",
    "quantitative_measures"
  )
)

hosts_schema <- list(
  type = "object",
  additionalProperties = FALSE,
  properties = list(
    wildlife_role = nullable_string(),
    domestic_role = nullable_string(),
    wildlife_evidence_quote = nullable_string(),
    wildlife_evidence_page = nullable_integer(),
    wildlife_evidence_section = nullable_string(),
    domestic_evidence_quote = nullable_string(),
    domestic_evidence_page = nullable_integer(),
    domestic_evidence_section = nullable_string()
  ),
  required = c(
    "wildlife_role",
    "domestic_role",
    "wildlife_evidence_quote",
    "wildlife_evidence_page",
    "wildlife_evidence_section",
    "domestic_evidence_quote",
    "domestic_evidence_page",
    "domestic_evidence_section"
  )
)

module_schema <- function(module_name) {
  switch(
    module_name,
    metadata = metadata_schema,
    interface = interface_schema,
    hosts = hosts_schema,
    stop("Unknown module schema: ", module_name)
  )
}

ask_openai_json <- function(prompt, schema, schema_name) {
  start_time <- Sys.time()

  resp <- request("https://api.openai.com/v1/chat/completions") |>
    req_headers(
      Authorization = paste("Bearer", openai_api_key),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(
      list(
        model = model_name,
        messages = list(
          list(
            role = "user",
            content = prompt
          )
        ),
        reasoning_effort = reasoning_effort,
        store = FALSE,
        response_format = list(
          type = "json_schema",
          json_schema = list(
            name = schema_name,
            strict = TRUE,
            schema = schema
          )
        )
      ),
      auto_unbox = TRUE
    ) |>
    req_timeout(600) |>
    req_perform()

  duration <- as.numeric(
    difftime(Sys.time(), start_time, units = "secs")
  )

  body <- resp_body_json(resp, simplifyVector = FALSE)
  raw_text <- body$choices[[1]]$message$content %||% ""

  list(
    raw = raw_text,
    time_sec = duration,
    model = body$model %||% model_name,
    prompt_tokens = as_number_or_na(
      body$usage$prompt_tokens %||% NA_real_
    ),
    output_tokens = as_number_or_na(
      body$usage$completion_tokens %||% NA_real_
    ),
    response_chars = nchar(raw_text, type = "chars")
  )
}

parse_json_safe <- function(x) {
  tryCatch(
    jsonlite::fromJSON(x, simplifyVector = FALSE),
    error = function(e) list(
      parse_error = TRUE,
      error_message = conditionMessage(e),
      raw_response = x
    )
  )
}

# ============================================================
# PROMPTS
# ============================================================
common_rules <- paste(
  "You are extracting auditable information from a scientific paper.",
  "Use only the supplied paper text.",
  "Do not infer facts that are not supported by the paper.",
  "Distinguish what this paper itself did from what cited studies did.",
  "For every quotation: copy the wording exactly from the supplied text and provide its page.",
  "Return valid JSON only.",
  sep = "\n"
)

build_metadata_prompt <- function(context_text) {
  paste(
    common_rules,
    "",
    "TASK: Extract ONLY descriptive fields needed for the manuscript summary.",
    "If a field is not supported, return null or an empty array.",
    "study_type must be one of:",
    "original_empirical, review, systematic_review, meta_analysis,",
    "modelling_secondary_data, surveillance_report, conceptual_commentary,",
    "protocol, other, unclear.",
    "",
    "For diseases, return one object per disease/pathogen evaluated by THIS paper.",
    "agent_type examples: viral, bacterial, parasitic, fungal, prion, other, unclear.",
    "transmission_modes should contain only modes supported by the paper;",
    "examples include vector-borne, direct contact, environmental, aerosol,",
    "faecal-oral, foodborne, waterborne, vertical, indirect contact, unclear.",
    "",
    "Return exactly this JSON structure:",
    "{",
    '  "study_type": null,',
    '  "study_design": null,',
    '  "countries_investigated": [],',
    '  "wildlife_taxa": [],',
    '  "domestic_taxa": [],',
    '  "wildlife_species": [],',
    '  "domestic_species": [],',
    '  "diseases": [',
    '    {',
    '      "name": null,',
    '      "agent_type": null,',
    '      "transmission_modes": []',
    '    }',
    '  ]',
    "}",
    "",
    "PAPER TEXT:",
    context_text,
    sep = "\n"
  )
}

build_interface_prompt <- function(context_text) {
  paste(
    common_rules,
    "",
    "TASK: Extract ONLY evidence needed to place the paper on the",
    "interface-centrality / operationalisation axis and to describe how",
    "THIS paper represents the wild-domestic relationship.",
    "",
    "CRITICAL PRINCIPLE:",
    "An interface can be operationalised EXPLICITLY or IMPLICITLY.",
    "The authors do NOT need to use the word 'interface' for a study to",
    "qualitatively represent, quantitatively represent, or operationalise it.",
    "",
    "Examples of IMPLICIT interface representation/operationalisation include:",
    "- shared versus non-shared sites used by wildlife and domestic animals;",
    "- mixed-host versus single-host ecological settings;",
    "- wildlife presence versus absence on livestock properties;",
    "- spatial overlap, proximity, contact rates, shared water/feed resources;",
    "- exposure categories based on wildlife-livestock contact;",
    "- comparisons of pathogen infection, prevalence, shedding or transmission",
    "  across wild and domestic hosts when the comparison represents their",
    "  ecological relationship or shared setting;",
    "- any equivalent measurable condition representing opportunities for",
    "  interaction between wild and domestic host domains.",
    "",
    "BOOLEAN RULE:",
    "Every Boolean must be true, false, or null.",
    "Use null ONLY when the supplied paper text is genuinely insufficient.",
    "A criterion being absent is FALSE, not null.",
    "",
    "Definitions:",
    "- interface_mentioned: a wild-domestic/wildlife-livestock relationship",
    "  is mentioned anywhere in relation to THIS paper.",
    "- interface_in_objectives: that relationship is part of aims/questions.",
    "- conceptual_framing: it structures rationale or interpretation but is",
    "  not translated into observable categories or measurements.",
    "- qualitative_representation: observable interface settings, contacts,",
    "  shared resources, behaviours or relationships are represented, but",
    "  not as a variable used in quantitative comparison/analysis.",
    "- quantitative_variable_present: at least one measurable interface",
    "  exposure, category or variable is reported. This includes categorical",
    "  operationalisations such as shared/non-shared or mixed/single-host",
    "  settings when they are used quantitatively; it need not be continuous.",
    "- variable_used_in_analysis: an interface exposure/category/variable is",
    "  used in comparison, association testing, modelling, effect estimation,",
    "  transmission analysis, or another quantitative analysis.",
    "- interface_specific_question_addressed: the analysis explicitly answers",
    "  a question about interaction, sharing, overlap, exposure, transmission,",
    "  infection patterns, or another relationship across wild/domestic domains.",
    "- interface_explicit: authors explicitly name an interface using terms",
    "  such as wildlife-livestock interface, wild-domestic interface,",
    "  livestock-wildlife interface, or a clearly equivalent named interface.",
    "",
    "EVIDENCE RULE:",
    "For EACH Boolean criterion, return its own best exact supporting quotation,",
    "page and section. Do not recycle all quotations into one criterion.",
    "For FALSE values, evidence may be null unless the paper contains a useful",
    "explicit negative statement. For TRUE values, provide evidence whenever",
    "the supplied text contains it.",
    "",
    "For interface_statement_verbatim, select ONE best exact quotation showing",
    "how THIS paper frames, represents or operationalises the relationship",
    "between wild and domestic animals. It may be explicit or implicit.",
    "",
    "QUANTITATIVE MEASURES:",
    "Only extract measures that directly characterise, compare, test, model, or",
    "otherwise operationalise the wild-domestic relationship.",
    "Do NOT collect unrelated numbers merely because the paper is quantitative.",
    "If quantitative_variable_present is false, quantitative_measures should be empty.",
    "For each quantitative interface measure capture measure name, meaning,",
    "measurement method, metric/unit, analytical role, one concise related result,",
    "and an exact supporting quotation/page/section.",
    "",
    "interface_statement_kind must be explicit, implicit, or none.",
    "",
    "Return exactly this JSON structure:",
    "{",
    '  "interface_mentioned": null,',
    '  "interface_mentioned_evidence_quote": null,',
    '  "interface_mentioned_evidence_page": null,',
    '  "interface_mentioned_evidence_section": null,',
    '  "interface_in_objectives": null,',
    '  "interface_in_objectives_evidence_quote": null,',
    '  "interface_in_objectives_evidence_page": null,',
    '  "interface_in_objectives_evidence_section": null,',
    '  "conceptual_framing": null,',
    '  "conceptual_framing_evidence_quote": null,',
    '  "conceptual_framing_evidence_page": null,',
    '  "conceptual_framing_evidence_section": null,',
    '  "qualitative_representation": null,',
    '  "qualitative_representation_evidence_quote": null,',
    '  "qualitative_representation_evidence_page": null,',
    '  "qualitative_representation_evidence_section": null,',
    '  "quantitative_variable_present": null,',
    '  "quantitative_variable_present_evidence_quote": null,',
    '  "quantitative_variable_present_evidence_page": null,',
    '  "quantitative_variable_present_evidence_section": null,',
    '  "variable_used_in_analysis": null,',
    '  "variable_used_in_analysis_evidence_quote": null,',
    '  "variable_used_in_analysis_evidence_page": null,',
    '  "variable_used_in_analysis_evidence_section": null,',
    '  "interface_specific_question_addressed": null,',
    '  "interface_specific_question_addressed_evidence_quote": null,',
    '  "interface_specific_question_addressed_evidence_page": null,',
    '  "interface_specific_question_addressed_evidence_section": null,',
    '  "interface_explicit": null,',
    '  "interface_explicit_evidence_quote": null,',
    '  "interface_explicit_evidence_page": null,',
    '  "interface_explicit_evidence_section": null,',
    '  "interface_variable_names": [],',
    '  "interface_term_used": null,',
    '  "interface_statement_kind": null,',
    '  "interface_statement_verbatim": null,',
    '  "interface_statement_page": null,',
    '  "interface_statement_section": null,',
    '  "quantitative_measures": [',
    '    {',
    '      "measure_name": null,',
    '      "what_it_represents": null,',
    '      "measurement_method": null,',
    '      "metric_or_unit": null,',
    '      "role_in_analysis": null,',
    '      "quantitative_result": null,',
    '      "evidence_quote": null,',
    '      "page": null,',
    '      "section": null',
    '    }',
    '  ]',
    "}",
    "",
    "FINAL CONSISTENCY CHECK BEFORE RETURNING JSON:",
    "1. If shared/non-shared, mixed/single-host, overlap, proximity, contact,",
    "   wildlife presence or another interface exposure/category is quantified,",
    "   quantitative_variable_present should normally be true.",
    "2. If such an interface variable/category is used in a statistical comparison",
    "   or model, variable_used_in_analysis should normally be true.",
    "3. Do not require explicit use of the word 'interface' for criteria 2-7.",
    "",
    "PAPER TEXT:",
    context_text,
    sep = "\n"
  )
}

build_hosts_prompt <- function(context_text) {
  paste(
    common_rules,
    "",
    "TASK: Determine the relative role of wildlife and domestic animals",
    "in THIS paper so that host-domain orientation can be assigned.",
    "",
    "For each host domain, choose exactly one role:",
    "ABSENT, CONTEXTUAL, SECONDARY, PRIMARY, or UNCLEAR.",
    "",
    "ROLE DEFINITIONS:",
    "- ABSENT: the host domain is not substantively present in the paper.",
    "- CONTEXTUAL: mentioned mainly as background, a potential source or",
    "  recipient of spillover, or contextual factor; it is not substantively",
    "  investigated, synthesised, or analysed.",
    "- SECONDARY: genuinely included in the study/synthesis/analysis, but",
    "  clearly subordinate to the other host domain.",
    "- PRIMARY: a central substantive focus of the objectives, empirical",
    "  evidence, analysis, synthesis, or interpretation.",
    "- UNCLEAR: the supplied text is genuinely insufficient or contradictory.",
    "",
    "IMPORTANT:",
    "- For reviews, modelling studies, and conceptual papers, PRIMARY does",
    "  NOT require direct physical sampling. It means a substantive central",
    "  focus of the paper's synthesis or analysis.",
    "- Do not classify a host domain as substantive merely because species",
    "  names occur in background text.",
    "- The two roles should describe RELATIVE HOST-DOMAIN EMPHASIS.",
    "",
    "Provide one best exact quotation and page supporting each role whenever possible.",
    "",
    "Return exactly this JSON structure:",
    "{",
    '  "wildlife_role": null,',
    '  "domestic_role": null,',
    '  "wildlife_evidence_quote": null,',
    '  "wildlife_evidence_page": null,',
    '  "wildlife_evidence_section": null,',
    '  "domestic_evidence_quote": null,',
    '  "domestic_evidence_page": null,',
    '  "domestic_evidence_section": null',
    "}",
    "",
    "PAPER TEXT:",
    context_text,
    sep = "\n"
  )
}

# ============================================================
# HUMAN-FRIENDLY AUDIT LABELS
# ============================================================
audit_question_lookup <- tribble(
  ~construct, ~variable, ~audit_question, ~audit_definition,

  "interface", "interface_mentioned",
  "Is a wild–domestic / wildlife–livestock interface or relationship mentioned in relation to this paper?",
  "TRUE when the paper itself discusses a relationship or interface between wild and domestic animals; FALSE when it does not; UNCLEAR when the supplied text is insufficient.",

  "interface", "interface_in_objectives",
  "Is the wild–domestic interface or relationship part of the paper's stated aims, objectives, or research questions?",
  "TRUE when the paper explicitly aims to investigate the interface/relationship; FALSE when it is not part of the stated aims; UNCLEAR when objectives cannot be determined.",

  "interface", "conceptual_framing",
  "Does the wild–domestic interface concept frame the rationale or interpretation without being represented through observable categories or measurements?",
  "TRUE when the interface is used conceptually but not operationalised; FALSE when this does not apply; UNCLEAR when the distinction cannot be made.",

  "interface", "qualitative_representation",
  "Does the paper describe observable interface settings, contacts, shared resources, behaviours, or relationships without quantifying them?",
  "TRUE for qualitative representation of the interface; FALSE when absent; UNCLEAR when the text is insufficient.",

  "interface", "quantitative_variable_present",
  "Does the study quantify a measurable feature of the wild–domestic interface?",
  "TRUE when at least one measurable interface-related variable is reported; FALSE when none is reported; UNCLEAR when the text is insufficient.",

  "interface", "variable_used_in_analysis",
  "Is an interface-related variable used in an analysis, comparison, model, association test, effect estimate, or transmission analysis?",
  "TRUE when an interface variable is analytically used; FALSE when it is only described or mentioned; UNCLEAR when analytical use cannot be determined.",

  "interface", "interface_specific_question_addressed",
  "Does the analysis explicitly address a research question about the wild–domestic interface or relationship?",
  "TRUE when the analytical question itself concerns the interface; FALSE when it does not; UNCLEAR when this cannot be determined.",

  "interface", "interface_explicit",
  "Do the authors explicitly name a wild–domestic / wildlife–livestock interface or clearly equivalent interface?",
  "TRUE when interface terminology is explicitly used; FALSE when the relationship is only implicit; UNCLEAR when wording cannot be determined.",

  "hosts", "wildlife_role",
  "What role do wildlife play in this study?",
  "Choose ABSENT, CONTEXTUAL, SECONDARY, PRIMARY, or UNCLEAR. PRIMARY = a central substantive focus of the study, analysis, synthesis, or interpretation. SECONDARY = genuinely included but clearly subordinate to the other host domain. CONTEXTUAL = mentioned mainly as background, a potential source/recipient, or context. ABSENT = not substantively present.",

  "hosts", "domestic_role",
  "What role do domestic animals play in this study?",
  "Choose ABSENT, CONTEXTUAL, SECONDARY, PRIMARY, or UNCLEAR. PRIMARY = a central substantive focus of the study, analysis, synthesis, or interpretation. SECONDARY = genuinely included but clearly subordinate to the other host domain. CONTEXTUAL = mentioned mainly as background, a potential source/recipient, or context. ABSENT = not substantively present."
)

# ============================================================
# REQUIRED BOOLEAN FIELDS
# ============================================================
required_boolean_fields <- list(
  interface = c(
    "interface_mentioned",
    "interface_in_objectives",
    "conceptual_framing",
    "qualitative_representation",
    "quantitative_variable_present",
    "variable_used_in_analysis",
    "interface_specific_question_addressed",
    "interface_explicit"
  )
)

host_role_fields <- c(
  "wildlife_role",
  "domestic_role"
)

module_completion_stats <- function(module_name, module_obj) {
  fields <- required_boolean_fields[[module_name]]
  vals <- map(fields, ~ as_logical_or_na(module_obj[[.x]]))
  completed <- sum(!map_lgl(vals, is.na))

  tibble(
    module = module_name,
    boolean_fields_expected = length(fields),
    boolean_fields_completed = completed,
    boolean_fields_null = length(fields) - completed,
    boolean_completion_rate = round(completed / length(fields), 3)
  )
}

# ============================================================
# DETERMINISTIC SCORING
# ============================================================
score_interface <- function(x) {
  # These seven criteria determine the ordered interface-centrality score.
  scoring_fields <- c(
    "interface_mentioned",
    "interface_in_objectives",
    "conceptual_framing",
    "qualitative_representation",
    "quantitative_variable_present",
    "variable_used_in_analysis",
    "interface_specific_question_addressed"
  )

  vals <- map(scoring_fields, ~ as_logical_or_na(x[[.x]]))

  # FAIL-SAFE:
  # Never convert an incomplete assessment into a substantive low score.
  if (any(map_lgl(vals, is.na))) {
    return(tibble(
      interface_score = NA_real_,
      interface_label = "Insufficient classification evidence",
      interface_score_complete = FALSE
    ))
  }

  used <- as_logical_or_na(x$variable_used_in_analysis)
  specific <- as_logical_or_na(x$interface_specific_question_addressed)
  quantitative <- as_logical_or_na(x$quantitative_variable_present)
  qualitative <- as_logical_or_na(x$qualitative_representation)
  conceptual <- as_logical_or_na(x$conceptual_framing)
  objective <- as_logical_or_na(x$interface_in_objectives)
  mentioned <- as_logical_or_na(x$interface_mentioned)

  score <- case_when(
    isTRUE(used) && isTRUE(specific) ~ 1.00,
    isTRUE(used) ~ 1.00,
    isTRUE(quantitative) ~ 0.50,
    isTRUE(qualitative) ~ 0.00,
    isTRUE(conceptual) || isTRUE(objective) ~ -0.50,
    isTRUE(mentioned) ~ -1.00,
    TRUE ~ NA_real_
  )

  label <- case_when(
    score == -1.00 ~ "Introductory mention",
    score == -0.50 ~ "Conceptual framing",
    score ==  0.00 ~ "Qualitative representation",
    score ==  0.50 ~ "Quantitative representation",
    score ==  1.00 ~ "Interface operationalised",
    TRUE ~ NA_character_
  )

  tibble(
    interface_score = score,
    interface_label = label,
    interface_score_complete = TRUE
  )
}

normalise_host_role <- function(x) {
  z <- scalar_or_na(x)
  if (is.na(z)) return(NA_character_)

  z <- str_to_upper(str_squish(z))

  if (z %in% c(
    "ABSENT", "CONTEXTUAL", "SECONDARY", "PRIMARY", "UNCLEAR"
  )) {
    return(z)
  }

  NA_character_
}

host_role_completion_stats <- function(host_obj) {
  roles <- c(
    normalise_host_role(host_obj$wildlife_role),
    normalise_host_role(host_obj$domestic_role)
  )

  completed <- sum(!is.na(roles) & roles != "UNCLEAR")

  tibble(
    module = "hosts",
    boolean_fields_expected = 2L,
    boolean_fields_completed = completed,
    boolean_fields_null = 2L - completed,
    boolean_completion_rate = round(completed / 2, 3)
  )
}

score_host_orientation <- function(x) {
  w <- normalise_host_role(x$wildlife_role)
  d <- normalise_host_role(x$domestic_role)

  substantive <- c("SECONDARY", "PRIMARY")

  score <- case_when(
    is.na(w) || is.na(d) || w == "UNCLEAR" || d == "UNCLEAR" ~ NA_real_,

    # Exclusive focus: only one host domain is substantively present.
    w %in% substantive && d == "ABSENT" ~ -1.00,
    d %in% substantive && w == "ABSENT" ~  1.00,

    # Predominant focus: the other host domain is only contextual.
    w %in% substantive && d == "CONTEXTUAL" ~ -0.75,
    d %in% substantive && w == "CONTEXTUAL" ~  0.75,

    # Both host domains are substantive, but one is clearly primary.
    w == "PRIMARY" && d == "SECONDARY" ~ -0.50,
    d == "PRIMARY" && w == "SECONDARY" ~  0.50,

    # Both host domains contribute substantively at comparable level.
    (w == "PRIMARY" && d == "PRIMARY") ||
      (w == "SECONDARY" && d == "SECONDARY") ~ 0.00,

    # No substantive host-domain focus can be established.
    TRUE ~ NA_real_
  )

  label <- case_when(
    score == -1.00 ~ "Exclusively wildlife-focused",
    score == -0.75 ~ "Predominantly wildlife-focused",
    score == -0.50 ~ "Wildlife-centred interface",
    score ==  0.00 ~ "Balanced host focus",
    score ==  0.50 ~ "Domestic-centred interface",
    score ==  0.75 ~ "Predominantly domestic-focused",
    score ==  1.00 ~ "Exclusively domestic-focused",
    TRUE ~ NA_character_
  )

  tibble(
    host_orientation_score = score,
    host_orientation_label = label,
    wildlife_role = w,
    domestic_role = d
  )
}

derive_animal_focus <- function(host_obj) {
  w <- normalise_host_role(host_obj$wildlife_role)
  d <- normalise_host_role(host_obj$domestic_role)

  w_substantive <- w %in% c("SECONDARY", "PRIMARY")
  d_substantive <- d %in% c("SECONDARY", "PRIMARY")

  case_when(
    is.na(w) || is.na(d) || w == "UNCLEAR" || d == "UNCLEAR" ~ "Unclear",
    w_substantive && d_substantive ~ "Both",
    w_substantive ~ "Wildlife",
    d_substantive ~ "Domestic",
    TRUE ~ "Unclear"
  )
}

# ============================================================
# EVIDENCE / AUDIT HELPERS
# ============================================================
flatten_evidence <- function(article_id, module_name, module_obj, page_tbl) {
  if (module_name != "interface") return(tibble())

  fields <- required_boolean_fields$interface

  map_dfr(fields, function(field_name) {
    quote <- scalar_or_na(
      module_obj[[paste0(field_name, "_evidence_quote")]]
    )
    page <- suppressWarnings(as.integer(
      module_obj[[paste0(field_name, "_evidence_page")]] %||% NA_integer_
    ))

    tibble(
      article_id = article_id,
      module = module_name,
      field = field_name,
      quote = quote,
      page = page,
      section = scalar_or_na(
        module_obj[[paste0(field_name, "_evidence_section")]]
      ),
      grounding_status = grounding_status(quote, page, page_tbl)
    )
  })
}

build_boolean_audit <- function(
  article_id,
  module_name,
  module_obj,
  page_tbl
) {
  fields <- required_boolean_fields[[module_name]]

  map_dfr(fields, function(field_name) {
    ai_value <- as_logical_or_na(module_obj[[field_name]])
    quote <- scalar_or_na(
      module_obj[[paste0(field_name, "_evidence_quote")]]
    )
    page <- suppressWarnings(as.integer(
      module_obj[[paste0(field_name, "_evidence_page")]] %||% NA_integer_
    ))

    status <- grounding_status(quote, page, page_tbl)

    tibble(
      audit_id = paste(article_id, module_name, field_name, sep = "__"),
      paper_id = article_id,
      construct = module_name,
      variable = field_name,
      ai_value = case_when(
        isTRUE(ai_value) ~ "TRUE",
        identical(ai_value, FALSE) ~ "FALSE",
        TRUE ~ "UNCLEAR"
      ),
      evidence_quote = quote,
      evidence_page = page,
      grounding_status = status,
      boolean_evidence_inconsistency =
        is.na(ai_value) && !is.na(quote) && nzchar(quote),
      reviewer_value = NA_character_,
      reviewer_agreement = NA_character_,
      reviewer_notes = NA_character_,
      final_human_value = NA_character_
    )
  })
}

build_host_role_audit <- function(article_id, host_obj, page_tbl) {
  make_row <- function(
    variable,
    role,
    quote,
    page,
    section
  ) {
    role_norm <- normalise_host_role(role)
    quote_norm <- scalar_or_na(quote)
    page_num <- suppressWarnings(as.integer(page %||% NA_integer_))

    tibble(
      audit_id = paste(article_id, "hosts", variable, sep = "__"),
      paper_id = article_id,
      construct = "hosts",
      variable = variable,
      ai_value = role_norm %||% "UNCLEAR",
      evidence_quote = quote_norm,
      evidence_page = page_num,
      grounding_status = grounding_status(
        quote_norm,
        page_num,
        page_tbl
      ),
      boolean_evidence_inconsistency =
        !is.na(role_norm) &&
        role_norm != "UNCLEAR" &&
        is.na(quote_norm),
      reviewer_value = NA_character_,
      reviewer_agreement = NA_character_,
      reviewer_notes = NA_character_,
      final_human_value = NA_character_
    )
  }

  bind_rows(
    make_row(
      "wildlife_role",
      host_obj$wildlife_role,
      host_obj$wildlife_evidence_quote,
      host_obj$wildlife_evidence_page,
      host_obj$wildlife_evidence_section
    ),
    make_row(
      "domestic_role",
      host_obj$domestic_role,
      host_obj$domestic_evidence_quote,
      host_obj$domestic_evidence_page,
      host_obj$domestic_evidence_section
    )
  )
}

# ============================================================
# QUANTITATIVE INTERFACE MEASURES
# ============================================================
flatten_quantitative_measures <- function(
  article_id,
  interface_obj,
  page_tbl
) {
  measures <- interface_obj$quantitative_measures

  if (is.null(measures) || length(measures) == 0) {
    return(tibble(
      quant_measure_id = character(),
      article_id = character(),
      measure_name = character(),
      what_it_represents = character(),
      measurement_method = character(),
      metric_or_unit = character(),
      role_in_analysis = character(),
      quantitative_result = character(),
      evidence_quote = character(),
      page = integer(),
      section = character(),
      grounding_status = character(),
      reviewer_measure_valid = character(),
      reviewer_quote_valid = character(),
      reviewer_notes = character()
    ))
  }

  map_dfr(seq_along(measures), function(i) {
    m <- measures[[i]]

    quote <- scalar_or_na(m$evidence_quote)
    page <- suppressWarnings(
      as.integer(m$page %||% NA_integer_)
    )

    tibble(
      quant_measure_id = paste0(article_id, "__quant__", i),
      article_id = article_id,
      measure_name = scalar_or_na(m$measure_name),
      what_it_represents = scalar_or_na(m$what_it_represents),
      measurement_method = scalar_or_na(m$measurement_method),
      metric_or_unit = scalar_or_na(m$metric_or_unit),
      role_in_analysis = scalar_or_na(m$role_in_analysis),
      quantitative_result = scalar_or_na(m$quantitative_result),
      evidence_quote = quote,
      page = page,
      section = scalar_or_na(m$section),
      grounding_status = grounding_status(
        quote,
        page,
        page_tbl
      ),
      reviewer_measure_valid = NA_character_,
      reviewer_quote_valid = NA_character_,
      reviewer_notes = NA_character_
    )
  })
}

make_quantitative_audit_sample <- function(
  quant_full,
  target_n = 50,
  seed = 42
) {
  if (nrow(quant_full) == 0 || target_n <= 0) return(quant_full)

  set.seed(seed)

  tagged <- quant_full |>
    mutate(
      grounding_group = case_when(
        grounding_status %in% c("EXACT", "NORMALISED_EXACT") ~ "grounded",
        TRUE ~ "needs_attention"
      ),
      role_group = if_else(
        is.na(role_in_analysis) | !nzchar(role_in_analysis),
        "role_missing",
        str_to_lower(str_squish(role_in_analysis))
      )
    )

  sampled <- tagged |>
    group_by(grounding_group, role_group) |>
    group_modify(
      ~ slice_sample(.x, n = min(nrow(.x), 2))
    ) |>
    ungroup()

  if (nrow(sampled) >= target_n) {
    sampled <- slice_sample(sampled, n = target_n)
  } else {
    remaining <- tagged |>
      anti_join(
        sampled |> select(quant_measure_id),
        by = "quant_measure_id"
      )

    top_up <- min(
      target_n - nrow(sampled),
      nrow(remaining)
    )

    if (top_up > 0) {
      sampled <- bind_rows(
        sampled,
        slice_sample(remaining, n = top_up)
      )
    }
  }

  sampled |>
    select(-any_of(c("grounding_group", "role_group")))
}

# ============================================================
# DISEASE FLATTENING
# ============================================================
flatten_diseases <- function(article_id, diseases_obj) {
  if (is.null(diseases_obj) || length(diseases_obj) == 0) {
    return(tibble(
      article_id = character(),
      disease = character(),
      agent_type = character(),
      transmission_mode = character()
    ))
  }

  map_dfr(diseases_obj, function(d) {
    disease <- scalar_or_na(d$name)
    agent_type <- scalar_or_na(d$agent_type)
    modes <- d$transmission_modes %||% list()

    modes_vec <- safe_trim(unlist(modes))
    modes_vec <- modes_vec[nzchar(modes_vec)]
    if (length(modes_vec) == 0) modes_vec <- NA_character_

    tibble(
      article_id = article_id,
      disease = disease,
      agent_type = agent_type,
      transmission_mode = modes_vec
    )
  }) |>
    distinct()
}

# ============================================================
# RUN ONE MODULE
# ============================================================
run_module <- function(module_name, context_text) {
  prompt <- switch(
    module_name,
    metadata = build_metadata_prompt(context_text),
    interface = build_interface_prompt(context_text),
    hosts = build_hosts_prompt(context_text),
    stop("Unknown module: ", module_name)
  )

  llm <- tryCatch(
    ask_openai_json(
      prompt = prompt,
      schema = module_schema(module_name),
      schema_name = paste0("synthesis_", module_name)
    ),
    error = function(e) list(
      raw = toJSON(
        list(parse_error = TRUE, error_message = conditionMessage(e)),
        auto_unbox = TRUE
      ),
      time_sec = NA_real_,
      model = model_name,
      prompt_tokens = NA_real_,
      output_tokens = NA_real_,
      response_chars = NA_real_
    )
  )

  prompt_chars <- nchar(prompt, type = "chars")
  context_chars <- nchar(context_text, type = "chars")
  response_chars <- llm$response_chars %||% nchar(llm$raw, type = "chars")

  prompt_tokens_est <- ifelse(
    is.na(llm$prompt_tokens),
    ceiling(prompt_chars / chars_per_token_fallback),
    llm$prompt_tokens
  )

  output_tokens_est <- ifelse(
    is.na(llm$output_tokens),
    ceiling(response_chars / chars_per_token_fallback),
    llm$output_tokens
  )

  list(
    parsed = parse_json_safe(llm$raw),
    raw = llm$raw,
    time_sec = llm$time_sec,
    model = llm$model,
    context_chars = context_chars,
    prompt_chars = prompt_chars,
    response_chars = response_chars,
    prompt_tokens = llm$prompt_tokens,
    output_tokens = llm$output_tokens,
    prompt_tokens_est = prompt_tokens_est,
    output_tokens_est = output_tokens_est
  )
}


# ============================================================
# STRATIFIED HUMAN AUDIT SAMPLE
# ============================================================
make_stratified_audit_sample <- function(audit_full, target_n = 100, seed = 42) {
  if (nrow(audit_full) == 0 || target_n <= 0) return(audit_full)

  set.seed(seed)

  audit_tagged <- audit_full |>
    mutate(
      ai_value = if_else(
        ai_value %in% c(
          "TRUE", "FALSE", "UNCLEAR",
          "ABSENT", "CONTEXTUAL", "SECONDARY", "PRIMARY"
        ),
        ai_value,
        "UNCLEAR"
      )
    )

  # Step 1: ensure every variable and AI class is represented where possible.
  strata <- audit_tagged |>
    count(construct, variable, ai_value, name = "available")

  base_n <- max(1L, floor(target_n / nrow(strata)))

  sampled <- audit_tagged |>
    group_by(construct, variable, ai_value) |>
    group_modify(~ slice_sample(.x, n = min(nrow(.x), base_n))) |>
    ungroup()

  # Step 2: move toward an approximately 50/50 construct split.
  desired <- tibble(
    construct = c("interface", "hosts"),
    target = c(floor(target_n / 2), target_n - floor(target_n / 2))
  )

  for (cc in desired$construct) {
    need <- desired$target[desired$construct == cc] -
      sum(sampled$construct == cc)
    need <- max(0, need)

    if (need > 0) {
      pool <- audit_tagged |>
        filter(construct == cc) |>
        anti_join(sampled |> select(audit_id), by = "audit_id")

      take <- min(need, nrow(pool))
      if (take > 0) sampled <- bind_rows(sampled, slice_sample(pool, n = take))
    }
  }

  # Step 3: top up from any remaining eligible rows.
  if (nrow(sampled) < target_n) {
    pool <- audit_tagged |>
      anti_join(sampled |> select(audit_id), by = "audit_id")

    take <- min(target_n - nrow(sampled), nrow(pool))
    if (take > 0) sampled <- bind_rows(sampled, slice_sample(pool, n = take))
  }

  # Step 4: if rounding overshoots, trim reproducibly.
  if (nrow(sampled) > target_n) {
    sampled <- slice_sample(sampled, n = target_n)
  }

  sampled
}

# ============================================================
# PROCESS ONE PDF
# ============================================================
process_one_pdf <- function(pdf_file) {
  pages <- extract_pdf_text_safe(pdf_file)
  if (length(pages) == 0) stop("No readable PDF text.")

  article_id <- make_article_id(pdf_file)
  file_name <- basename(pdf_file)
  page_tbl <- make_page_records(pages)

  pdf_quality <- pdf_quality_metrics(article_id, file_name, pages)

  master_row <- master_records |>
    filter(paper_id == article_id)

  if (nrow(master_row) != 1) {
    stop(
      "Expected exactly one master_table match for ",
      article_id,
      "; found ", nrow(master_row), "."
    )
  }

  master_record_id <- master_row$ID[[1]]
  author <- master_row$Author[[1]]
  title <- master_row$Title[[1]]
  journal <- master_row$Journal[[1]]
  doi <- master_row$DOI[[1]]
  metadata_join_status <- "MATCHED"

  modules <- c("metadata", "interface", "hosts")
  module_results <- set_names(vector("list", length(modules)), modules)

  for (m in modules) {
    context_text <- select_module_context(page_tbl, m)
    module_results[[m]] <- run_module(m, context_text)
  }

  metadata_obj  <- module_results$metadata$parsed
  interface_obj <- module_results$interface$parsed
  hosts_obj     <- module_results$hosts$parsed

  interface_score <- score_interface(interface_obj)
  host_score <- score_host_orientation(hosts_obj)
  animal_focus <- derive_animal_focus(hosts_obj)

  evidence_long <- bind_rows(
    flatten_evidence(article_id, "interface", interface_obj, page_tbl),
    flatten_evidence(article_id, "hosts", hosts_obj, page_tbl)
  )

  audit_long <- bind_rows(
    build_boolean_audit(
      article_id, "interface", interface_obj, page_tbl
    ),
    build_host_role_audit(
      article_id, hosts_obj, page_tbl
    )
  )

  module_audit <- map_dfr(modules, function(m) {
    completion <- if (m == "interface") {
      module_completion_stats(
        "interface",
        module_results[[m]]$parsed
      )
    } else if (m == "hosts") {
      host_role_completion_stats(
        module_results[[m]]$parsed
      )
    } else {
      tibble(
        module = m,
        boolean_fields_expected = NA_integer_,
        boolean_fields_completed = NA_integer_,
        boolean_fields_null = NA_integer_,
        boolean_completion_rate = NA_real_
      )
    }

    tibble(
      article_id = article_id,
      module = m,
      raw_output = module_results[[m]]$raw,
      parse_error = isTRUE(module_results[[m]]$parsed$parse_error),
      model = module_results[[m]]$model,
      time_sec = module_results[[m]]$time_sec,
      context_chars = module_results[[m]]$context_chars,
      prompt_chars = module_results[[m]]$prompt_chars,
      response_chars = module_results[[m]]$response_chars,
      prompt_tokens = module_results[[m]]$prompt_tokens,
      output_tokens = module_results[[m]]$output_tokens,
      prompt_tokens_est = module_results[[m]]$prompt_tokens_est,
      output_tokens_est = module_results[[m]]$output_tokens_est
    ) |>
      bind_cols(completion |> select(-module))
  })

  quantitative_measures_long <- flatten_quantitative_measures(
    article_id,
    interface_obj,
    page_tbl
  )

  disease_long <- flatten_diseases(article_id, metadata_obj$diseases)

  disease_names <- disease_long |>
    filter(!is.na(disease), nzchar(disease)) |>
    pull(disease) |>
    unique()

  agent_types <- disease_long |>
    filter(!is.na(agent_type), nzchar(agent_type)) |>
    pull(agent_type) |>
    unique()

  transmission_modes <- disease_long |>
    filter(!is.na(transmission_mode), nzchar(transmission_mode)) |>
    pull(transmission_mode) |>
    unique()

  interface_quote <- scalar_or_na(interface_obj$interface_statement_verbatim)
  interface_page <- suppressWarnings(
    as.integer(interface_obj$interface_statement_page %||% NA_integer_)
  )
  interface_quote_grounding <- grounding_status(
    interface_quote,
    interface_page,
    page_tbl
  )

  paper_summary <- tibble(
    article_id = article_id,
    paper_id = article_id,
    file_name = file_name,
    master_record_id = master_record_id,
    metadata_join_status = metadata_join_status,
    Author = author,
    Title = title,
    Journal = journal,
    DOI = doi,

    study_type = scalar_or_na(metadata_obj$study_type),
    study_design = scalar_or_na(metadata_obj$study_design),
    countries_investigated = collapse_array(metadata_obj$countries_investigated),

    animal_focus = animal_focus,
    wildlife_taxa = collapse_array(metadata_obj$wildlife_taxa),
    domestic_taxa = collapse_array(metadata_obj$domestic_taxa),
    wildlife_species = collapse_array(metadata_obj$wildlife_species),
    domestic_species = collapse_array(metadata_obj$domestic_species),

    diseases_or_pathogens = if (length(disease_names) > 0) {
      paste(disease_names, collapse = "; ")
    } else NA_character_,
    disease_agent_types = if (length(agent_types) > 0) {
      paste(agent_types, collapse = "; ")
    } else NA_character_,
    transmission_modes = if (length(transmission_modes) > 0) {
      paste(transmission_modes, collapse = "; ")
    } else NA_character_,

    host_orientation = host_score$host_orientation_score,
    host_orientation_label = host_score$host_orientation_label,
    wildlife_role = host_score$wildlife_role,
    domestic_role = host_score$domestic_role,
    interface_operationalisation = interface_score$interface_score,
    interface_label = interface_score$interface_label,
    interface_score_complete = interface_score$interface_score_complete,

    host_focus_group = case_when(
      is.na(host_score$host_orientation_score) ~ "Unclassified",
      host_score$host_orientation_score < -0.20 ~ "Wildlife-centred",
      host_score$host_orientation_score >  0.20 ~ "Domestic-centred",
      TRUE ~ "Balanced wild–domestic"
    ),

    interface_explicit = as_logical_or_na(interface_obj$interface_explicit),
    interface_term_used = scalar_or_na(interface_obj$interface_term_used),
    interface_statement_kind =
      scalar_or_na(interface_obj$interface_statement_kind),
    interface_statement_verbatim = interface_quote,
    interface_statement_page = interface_page,
    interface_statement_section =
      scalar_or_na(interface_obj$interface_statement_section),
    interface_statement_grounding = interface_quote_grounding,

    quantitative_interface_measures_n =
      nrow(quantitative_measures_long),
    quantitative_interface_measure_names = if (
      nrow(quantitative_measures_long) > 0
    ) {
      paste(
        unique(na.omit(quantitative_measures_long$measure_name)),
        collapse = "; "
      )
    } else NA_character_,

    interface_boolean_completion =
      module_completion_stats("interface", interface_obj)$boolean_completion_rate,
    hosts_role_completion =
      host_role_completion_stats(hosts_obj)$boolean_completion_rate,

    processing_time_sec = sum(module_audit$time_sec, na.rm = TRUE),
    processing_time_min =
      round(sum(module_audit$time_sec, na.rm = TRUE) / 60, 2),
    prompt_tokens_total =
      sum(module_audit$prompt_tokens_est, na.rm = TRUE),
    output_tokens_total =
      sum(module_audit$output_tokens_est, na.rm = TRUE),

    estimated_api_cost_usd =
      (sum(module_audit$prompt_tokens_est, na.rm = TRUE) / 1e6) *
        api_input_usd_per_million_tokens +
      (sum(module_audit$output_tokens_est, na.rm = TRUE) / 1e6) *
        api_output_usd_per_million_tokens,

    any_parse_error = any(module_audit$parse_error, na.rm = TRUE),
    any_boolean_evidence_inconsistency =
      any(audit_long$boolean_evidence_inconsistency, na.rm = TRUE),
    any_quote_not_found =
      any(evidence_long$grounding_status == "NOT_FOUND", na.rm = TRUE) ||
      identical(interface_quote_grounding, "NOT_FOUND"),
    interface_needs_review = !isTRUE(
      interface_score$interface_score_complete
    ),

    pdf_text_status = pdf_quality$pdf_text_status
  )

  conceptual_evidence <- paper_summary |>
    transmute(
      paper_id = article_id,
      file_name,
      master_record_id,
      title = Title,
      journal = Journal,
      doi = DOI,
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

  plot_data <- paper_summary |>
    transmute(
      paper_id = article_id,
      file_name,
      master_record_id,
      title = Title,
      author = Author,
      journal = Journal,
      doi = DOI,
      study_type,
      animal_focus,
      host_orientation,
      interface_operationalisation,
      host_focus_group,
      interface_explicit,
      interface_statement_verbatim,
      interface_statement_page,
      pdf_text_status,
      citations_per_year = NA_real_
    )

  list(
    summary = paper_summary,
    plot_data = plot_data,
    disease_long = disease_long,
    quantitative_measures = quantitative_measures_long,
    conceptual = conceptual_evidence,
    evidence = evidence_long,
    audit = audit_long,
    module_audit = module_audit,
    pdf_quality = pdf_quality
  )
}

# ============================================================
# CHECKPOINTS / RESUME
# ============================================================
summary_checkpoint <- file.path(
  results_dir, paste0(final_prefix, "_lean_summary_checkpoint.csv")
)
plot_checkpoint <- file.path(
  results_dir, paste0(final_prefix, "_lean_plot_checkpoint.csv")
)
disease_checkpoint <- file.path(
  results_dir, paste0(final_prefix, "_lean_disease_checkpoint.csv")
)
quantitative_checkpoint <- file.path(
  results_dir, paste0(final_prefix, "_lean_quantitative_checkpoint.csv")
)
conceptual_checkpoint <- file.path(
  results_dir, paste0(final_prefix, "_lean_conceptual_checkpoint.csv")
)
evidence_checkpoint <- file.path(
  results_dir, paste0(final_prefix, "_lean_evidence_checkpoint.csv")
)
audit_checkpoint <- file.path(
  results_dir, paste0(final_prefix, "_lean_audit_checkpoint.csv")
)
module_audit_checkpoint <- file.path(
  results_dir, paste0(final_prefix, "_lean_module_audit_checkpoint.csv")
)
pdf_quality_checkpoint <- file.path(
  results_dir, paste0(final_prefix, "_lean_pdf_quality_checkpoint.csv")
)

processing_log_file <- file.path(
  results_dir, paste0(final_prefix, "_processing_log.csv")
)

read_checkpoint <- function(path) {
  if (resume && file_exists(path)) {
    read_csv(path, show_col_types = FALSE)
  } else {
    tibble()
  }
}

summary_accum <- read_checkpoint(summary_checkpoint)
plot_accum <- read_checkpoint(plot_checkpoint)
disease_accum <- read_checkpoint(disease_checkpoint)
quantitative_accum <- read_checkpoint(quantitative_checkpoint)
conceptual_accum <- read_checkpoint(conceptual_checkpoint)
evidence_accum <- read_checkpoint(evidence_checkpoint)
audit_accum <- read_checkpoint(audit_checkpoint)
module_audit_accum <- read_checkpoint(module_audit_checkpoint)
pdf_quality_accum <- read_checkpoint(pdf_quality_checkpoint)

processing_log <- if (file_exists(processing_log_file)) {
  read_csv(processing_log_file, show_col_types = FALSE)
} else {
  tibble(
    paper_id = character(),
    file_name = character(),
    status = character(),
    start_time = character(),
    end_time = character(),
    elapsed_sec = numeric(),
    model = character(),
    prompt_tokens = numeric(),
    output_tokens = numeric(),
    estimated_api_cost_usd = numeric(),
    error_message = character()
  )
}

existing_done <- if ("article_id" %in% names(summary_accum)) {
  unique(summary_accum$article_id)
} else {
  character()
}

pdf_files <- dir_ls(pdf_dir, regexp = "\\.pdf$", type = "file") |> sort()
if (test_mode) pdf_files <- head(pdf_files, max_pdfs_test)

selected_paper_ids <- map_chr(pdf_files, make_article_id)

# TARGETED-RERUN PRE-FLIGHT GUARD -----------------------------------------
expected_paper_ids <- c("a52", "a122")

if (length(selected_paper_ids) != length(expected_paper_ids) ||
    !setequal(selected_paper_ids, expected_paper_ids)) {
  stop(
    "SAFETY STOP: targeted rerun expects exactly a52 and a122. Found: ",
    ifelse(length(selected_paper_ids) == 0,
           "<none>", paste(selected_paper_ids, collapse = ", ")),
    "\nNote: make_article_id() prepends 'a' to the PDF filename stem; ",
    "use the same PDF filenames as the original production run."
  )
}

message("============================================================")
message("TARGETED QC RERUN — a52 + a122")
message("Input directory:  ", pdf_dir)
message("Output directory: ", results_dir)
message("Original results (READ/WRITE PROHIBITED): ", original_results_dir)
message("Selected papers: ", paste(sort(selected_paper_ids), collapse = ", "))
message("============================================================")

unmatched_paper_ids <- setdiff(
  selected_paper_ids,
  master_records$paper_id
)

if (length(unmatched_paper_ids) > 0) {
  stop(
    "These selected PDFs have no paper_id match in master_table.xlsx: ",
    paste(unmatched_paper_ids, collapse = ", ")
  )
}

message(
  "Metadata preflight passed: all ",
  length(selected_paper_ids),
  " selected PDFs have a unique master-table match."
)

todo_files <- pdf_files[
  !map_chr(pdf_files, make_article_id) %in% existing_done
]

message("Total selected PDFs: ", length(pdf_files))
message("Already done: ", length(existing_done))
message("To process now: ", length(todo_files))

for (i in seq_along(todo_files)) {
  pdf_file <- todo_files[i]
  this_paper_id <- make_article_id(pdf_file)
  this_file_name <- basename(pdf_file)

  message(
    "[", i, "/", length(todo_files), "] Processing: ",
    this_file_name
  )

  started_at <- Sys.time()

  res <- tryCatch(
    process_one_pdf(pdf_file),
    error = function(e) {
      ended_at <- Sys.time()

      processing_log <<- bind_rows(
        processing_log,
        tibble(
          paper_id = this_paper_id,
          file_name = this_file_name,
          status = "ERROR",
          start_time = format(started_at, "%Y-%m-%d %H:%M:%S"),
          end_time = format(ended_at, "%Y-%m-%d %H:%M:%S"),
          elapsed_sec = as.numeric(
            difftime(ended_at, started_at, units = "secs")
          ),
          model = model_name,
          prompt_tokens = NA_real_,
          output_tokens = NA_real_,
          estimated_api_cost_usd = NA_real_,
          error_message = conditionMessage(e)
        )
      )

      write_csv(processing_log, processing_log_file)

      message(
        "Error in ", this_file_name, ": ",
        conditionMessage(e)
      )

      NULL
    }
  )

  if (is.null(res)) next

  summary_accum <- bind_rows(summary_accum, res$summary)
  plot_accum <- bind_rows(plot_accum, res$plot_data)
  disease_accum <- bind_rows(disease_accum, res$disease_long)
  quantitative_accum <- bind_rows(
    quantitative_accum,
    res$quantitative_measures
  )
  conceptual_accum <- bind_rows(conceptual_accum, res$conceptual)
  evidence_accum <- bind_rows(evidence_accum, res$evidence)
  audit_accum <- bind_rows(audit_accum, res$audit)
  module_audit_accum <- bind_rows(
    module_audit_accum, res$module_audit
  )
  pdf_quality_accum <- bind_rows(
    pdf_quality_accum, res$pdf_quality
  )

  # Immediately de-duplicate by paper after a successful completion.
  summary_accum <- summary_accum |>
    distinct(article_id, .keep_all = TRUE)

  plot_accum <- plot_accum |>
    distinct(paper_id, .keep_all = TRUE)

  conceptual_accum <- conceptual_accum |>
    distinct(paper_id, .keep_all = TRUE)

  audit_accum <- audit_accum |>
    distinct(audit_id, .keep_all = TRUE)

  pdf_quality_accum <- pdf_quality_accum |>
    distinct(article_id, .keep_all = TRUE)

  if ("quant_measure_id" %in% names(quantitative_accum)) {
    quantitative_accum <- quantitative_accum |>
      distinct(quant_measure_id, .keep_all = TRUE)
  }

  if (all(c("article_id", "module") %in% names(module_audit_accum))) {
    module_audit_accum <- module_audit_accum |>
      distinct(article_id, module, .keep_all = TRUE)
  }

  if (all(c("article_id", "module", "field") %in% names(evidence_accum))) {
    evidence_accum <- evidence_accum |>
      distinct(article_id, module, field, .keep_all = TRUE)
  }

  ended_at <- Sys.time()

  paper_log_row <- res$summary |>
    transmute(
      paper_id = article_id,
      file_name = file_name,
      status = "COMPLETED",
      start_time = format(started_at, "%Y-%m-%d %H:%M:%S"),
      end_time = format(ended_at, "%Y-%m-%d %H:%M:%S"),
      elapsed_sec = as.numeric(
        difftime(ended_at, started_at, units = "secs")
      ),
      model = model_name,
      prompt_tokens = prompt_tokens_total,
      output_tokens = output_tokens_total,
      estimated_api_cost_usd = estimated_api_cost_usd,
      error_message = NA_character_
    )

  processing_log <- bind_rows(
    processing_log,
    paper_log_row
  ) |>
    group_by(paper_id) |>
    slice_tail(n = 1) |>
    ungroup()

  # CRITICAL SAFETY STEP:
  # Save every scientific checkpoint immediately after each completed paper.
  write_csv(summary_accum, summary_checkpoint)
  write_csv(plot_accum, plot_checkpoint)
  write_csv(disease_accum, disease_checkpoint)
  write_csv(quantitative_accum, quantitative_checkpoint)
  write_csv(conceptual_accum, conceptual_checkpoint)
  write_csv(evidence_accum, evidence_checkpoint)
  write_csv(audit_accum, audit_checkpoint)
  write_csv(module_audit_accum, module_audit_checkpoint)
  write_csv(pdf_quality_accum, pdf_quality_checkpoint)
  write_csv(processing_log, processing_log_file)

  message(
    "  Saved checkpoint for ", this_paper_id,
    " (", round(res$summary$processing_time_min, 2), " min; US$",
    round(res$summary$estimated_api_cost_usd, 4), ")."
  )
}

# ============================================================
# FINAL EXPORT
# ============================================================
summary_accum <- summary_accum |>
  distinct(article_id, .keep_all = TRUE)

plot_accum <- plot_accum |>
  distinct(paper_id, .keep_all = TRUE)

conceptual_accum <- conceptual_accum |>
  distinct(paper_id, .keep_all = TRUE)

audit_accum <- audit_accum |>
  distinct(audit_id, .keep_all = TRUE)

pdf_quality_accum <- pdf_quality_accum |>
  distinct(article_id, .keep_all = TRUE)

# Stratified audit sample:
# approximately half Interface and half Hosts, while ensuring representation
# across individual variables and TRUE/FALSE/UNCLEAR where those occur.
audit_target_n <- min(100, nrow(audit_accum))

audit_sample <- make_stratified_audit_sample(
  audit_accum,
  target_n = audit_target_n,
  seed = 42
) |>
  left_join(
    audit_question_lookup,
    by = c("construct", "variable")
  ) |>
  select(
    audit_id,
    paper_id,
    construct,
    audit_question,
    audit_definition,
    ai_value,
    evidence_quote,
    evidence_page,
    grounding_status,
    reviewer_value,
    reviewer_agreement,
    reviewer_notes,
    final_human_value,
    variable,
    everything()
  )

quantitative_audit_target_n <- min(50, nrow(quantitative_accum))
quantitative_audit_sample <- make_quantitative_audit_sample(
  quantitative_accum,
  target_n = quantitative_audit_target_n,
  seed = 42
)

benchmark_by_paper <- summary_accum |>
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

benchmark_run_summary <- tibble(
  run_prefix = final_prefix,
  model = model_name,
  completed_papers = nrow(summary_accum),
  target_full_corpus_n = target_full_corpus_n,
  total_runtime_hours_observed =
    sum(summary_accum$processing_time_sec, na.rm = TRUE) / 3600,
  mean_minutes_per_paper =
    mean(summary_accum$processing_time_min, na.rm = TRUE),
  median_minutes_per_paper =
    median(summary_accum$processing_time_min, na.rm = TRUE),
  projected_full_runtime_hours_sequential =
    mean(summary_accum$processing_time_sec, na.rm = TRUE) *
    target_full_corpus_n / 3600,
  mean_prompt_tokens_per_paper =
    mean(summary_accum$prompt_tokens_total, na.rm = TRUE),
  mean_output_tokens_per_paper =
    mean(summary_accum$output_tokens_total, na.rm = TRUE),
  projected_full_prompt_tokens =
    mean(summary_accum$prompt_tokens_total, na.rm = TRUE) *
    target_full_corpus_n,
  projected_full_output_tokens =
    mean(summary_accum$output_tokens_total, na.rm = TRUE) *
    target_full_corpus_n,
  observed_api_cost_usd =
    sum(summary_accum$estimated_api_cost_usd, na.rm = TRUE),
  mean_api_cost_per_paper_usd =
    mean(summary_accum$estimated_api_cost_usd, na.rm = TRUE),
  projected_full_api_cost_usd =
    mean(summary_accum$estimated_api_cost_usd, na.rm = TRUE) *
    target_full_corpus_n,
  papers_pdf_not_ok =
    sum(summary_accum$pdf_text_status != "OK", na.rm = TRUE),
  papers_with_parse_error =
    sum(summary_accum$any_parse_error, na.rm = TRUE),
  papers_with_quote_not_found =
    sum(summary_accum$any_quote_not_found, na.rm = TRUE),
  papers_with_boolean_evidence_inconsistency =
    sum(summary_accum$any_boolean_evidence_inconsistency, na.rm = TRUE)
)

final_summary_csv <- file.path(
  results_dir, paste0(final_prefix, "_paper_summary_lean.csv")
)
final_plot_csv <- file.path(
  results_dir, paste0(final_prefix, "_plot_data.csv")
)
final_disease_csv <- file.path(
  results_dir, paste0(final_prefix, "_disease_long.csv")
)
final_quantitative_csv <- file.path(
  results_dir, paste0(final_prefix, "_quantitative_measures_long.csv")
)
final_quantitative_audit_csv <- file.path(
  results_dir, paste0(final_prefix, "_quantitative_audit_sample.csv")
)
final_conceptual_csv <- file.path(
  results_dir, paste0(final_prefix, "_conceptual_evidence.csv")
)
final_evidence_csv <- file.path(
  results_dir, paste0(final_prefix, "_evidence_long_lean.csv")
)
final_audit_csv <- file.path(
  results_dir, paste0(final_prefix, "_audit_long_lean.csv")
)
final_audit_sample_csv <- file.path(
  results_dir, paste0(final_prefix, "_audit_sample_lean.csv")
)
final_module_audit_csv <- file.path(
  results_dir, paste0(final_prefix, "_module_audit_lean.csv")
)
final_pdf_quality_csv <- file.path(
  results_dir, paste0(final_prefix, "_pdf_quality.csv")
)
final_benchmark_paper_csv <- file.path(
  results_dir, paste0(final_prefix, "_benchmark_by_paper_lean.csv")
)
final_benchmark_run_csv <- file.path(
  results_dir, paste0(final_prefix, "_benchmark_run_summary_lean.csv")
)
final_xlsx <- file.path(
  results_dir, paste0(final_prefix, "_lean_results.xlsx")
)
final_audit_xlsx <- file.path(
  results_dir, paste0(final_prefix, "_lean_human_audit.xlsx")
)

write_csv(summary_accum, final_summary_csv)
write_csv(plot_accum, final_plot_csv)
write_csv(disease_accum, final_disease_csv)
write_csv(quantitative_accum, final_quantitative_csv)
write_csv(
  quantitative_audit_sample,
  final_quantitative_audit_csv
)
write_csv(conceptual_accum, final_conceptual_csv)
write_csv(evidence_accum, final_evidence_csv)
write_csv(audit_accum, final_audit_csv)
write_csv(audit_sample, final_audit_sample_csv)
write_csv(module_audit_accum, final_module_audit_csv)
write_csv(pdf_quality_accum, final_pdf_quality_csv)
write_csv(benchmark_by_paper, final_benchmark_paper_csv)
write_csv(benchmark_run_summary, final_benchmark_run_csv)
write_csv(processing_log, processing_log_file)

write.xlsx(
  list(
    README = tibble(
      sheet = c(
        "paper_summary",
        "plot_data",
        "disease_long",
        "quantitative_measures",
        "conceptual_evidence",
        "audit_sample",
        "pdf_quality",
        "benchmark",
        "processing_log"
      ),
      purpose = c(
        "One row per paper; descriptive synthesis + Fig 5 scores.",
        "Direct input for the Fig 5 synthesis script.",
        "One row per disease/transmission-mode combination.",
        "One row per interface-relevant quantitative measure with method, metric, analytical role, result, quotation and page.",
        "One row per paper with explicit/implicit interface statement.",
        "Stratified human-review sample of Figure 5 Boolean decisions with plain-language questions.",
        "PDF text-extraction quality diagnostics.",
        "Per-paper runtime/token/QA metrics.",
        "Persistent per-paper processing status, timing, token and error log."
      )
    ),
    paper_summary = summary_accum,
    plot_data = plot_accum,
    disease_long = disease_accum,
    quantitative_measures = quantitative_accum,
    conceptual_evidence = conceptual_accum,
    audit_sample = audit_sample,
    pdf_quality = pdf_quality_accum,
    benchmark = benchmark_by_paper,
    processing_log = processing_log
  ),
  file = final_xlsx,
  overwrite = TRUE
)

audit_instructions <- tibble(
  step = 1:8,
  instruction = c(
    "Review each row independently. Do not use or infer the final Figure 5 score.",
    "Read the plain-language audit question and definition first.",
    "Compare the AI answer (TRUE/FALSE/UNCLEAR) with the supporting quotation.",
    "If the quotation is insufficient, inspect the original PDF at the reported page.",
    "For interface rows enter TRUE, FALSE, or UNCLEAR. For host-role rows enter ABSENT, CONTEXTUAL, SECONDARY, PRIMARY, or UNCLEAR.",
    "Enter reviewer_agreement as AGREE, DISAGREE, or UNCLEAR.",
    "Use reviewer_notes for disagreements, ambiguity, or PDF/readability problems.",
    "Do not change audit_id, paper_id, construct, variable, AI answer, evidence, or grounding fields."
  )
)

audit_value_guide <- tibble(
  reviewer_value = c("TRUE", "FALSE", "ABSENT", "CONTEXTUAL", "SECONDARY", "PRIMARY", "UNCLEAR"),
  meaning = c(
    "Interface criterion is supported by the paper.",
    "Interface criterion is not supported by the paper.",
    "Host domain is not substantively present.",
    "Host domain appears mainly as background/context/source/recipient.",
    "Host domain is genuinely included but subordinate to the other domain.",
    "Host domain is a central substantive focus of the paper.",
    "The available evidence is insufficient or genuinely ambiguous."
  )
)

write.xlsx(
  list(
    INSTRUCTIONS = audit_instructions,
    VALUE_GUIDE = audit_value_guide,
    QUESTION_DEFINITIONS = audit_question_lookup,
    AUDIT_SAMPLE = audit_sample,
    QUANTITATIVE_AUDIT = quantitative_audit_sample,
    PDF_QUALITY = pdf_quality_accum
  ),
  file = final_audit_xlsx,
  overwrite = TRUE
)

message("Lean synthesis complete.")
message("Saved paper summary: ", final_summary_csv)
message("Saved Fig 5 plot data: ", final_plot_csv)
message("Saved disease long data: ", final_disease_csv)
message("Saved quantitative measures: ", final_quantitative_csv)
message(
  "Saved quantitative audit sample: ",
  final_quantitative_audit_csv
)
message("Saved conceptual evidence: ", final_conceptual_csv)
message("Saved audit sample: ", final_audit_sample_csv)
message("Saved PDF quality: ", final_pdf_quality_csv)
message("Saved benchmark summary: ", final_benchmark_run_csv)
message("Saved processing log: ", processing_log_file)
message("Saved results workbook: ", final_xlsx)
message("Saved human-audit workbook: ", final_audit_xlsx)
