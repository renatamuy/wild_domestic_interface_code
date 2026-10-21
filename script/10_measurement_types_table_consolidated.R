# Interface measurements

library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(tibble)

# ============================================================
# 1. READ DATA
# ============================================================

setwd('../results_synthesis_consolidated')

file <- "evidence_first_test_manuscript_summary_tables.xlsx"

qm <- read_excel(
  file,
  sheet = "quantitative_measures"
)

# Basic checks
nrow(qm)
n_distinct(qm$paper_id)


# ============================================================
# 2. TEXT USED FOR CLASSIFICATION
# ============================================================
# Use the available descriptions of each quantitative measure.
# This gives more evidence than classifying from measure_name alone.

qm <- qm %>%
  mutate(
    classification_text = str_to_lower(
      paste(
        coalesce(measure_name, ""),
        coalesce(what_it_represents, ""),
        coalesce(measurement_method, ""),
        coalesce(role_in_analysis, ""),
        sep = " | "
      )
    )
  )


# ============================================================
# 3. HIGH-LEVEL INTERFACE DIMENSIONS
# ============================================================
# Categories are NON-MUTUALLY EXCLUSIVE.
# A measure/study can therefore contribute to >1 dimension.

dimension_patterns <- list(
  
  "Spatial proximity" = paste(
    "\\bdistance\\b",
    "\\bproximity\\b",
    "\\bnear(?:by)?\\b",
    "\\bbuffer\\b",
    "\\bdistance to\\b",
    "\\bproximity to\\b",
    sep = "|"
  ),
  
  "Spatial overlap / co-occurrence" = paste(
    "\\boverlap\\b",
    "\\bco-occurr?ence\\b",
    "\\bco-occur\\b",
    "\\bshared space\\b",
    "\\bspace use\\b",
    "\\bhome range\\b",
    "\\bhome-range\\b",
    "\\butili[sz]ation distribution\\b",
    "\\bsympatr",
    sep = "|"
  ),
  
  "Direct/indirect contact" = paste(
    "\\bcontact\\b",
    "\\binteraction\\b",
    "\\bencounter\\b",
    "\\bcontact rate\\b",
    "\\bcontact frequency\\b",
    "\\bshared feeding\\b",
    "\\bshared water",
    "\\bshared grazing\\b",
    "\\bshared resource",
    "\\bshared site",
    "\\bvisit(?:ation)?\\b",
    sep = "|"
  ),
  
  "Landscape/interface structure" = paste(
    "\\blandscape\\b",
    "\\bland use\\b",
    "\\bland-use\\b",
    "\\bhabitat\\b",
    "\\bfragmentation\\b",
    "\\bedge\\b",
    "\\bboundary\\b",
    "\\bforest cover\\b",
    "\\bvegetation\\b",
    "\\bpasture\\b",
    "\\bagricultur",
    "\\bfarm.*forest\\b",
    sep = "|"
  ),
  
  "Host availability / pressure" = paste(
    "\\babundance\\b",
    "\\bdensity\\b",
    "\\bpopulation size\\b",
    "\\boccupancy\\b",
    "\\bherd size\\b",
    "\\bstocking density\\b",
    "\\bhost density\\b",
    "\\blivestock density\\b",
    "\\bwildlife density\\b",
    sep = "|"
  ),
  
  "Movement / connectivity" = paste(
    "\\bmovement\\b",
    "\\bmovements\\b",
    "\\bdispersal\\b",
    "\\bmigration\\b",
    "\\bconnectivity\\b",
    "\\bgps\\b",
    "\\btelemetry\\b",
    "\\btracking\\b",
    "\\bmovement network\\b",
    "\\bmovement path\\b",
    sep = "|"
  ),
  
  "Pathogen connectivity" = paste(
    "\\bpathogen\\b",
    "\\binfection\\b",
    "\\bprevalence\\b",
    "\\bseroprevalence\\b",
    "\\bsero-prevalence\\b",
    "\\bserolog",
    "\\bantibod",
    "\\bpcr\\b",
    "\\brt-pcr\\b",
    "\\bsequence\\b",
    "\\bgenetic similarity\\b",
    "\\bphylogen",
    "\\bgenomic\\b",
    "\\bstrain\\b",
    "\\bgenotype\\b",
    "\\btransmission\\b",
    "\\bviral\\b",
    "\\bvirus\\b",
    "\\bparasite\\b",
    "\\bbacter",
    sep = "|"
  ),
  
  "Human/management-mediated interface" = paste(
    "\\bmanagement\\b",
    "\\bhusbandry\\b",
    "\\bbiosecurity\\b",
    "\\bvaccin",
    "\\bfeeding\\b",
    "\\bgrazing\\b",
    "\\bfenc",
    "\\bfarming practice",
    "\\bfarm management",
    "\\banimal management",
    "\\bfarmer",
    "\\bowner",
    sep = "|"
  )
)


# ============================================================
# 4. CLASSIFY QUANTITATIVE MEASURES
# ============================================================

classified_measures <- imap_dfr(
  dimension_patterns,
  function(pattern, dimension) {
    
    qm %>%
      filter(
        str_detect(
          classification_text,
          regex(pattern, ignore_case = TRUE)
        )
      ) %>%
      transmute(
        paper_id,
        interface_dimension = dimension,
        measure_name,
        what_it_represents,
        measurement_method
      )
  }
)


# ============================================================
# 5. MINIMAL AUDIT TABLE
# ============================================================
# This preserves the evidence behind every classification.

interface_measure_audit <- classified_measures %>%
  distinct(
    interface_dimension,
    paper_id,
    measure_name,
    what_it_represents,
    measurement_method
  ) %>%
  arrange(interface_dimension, paper_id)


# ============================================================
# 6. COUNT UNIQUE STUDIES
# ============================================================
# IMPORTANT:
# A paper with several measures in the same dimension counts ONCE.

dimension_counts <- classified_measures %>%
  distinct(
    interface_dimension,
    paper_id
  ) %>%
  count(
    interface_dimension,
    name = "N_studies"
  )


# ============================================================
# 7. CLEAN MANUSCRIPT TABLE
# ============================================================

interface_structure <- tribble(
  
  ~interface_dimension,
  ~what_is_characterised,
  ~examples_of_measurements,
  
  "Spatial proximity",
  "How close wild and domestic hosts are",
  "Distance to farms, distance to wildlife habitat, buffers, proximity measures",
  
  "Spatial overlap / co-occurrence",
  "Whether wild and domestic hosts use the same places",
  "GPS overlap, home-range overlap, camera-trap co-occurrence, shared habitat",
  
  "Direct/indirect contact",
  "Actual or inferred encounters between host domains",
  "Contact rates, proximity events, shared feeding or watering sites, observed interactions",
  
  "Landscape/interface structure",
  "Environmental configuration producing or modifying the interface",
  "Habitat edges, fragmentation, land-use composition, farm-forest boundaries",
  
  "Host availability / pressure",
  "Availability or abundance of hosts on either side of the interface",
  "Livestock density, wildlife abundance, herd size, occupancy",
  
  "Movement / connectivity",
  "Movement of hosts across domains or populations",
  "GPS movement, telemetry, dispersal, migration, movement networks",
  
  "Pathogen connectivity",
  "Evidence of infection or pathogen links across host domains",
  "Pathogen prevalence, serology, PCR detection, sequence similarity, genetic evidence",
  
  "Human/management-mediated interface",
  "Human practices that create, alter or manage wild-domestic interactions",
  "Husbandry, grazing, feeding, fencing, vaccination, biosecurity"
)


interface_dimension_summary <- interface_structure %>%
  left_join(
    dimension_counts,
    by = "interface_dimension"
  ) %>%
  mutate(
    N_studies = replace_na(N_studies, 0L)
  )


# ============================================================
# 8. RESULTS
# ============================================================

print(interface_dimension_summary)


# ============================================================
# 9. QUICK AUDIT EXAMPLE
# ============================================================
# If we ever need to see which evidence generated a category:

interface_measure_audit %>%
  filter(interface_dimension == "Spatial proximity")


# ============================================================
# 10. CHECK UNCLASSIFIED MEASURES
# ============================================================
# Important quality-control step:
# these are quantitative measures that did not match any category.

classified_ids <- classified_measures %>%
  distinct(paper_id, measure_name)

unclassified_measures <- qm %>%
  anti_join(
    classified_ids,
    by = c("paper_id", "measure_name")
  ) %>%
  select(
    paper_id,
    measure_name,
    what_it_represents,
    measurement_method,
    role_in_analysis
  )

cat(
  "\nUnclassified quantitative measures:",
  nrow(unclassified_measures),
  "of",
  nrow(qm),
  "\n"
)

View(unclassified_measures)

# Manual review

library(openxlsx)

unclassified_review <- unclassified_measures %>%
  mutate(
    review_id = row_number(),
    manual_interface_dimension = ""
  ) %>%
  select(
    review_id,
    paper_id,
    measure_name,
    what_it_represents,
    measurement_method,
    role_in_analysis,
    manual_interface_dimension
  )

#
write.xlsx(
  unclassified_review,
  "08_34_unclassified_measures_FOR_REVIEW.xlsx",
  overwrite = TRUE
)


# OPEN REVIEWED
manual_review <- read_excel(
  "08_34_unclassified_measures_REVIEWED.xlsx"
)

# After manual review
# Check what you entered
manual_review %>%
  count(manual_interface_dimension, sort = TRUE)


valid_dimensions <- c(
  "Spatial proximity",
  "Spatial overlap / co-occurrence",
  "Direct/indirect contact",
  "Landscape/interface structure",
  "Host availability / pressure",
  "Movement / connectivity",
  "Pathogen connectivity",
  "Human/management-mediated interface",
  "EXCLUDE"
)

# OPEN MANUAL REVIEW

manual_review %>%
  filter(!manual_interface_dimension %in% valid_dimensions)

manual_classified_measures <- manual_review %>%
  filter(manual_interface_dimension != "EXCLUDE") %>%
  transmute(
    paper_id,
    interface_dimension = manual_interface_dimension,
    measure_name,
    what_it_represents,
    measurement_method
  )

classified_measures_final <- bind_rows(
  classified_measures,
  manual_classified_measures
) %>%
  distinct()



dimension_counts_final <- classified_measures_final %>%
  distinct(
    interface_dimension,
    paper_id
  ) %>%
  count(
    interface_dimension,
    name = "N_studies"
  )



interface_dimension_summary_final <- interface_structure %>%
  left_join(
    dimension_counts_final,
    by = "interface_dimension"
  ) %>%
  mutate(
    N_studies = replace_na(N_studies, 0L)
  )

print(interface_dimension_summary_final)


str(classified_measures_final)

interface_dimension_summary_final

study_dimension <- classified_measures_final %>%
  filter(!is.na(interface_dimension)) %>%
  distinct(paper_id, interface_dimension)

N_studies_total <- study_dimension %>%
  distinct(paper_id) %>%
  nrow()

dimension_summary <- study_dimension %>%
  count(interface_dimension, name = "N_studies") %>%
  mutate(
    percent_studies = 100 * N_studies / N_studies_total
  )

dimension_summary
interface_structure

dimension_summary <- dimension_summary %>%
  left_join(
    interface_structure,
    by = "interface_dimension"
  )

dimension_summary

openxlsx::write.xlsx(
  dimension_summary,
  file = file.path(
    results_dir,
    "Table_interface_dimension_summary.xlsx"
  ),
  overwrite = TRUE
)

openxlsx::write.xlsx(
  classified_measures_final,
  file = file.path(
    results_dir,
    "Supplementary_table_classified_measures_final.xlsx"
  ),
  overwrite = TRUE
)

#-------------------------------------------------------------------------------

