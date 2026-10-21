# Publication metadata extraction
# R. Muylaert 2026
#-------------------------------------------------------------------------------

rm(list = ls())
gc()

packages <- c("rcrossref", "dplyr", "purrr", "stringr", "pdftools", "here", "openxlsx")

to_install <- packages[!packages %in% installed.packages()[, "Package"]]

if (length(to_install) > 0) install.packages(to_install)

lapply(packages, library, character.only = TRUE)

setwd(here())

input_df <- read.xlsx('master/master_table.xlsx', sheet = 2)

input_df$DOI 

extract_publication_metadata <- function(id, doi) {
  
  cr <- tryCatch(
    rcrossref::cr_works(dois = doi)$data,
    error = function(e) NULL
  )
  
  if (is.null(cr)) {
    return(tibble::tibble(
      ID = id,
      DOI = doi,
      Status_published = NA,
      Type_of_publication = NA,
      Reference_count = NA,
      Number_of_citations = NA,
      Authors_sep_by_semicolon = NA,
      Number_of_authors = NA,
      Year = NA,
      metadata_confidence = "missing"
    ))
  }
  
  authors_df <- cr$author[[1]] # get from sub df 
  
  authors_string <- if (!is.null(authors_df)) {
    paste(paste(authors_df$given, authors_df$family), collapse = ";") #given first, family name last
  } else NA_character_
  
  year <- substr(cr$issued,1,4)
  
  tibble::tibble(
    ID = id,
    DOI = doi,
    
    Status_published = cr$status,
    Type_of_publication = cr$type,
  
    Number_of_citations = cr$is.referenced.by.count,
    Reference_count = cr$reference.count,
    Authors_sep_by_semicolon = authors_string,
    
    
    Number_of_authors = if (!is.null(authors_df)) nrow(authors_df) else NA,
    Year = year,
    

    metadata_confidence = if (!is.na(year) && !is.na(authors_string)) "high" else "low"
  )
}


# Results

results <- pmap_dfr(
  list(input_df$ID, input_df$DOI),
  extract_publication_metadata
)


# Flag check -------------------------------------------------------------------

#pmap_dfr(list(ID, DOI, pdf_path), extract_publication_metadata)


setwd(here())

nrow(results)

setwd('results')

diff_input <- input_df %>%
  filter(!(ID %in% results$ID)) %>%
  select(ID, Author, DOI)

# The differences are for links that are not DOIs, preprints, and non valid DOIS - non-traceable articles 

diff_input

# posted content is preprint

results %>%
  count(Type_of_publication, sort = TRUE) %>%
  mutate(Percent = round(100 * n / sum(n), 1))

nrow(input_df)

nrow(input_df) - nrow(diff_input)

#openxlsx::write.xlsx(results, file = "article_bibmap.xlsx", overwrite = TRUE)

#-------------------------------------------------------------------------------