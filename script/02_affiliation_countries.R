# Topics, funders, authors and institutional afiliations - countries

packages <- c('Manu',
  "here",
  "openalexR",
  "dplyr",
  "purrr",
  "openxlsx",
  "tidyr",
  "stringr",
  "fuzzyjoin",
  "treemapify",
  "devtools",
  "ggraph",
  "ggrepel",
  "igraph",
  "rcrossref",
  "rJava",
  "xlsx"
)

library("wesanderson")

# Install any packages that are not already installed
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))

# Set up
setwd(here())
getwd()

input_df <- read.xlsx('master/master_table.xlsx', sheetName = "Records")

# Check for Duplicate DOI (may happen with book chapters from the same book)
# 3 NA and some links are not DOIS

input_df %>%
  count(DOI, sort = TRUE) %>%
  filter(n > 1)

# clean

clean_doi <- function(x) {
  x %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace("^https?://doi.org/", "") %>%
    stringr::str_replace("^doi:", "")
}

input_df <- input_df %>%
  mutate(doi_clean = clean_doi(DOI))

# oa_fecth does 50 at a time
# oa_fetch()` and `oa2df()` now return new names for some columns in openalexR v2.0.0.

works_from_dois <- oa_fetch(
  entity = "works",
  doi = input_df$doi_clean,
  verbose = TRUE
)

nrow(works_from_dois)

table(table(works_from_dois$doi))

works_from_dois <- works_from_dois %>%
  mutate(doi_clean = clean_doi(doi))

# Get ID
works_from_dois <- works_from_dois %>%
  left_join(
    input_df %>% select(ID, doi_clean),
    by = "doi_clean"
  )

nrow(works_from_dois)

# Check if there are dups
works_from_dois %>%
  count(doi, sort = TRUE) %>%
  filter(n > 1)

works_df <- works_from_dois %>%
  transmute(
    doi = doi,
    title = display_name,
    publication_year = publication_year,
    journal = display_name
  )


table(table(input_df$ID))

missing_dois <- setdiff(
  input_df$doi_clean,
  works_from_dois$doi_clean
)

length(missing_dois)

# 252 traceable -  others $doi invalid, not indexed
# Now 254 traceable it seems

270-16

colnames(works_from_dois)


works_from_dois$display_name

# Get keywords for each paper
c(works_from_dois$ID, works_from_dois$keywords['display_name'])


str(works_from_dois$keywords)
summary(works_from_dois$keywords)

# Funders
colnames(works_from_dois)
works_from_dois$topics[[1]]

works_from_dois$topics[[1]] %>%
  filter(type == "topic") %>%
  pull(display_name)

# Use type from Openalex 
works_from_dois$type


# max score for just subsetting one topic

works_from_dois$topics[[1]] %>%
  filter(type == "topic") %>%
  slice_max(score, n = 10, with_ties = FALSE) %>%
  pull(display_name)


top_topics <- map_dfr(seq_len(nrow(works_from_dois)), \(i) {
  
  topics_i <- works_from_dois$topics[[i]]
  
  # skip if NULL or empty
  if (is.null(topics_i) || nrow(topics_i) == 0) return(NULL)
  
  topics_i %>%
    filter(type == "topic") %>%
    slice_max(score, n = 1, with_ties = FALSE) %>%
    transmute(
      ID = works_from_dois$ID[i],
      doi = works_from_dois$doi[i],
      top_topic = display_name,
      score = score
    )
})

# Now get for all papers the top scoring topic
top_topics <- map_dfr(seq_len(nrow(works_from_dois)), function(i) {
  
  topics_i <- works_from_dois$topics[[i]]
  
  # handle missing / empty
  if (is.null(topics_i) || nrow(topics_i) == 0) {
    return(NULL)
  }
  
  topics_i <- topics_i[topics_i$type == "topic", ]
  
  if (nrow(topics_i) == 0) {
    return(NULL)
  }
  
  best <- topics_i[which.max(topics_i$score), ]
  
  data.frame(
    ID = works_from_dois$ID[i],
    doi = works_from_dois$doi[i],
    top_topic = best$display_name,
    score = best$score
  )
})


nrow(top_topics)

# export 
# write.xlsx(top_topics, file = "topics.xlsx")
colnames(top_topics)
t_counts <- top_topics %>%
  count(top_topic, sort = TRUE)


tfig <- ggplot(t_counts, aes(area = n, label = top_topic, fill = n)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", reflow = TRUE) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("FrenchDispatch")) +
  labs(
    title = "",
    fill = "Count"
  ) +
  theme_minimal()

tfig

#export

ggsave( filename = "fig_topics.tif",
        plot = tfig,
        width = 18,
        height = 13,
        dpi = 300)


library(dplyr)


topic_table <- top_topics %>%
  count(top_topic, sort = TRUE) %>%
  mutate(
    Percentage = scales::percent(n / sum(n), accuracy = 0.1)
  )

topic_table

setwd('results')

write.xlsx(topic_table, file = "topic_table.xlsx")

# Funders

works_from_dois$funders[[1]]$display_name 

get_funders <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  
  if (is.data.frame(x)) return(x$display_name)
  if (is.atomic(x)) return(as.character(x))
  
  return(unlist(lapply(x, function(y) y$display_name)))
}


fund <- map_dfr(seq_len(nrow(works_from_dois)), function(i) {
  
  ff <- get_funders(works_from_dois$funders[[i]])
  
  if (is.null(ff)) return(NULL)
  
  data.frame(
    ID = works_from_dois$ID[i],
    doi = works_from_dois$doi[i],
    doi_clean = works_from_dois$doi_clean[i],
    funder = ff
  )
})

nrow(fund)
nrow(distinct(fund))

fund <- fund %>%
  dplyr::filter(!is.na(funder)) 


head(fund)

f_counts <- fund %>%
  count(funder, sort = TRUE) %>%  glimpse()

f_counts

# Check pals

get_pal("Kereru")[c(1:3)]
c("#325756", "#7d9fc2", "#C582B2", "#51806a", "#4d5f8e", "#A092B7")


fundfig <- ggplot(f_counts, aes(area = n, label = funder, fill = n)) +
  geom_treemap() +
  geom_treemap_text(colour = "gray90", place = "centre", reflow = TRUE) +
  #scale_fill_gradientn(colours = wesanderson::wes_palette("BottleRocket1")) +
  #scale_colour_brewer(palette = "Set1")+
  #scale_fill_viridis(option = "Accent", direction = 1) + #rocket
  scale_fill_gradientn(colours =get_pal("Kereru")[c(1:3)] )+
  labs(title = "", fill = "Count") +
  theme_minimal()

fundfig

# Export
#write.xlsx(fund, file = "funders.xlsx")
#write.xlsx(f_counts, file = "funders_count.xlsx")

ggsave( filename = "fig_funders.tif",
        plot = fundfig,
        width = 18,
        height = 13,
        dpi = 300)


# Keywords df

kw <- map_dfr(seq_len(nrow(works_from_dois)), function(i) {
  
  kws <- works_from_dois$keywords[[i]]
  
  # handle cases with no keywords
  if (is.null(kws) || nrow(kws) == 0) {
    return(NULL)
  }
  
  data.frame(
    ID = works_from_dois$ID[i],
    doi = works_from_dois$doi[i],
    doi_clean = works_from_dois$doi_clean[i],
    keyword = kws$display_name
  )
})

# Export

library(dplyr)
library(stringr)
library(ggplot2)
library(ggwordcloud)

kw_freq <- kw %>%
  mutate(
    keyword = str_to_lower(str_trim(keyword))
  ) %>%
  filter(keyword != "") %>%
  count(keyword, sort = TRUE)

# 3 or more 

kw_freq <- kw %>%
  mutate(keyword = str_to_lower(str_trim(keyword))) %>%
  count(keyword, sort = TRUE) %>%
  filter(n >= 3)

keyword_cloud <- ggplot(kw_freq,
       aes(label = keyword,
           size = n,
           colour = n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 18) +
  scale_colour_gradientn(
    colours = wesanderson::wes_palette("FrenchDispatch")
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

keyword_cloud

# Export keyword analysis

write.xlsx(kw, file = "keywords.xlsx")

# Count

kw_counts <- kw %>%
  count(keyword, sort = TRUE)

head(kw_counts)

kwfig <- ggplot(kw_counts, aes(area = n, label = keyword, fill = n)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", reflow = TRUE) +
  scale_fill_viridis(option = "rocket", direction = 1) + #rocket
  labs(
    title = "",
    fill = "Count"
  ) +
  theme_minimal()

getwd()


kwfig

ggsave( filename = "fig_keywords.tif",
        plot = kwfig,
        width = 18,
        height = 13,
        dpi = 300)

# Export wordcloud
ggsave(  filename = here::here("results", "keyword_cloud.png"),
  plot = keyword_cloud,
  width = 10,
  height = 8,
  units = "in",
  dpi = 600,
  bg = "white"
)


authors_df <- purrr::map_dfr(seq_len(nrow(works_from_dois)), function(i) {
  
  auths <- works_from_dois$authorships[[i]]
  doi_i <- works_from_dois$doi[i]
  
  if (is.null(auths) || nrow(auths) == 0) {
    return(tibble(
      doi = doi_i,
      author_name = NA_character_,
      institution = NA_character_,
      country = NA_character_
    ))
  }
  
  tibble(
    doi = doi_i,
    author_name = auths$display_name,
    institution = purrr::map_chr(auths$affiliations, ~ if (nrow(.x) > 0) .x$display_name[1] else NA_character_),
    country = purrr::map_chr(auths$affiliations, ~ if (nrow(.x) > 0) .x$country_code[1] else NA_character_)
  )
})


authors_df

# ------------------------------------------------------------------------------

# Author inspection
equire(janitor)

unique(authors_df$author_name)

authors_df %>%
  tabyl(author_name)

agrep(" Davies M. Pfukenyi", authors_df$author_name, value = TRUE)

length(unique(authors_df$author_name))

name_check <- unique(authors_df$author_name)

name_check

# Author name check

authors_df[which(authors_df$author_name == 'Dae-sung Yu'), 'doi']

authors_df[which(authors_df$author_name == 'Dae‐Sung Yoo'), 'doi']

authors_df[which(authors_df$author_name == 'Wooseg Jeong'), 'doi']

authors_df[which(authors_df$author_name == 'Wooseog Jeong'), 'doi']


# To identify and correct inconsistencies in author names, we implemented a multi‑step cleaning and fuzzy‑matching workflow in R. First, all author names were normalized, converting text to lowercase, trimming whitespace, collapsing multiple spaces, standardizing hyphens, and removing periods. This produced a canonical clean version of each name for comparison: 

authors_df <- authors_df %>%
  mutate(
    clean = author_name |> 
      str_to_lower() |> 
      str_trim() |> 
      str_replace_all("\\s+", " ") |> 
      str_replace_all("–|—", "-") |> 
      str_replace_all("\\.", "")
  )


library(stringdist)


# To detect potential duplicate authors arising from minor spelling differences, hyphenation inconsistencies, or typographical errors, we computed a pairwise Levenshtein distance matrix using the stringdist package:
dist_mat <- stringdistmatrix(authors_df$clean, authors_df$clean, method = "lv")
dist_mat

# Pairs of names with a Levenshtein distance of 1 (i.e., differing by a single character insertion, deletion, or substitution) were flagged as potential duplicates. Self‑matches (distance = 0) were excluded: 
possible_dupes <- which(dist_mat <= 1 & dist_mat > 0, arr.ind = TRUE)

dupe_pairs <- data.frame(
  name1 = authors_df$author_name[possible_dupes[,1]],
  name2 = authors_df$author_name[possible_dupes[,2]],
  dist  = dist_mat[possible_dupes]
)

dupe_pairs  
nrow(dupe_pairs) # 48 flags

dupe_pairs <- dupe_pairs %>%
  # Step 1: alphabetize each pair internally
  rowwise() %>%
  mutate(
    sorted = list(sort(c(name1, name2))),
    name1 = sorted[[1]],
    name2 = sorted[[2]]
  ) %>%
  ungroup() %>%
  select(-sorted) %>%
  # Step 2: sort the full table alphabetically
  arrange(name1, name2) %>% distinct()

# The full set of flagged pairs was then sorted alphabetically and deduplicated:

dupe_pairs

#---------------------------
# Author inspection

# The resulting table provided a concise list of author‑name that could be variants but that is not verified. These flagged pairs were subsequently used to guide manual standardization of author names in the dataset.
authors_df[which(authors_df$author_name == "Andrés M. Perez"), 'author_name']

authors_df[which(authors_df$author_name == "Antonio Rivero-Juárez"), 'author_name']

authors_df[which(authors_df$author_name == "Ferrán Jori"), 'author_name']

authors_df[which(authors_df$author_name == "María Á. Risalde"), 'author_name']

authors_df[which(authors_df$author_name == "María A. Risalde "), 'author_name']

authors_df[which(authors_df$author_name == "Meng Li"), 'author_name']

authors_df[which(authors_df$author_name == "Meng Lu"), 'author_name']

#------------------------
# Maps 
library(rnaturalearth)
library(sf)
library(ggplot2)

world <- ne_countries(scale = "medium", returnclass = "sf")

country_counts <- authors_df %>%
  filter(!is.na(country)) %>%
  distinct(author_name, country) %>%
  count(country)

country_counts
# normalize
country_counts <- country_counts %>%
  mutate(z = scale(n)[,1])

world_map <- world %>%
  left_join(country_counts, by = c("iso_a2" = "country"))

world_map$name

labels <- world_map %>%
  filter(n > 0) %>%
  st_point_on_surface() %>%   # better than centroid for weird shapes
  mutate(label = paste0(name, "\nN=", n))

labels$name

ggplot(world_map) +
  geom_sf(aes(fill = z), color = "gray90", linewidth = 0.1) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "maroon",
    midpoint = 0,
    na.value = "white"
  ) +
  geom_text_repel(
    data = labels,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    size = 4,
    min.segment.length = 0
  ) +
  theme_minimal() +
  labs(
    title = "Author distribution",
    fill = "Deviation"  )

country_counts <- authors_df %>%
  group_by(country, doi) %>%
  distinct(author_name) %>%
  summarise(authors = n(), .groups = "drop") %>%
  group_by(country) %>%
  summarise(
    total_authors = sum(authors),
    n_papers = n_distinct(doi),
    authors_per_paper = total_authors / n_papers
  )

country_counts <- country_counts %>%
  mutate(author_share = total_authors / sum(total_authors))


world_map <- world %>%
  left_join(country_counts, by = c("iso_a2" = "country"))

world_map$authors_per_paper

world_map$total_authors


#------------------

# Map of gaps
sum(table(world_map$n_papers))

world_map <- world_map %>%
  mutate(
    gap = ifelse(n_papers == 0, 1, 0)
  )

nrow(world_map) - table(world_map$gap)

gap_labels <- world_map %>%
  filter(is.na(n_papers)) %>%
  st_point_on_surface() %>%
  mutate(label = name)


fig_gaps <- ggplot(world_map) +
  geom_sf(aes(fill = factor(gap)), color = "white", linewidth = 0.1) +
  scale_fill_manual(
    values = c("0" = "gray90",  # has data (blue)
               "1" = "white"), # does not make sense so dont bother
    labels = c("Covered", "No authorship"),
    na.value = "lightskyblue" ) +
  geom_text_repel(
    data = gap_labels,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    size = 4,
    max.overlaps = 50
  ) +
  theme_minimal() +
  labs(
    title = "Authorship gaps",
    fill = ""
  )

fig_gaps

ggsave( filename = "Fig_gaps.png",
        plot = fig_gaps,
        width = 20,
        height = 13,
        dpi = 300)

#participation weight in knowledge production
#Country-level fractional contribution to authorship across 252 publications, 
# where each publication contributes one unit of weight distributed across contributing countries
#The United States accounts for most paper-equivalents
# Fractional contribution
# Each paper contributes 1 unit total, split across countries.
# each paper = 1 unit
# no inflation from team size
# corrects for multinational papers
# standard in bibliometrics

authors_df$country
sum(world_map$total_fraction, na.rm = T)

country_fractional <- authors_df %>%
  distinct(doi, country, author_name) %>%
  count(doi, country, name = "n_authors") %>%
  group_by(doi) %>%
  mutate(frac = n_authors / sum(n_authors)) %>%
  ungroup() %>%
  group_by(country) %>%
  summarise(total_fraction = sum(frac), .groups = "drop")

sum(country_fractional$total_fraction)



world_map <- world %>%
  left_join(country_fractional, by = c("iso_a2" = "country"))

labelsf <- world_map %>%
  filter(total_fraction > 0) %>%
  st_point_on_surface() %>%   # better than centroid for weird shapes
  mutate(label = paste0(name, "\n", " ", round(total_fraction,2) ))

labelsf

country_fractional

world_map$total_fraction

labels$label


fig_paper_eq <- ggplot(world_map) +
  geom_sf(aes(fill = total_fraction), color = "gray90", linewidth = 0.1) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "gray90",
    high = "maroon",
    midpoint = 0,
    na.value = "white"
  ) +   geom_text_repel(  data = labelsf,
                          aes(label = label, geometry = geometry),
                          stat = "sf_coordinates",
                          size = 4,
                          min.segment.length = 0
  )+
  theme_minimal() +
  labs( title = "Institutional weighted contribution per country",
        fill = "Paper-equivalents"   )


fig_paper_eq

ggsave( filename = "Fig_paper_eq.png",
        plot = fig_paper_eq,
        width = 20,
        height = 13,
        dpi = 300)


#--------------
# Merge author position to author cleaned names!!!! Not today...

works_from_dois$authorships[[2]]

team_growth_fig <- ggplot(
  df,
  aes(x = Year, y = Number_of_authors)
) +
  geom_jitter(
    width = 0.20,
    height = 0,
    alpha = 0.55,
    size = 2.5,
    colour = "maroon"
  ) +
  labs(
    x = "Publication year",
    y = "Number of authors"
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "black") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      linewidth = 0.3,
      colour = "grey90"
    ),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    plot.margin = margin(5, 5, 5, 5)
  )

team_growth_fig

cor.test(df$Year, df$Number_of_authors )


# ============================================================
# PANEL B — Institutional contribution by country
# ============================================================

labelsf <- world_map %>%
  filter(!is.na(total_fraction)) %>%
  slice_max(
    order_by = total_fraction,
    n = 10,
    with_ties = FALSE
  ) %>%
  mutate(
    label = paste0(
      name, "\n",
      round(total_fraction, 1)
    )
  )

fig_paper_eq <- ggplot(world_map) +
  geom_sf(
    aes(fill = total_fraction),
    colour = "grey90",
    linewidth = 0.1
  ) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "grey90",
    high = "maroon",
    midpoint = 0,
    na.value = "white"
  ) +
  geom_text_repel(
    data = labelsf,
    aes(
      label = label,
      geometry = geometry
    ),
    stat = "sf_coordinates",
    size = 3,
    min.segment.length = 0
  ) +
  labs(
    fill = "Paper-equivalents"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.margin = margin(5, 5, 5, 5)
  )

# ============================================================
# COMBINE PANELS
# ============================================================
library(patchwork)

Fig_team_geography <- patchwork::wrap_plots(
  team_growth_fig,
  fig_paper_eq,
  ncol = 1,
  heights = c(0.8, 1.4)
) +
  patchwork::plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(
        face = "bold",
        size = 14
      )
    )
  )

Fig_team_geography



ggsave( "Fig_team_geography.jpg",
  Fig_team_geography,
  width = 170,
  height = 200,
  units = "mm",
  dpi = 600,
  bg = "white"
)
#-------------------------------------------------------------------------------