#Figures - trends from metadata
# R. Muylaert 2026
#-------------------------------------------------------------------------------

rm(list = ls())
gc()

library(rcrossref)
library(dplyr)
library(purrr)
library(stringr)
library(pdftools)
library(here)
library(openxlsx)
require(ggrepel)
require(ggplot2)

# Open 
setwd(here())

setwd('results')


df <- openxlsx::read.xlsx("article_bibmap.xlsx")

# PDF was not in folder now it is

df[df$ID==995,]

# Entire book generate unfair stats - removed from bibap

t(df[df$ID==869,])

# Remove 

nrow(df)

# Publication label 
df$label <- sapply(seq_len(nrow(df)), function(i) {
  
  # Split authors at semicolons
  authors <- strsplit(
    as.character(df$Authors_sep_by_semicolon[i]),
    ";"
  )[[1]]
  
  authors <- trimws(authors)
  
  # Extract surname as last word of each author name
  surnames <- sapply(
    strsplit(authors, "\\s+"),
    function(x) tail(x, 1)
  )
  
  # Clean year
  year <- sub(
    ".*?(\\d{4})\\D*$",
    "\\1",
    as.character(df$Year[i])
  )
  
  # Create citation-style label
  if (length(surnames) == 1) {
    paste(surnames[1], year)
    
  } else if (length(surnames) == 2) {
    paste(surnames[1], "&", surnames[2], year)
    
  } else {
    paste(surnames[1], "et al.", year)
  }
})


df$label


pubs_per_year <- df %>%
  count(Year)

# Join with t
head(df)
head(t)

df$id <-paste0('a', df$ID)
 

df <- df %>%
  left_join(t, by = "id")

head(df)



ggplot(pubs_per_year, aes(x = Year, y = n)) +
  geom_col() +
  geom_smooth(se = FALSE, method = "loess", linewidth = 0.8) +
  labs(x = "Year",
       y = "Number of publications",
       title = "") +
  theme_minimal(base_size = 14) + geom_vline(xintercept = 2020, linetype = "dashed")

#
head(df)

df$Number_of_citations <- as.numeric(df$Number_of_citations )
  
table(df$Number_of_citations )                             

cit_year <- df %>%
  group_by(Year) %>%
  summarise(mean_citations = mean(as.numeric(Number_of_citations), na.rm = TRUE))

ggplot(cit_year, aes(x = Year, y = mean_citations)) +
  geom_line() +
  geom_point(size=4, col='maroon') +
  labs(x = "Year",
       y = "Mean citations",
       title = "") +
  theme_minimal(base_size = 14)

summary(df$Number_of_citations)

current_year <- 2026

df$Year <- as.integer(df$Year)

min(df$Year)

df <- df %>%
  mutate(years_since_pub = current_year - Year,
         citations_per_year = Number_of_citations / pmax(years_since_pub, 1))

min(df$citations_per_year)

summary(df$citations_per_year)
plot(df$citations_per_year, df$Number_of_citations)

head(df)


ggplot(df, aes(x = Year, y = citations_per_year)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(x = "Year",
       y = "Citations per year",
       title = "Citation rate over time") +
  theme_minimal(base_size = 14)

# Increase in teams - Panel A

cor.test(df$Year, df$Number_of_authors )

ggplot(df, aes(x = Year, y = Number_of_authors)) +
  geom_jitter(width = 0.2, alpha = 0.6, , size=4, col='maroon') +
  #geom_smooth(se = FALSE, method = "loess") +
  labs(x = "Year",
       y = "Number of authors",
       title = "") +
  theme_minimal(base_size = 16)


# Report the change in predicted team size over time
cor.test(df$Year,
         df$Number_of_authors,
         method = "spearman")

# Decades is not representative of the true growth
df$Period <- cut(df$Year,
                 breaks = c(1990, 2000, 2010, 2020, 2030))

aggregate(Number_of_authors ~ Period,
          data = df,
          mean)

# 5 year breaks

table_growth <- df %>%
  mutate(Period = paste0(
    floor(Year / 5) * 5, "-",
    floor(Year / 5) * 5 + 4
  )) %>%
  group_by(Period) %>%
  summarise(
    Mean_authors = mean(Number_of_authors, na.rm = TRUE),
    n = n()
  )
table_growth

openxlsx::write.xlsx(
  table_growth,
  "Table_authorship_growth_5year.xlsx",
  rowNames = FALSE
)

table_growth
# Add CIs: but the field did not exist in the 90s so not fair to hold on to them!

summary_tab <- df %>%
  group_by(Period) %>%
  summarise(
    mean = mean(Number_of_authors),
    n = n(),
    sd = sd(Number_of_authors),
    se = sd/sqrt(n),
    lower = mean - 1.96*se,
    upper = mean + 1.96*se
  )

ggplot(summary_tab,
       aes(x = Period, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = 0.2) +
  theme_minimal()


#
ggplot(df, aes(x = Number_of_authors, y = as.numeric(Number_of_citations)  )) +
  geom_point(alpha = 0.6, size=4, col='maroon') +
  geom_smooth(se = FALSE, method = "lm", col='black') +
  geom_text_repel(    data = subset(df, as.numeric(Number_of_citations) > 30),
                      aes(label = label),   # change this if you prefer another label
                      size = 4,
                      fontface = "italic",
                      max.overlaps = Inf
  ) +scale_y_log10() +
  labs(x = "Number of authors",
       y = "Citations (log scale)",
       title = "Collaboration size vs citation impact") +
  theme_minimal(base_size = 14)

# Year adjusted
summary(df$citations_per_year)

cor.test(df$Number_of_authors,  as.numeric(df$Number_of_citations) )

#Panel a - more than 100 citations per year lab

cca <- ggplot(df, aes(x = Number_of_authors, y = as.numeric(citations_per_year)  )) +
  geom_point(alpha = 0.6, size=4, col='maroon') +
  geom_smooth(se = FALSE, method = "lm", col='black', lty=2) +
  geom_text_repel(    data = subset(df, as.numeric(citations_per_year) > 30),
                      aes(label = label),   # change this if you prefer another label
                      size = 4,
                      fontface = "italic",
                      max.overlaps = Inf ) +
  scale_y_log10() +
  labs(x = "Number of authors",
       y = "Year-adjusted citations (log scale)",
       title = "") +
  theme_minimal(base_size = 14)

cca

ggsave( filename = "Fig_year_collab_citation_adjusted.tiff",
  plot = cca,
  width = 24,
  height = 16,
  units = "cm",
  dpi = 600,
  compression = "lzw",
  bg = "white" )


# adjusted for year

head(df)

str(df$Reference_count)

cor.test(as.numeric(df$Reference_count), df$citations_per_year)

# Panel b - more than 100 citations per year lab

ccr <- ggplot(df, aes(x = as.numeric(Reference_count),
               y = as.numeric(citations_per_year))) +
  geom_point(alpha = 0.6, size = 4, col = "maroon") +
  
  geom_smooth(se = FALSE, method = "lm", col = "black") +
  geom_text_repel(    data = subset(df, as.numeric(citations_per_year) > 30),
                      aes(label = label),   # change this if you prefer another label
                      size = 4,
                      fontface = "italic",
                      max.overlaps = Inf ) +
    scale_y_log10() +
  
  labs(x = "Reference count",
       y = "Year-adjusted citations (log scale)",
       title = "") +
  theme_minimal(base_size = 14)

ccr


# Citations as a function of interface mentions
max(df$interface_total)

max(df$interface_main_text)

max(df$interface_lit)

#
df$citations_per_year

cor.test( as.numeric(df$interface_main_text),as.numeric(df$citations_per_year) )

plot_citations_interface <- ggplot(df, aes(x = as.numeric(interface_main_text),
               y = as.numeric(citations_per_year))) +
  geom_point(alpha = 0.6, size = 4, col = "maroon") +
  
  geom_smooth(se = FALSE, method = "lm", col = "black") +
  
  geom_text_repel(    data = subset(df, as.numeric(citations_per_year) > 30),
                      aes(label = label),   # change this if you prefer another label
                      size = 4,
                      fontface = "italic",
                      max.overlaps = Inf
  ) +
  scale_y_log10() +
  
  labs(x = "Mention to Interface in the main text",
       y = "Year-adjusted citations (log scale)",
       title = "") +
  theme_minimal(base_size = 14)


plot_citations_interface

#
head(df)

# all mentions
df$interface_total

df$citations_per_year
cit_interface_entire <- ggplot(df, aes(x = as.numeric(citations_per_year),
               y = as.numeric(Number_of_citations))) +
  geom_point(alpha = 0.6, size = 4, col = "maroon") +
  
  geom_smooth(se = FALSE, method = "lm", col = "black") +
  geom_text_repel(    data = subset(df, as.numeric(citations_per_year) > 30),
                      aes(label = label),   # change this if you prefer another label
                      size = 4,
                      fontface = "italic",
                      max.overlaps = Inf
  ) +
  scale_y_log10() +
  
  labs(x = "Mention to Interface in the entire publication",
       y = "Citations (log scale)",
       title = "") +
  theme_minimal(base_size = 14)

cit_interface_entire


# Citations per year citations_per_year

# References only
class(df$interface_main_text)
df$citations_per_year
df$interface_lit <- df$interface_total - df$interface_main_text

subset(df, as.numeric(interface_lit) > 200)

ggplot(df, aes(x = interface_lit,
               y = as.numeric(citations_per_year))) +
  
  geom_point(alpha = 0.6, size = 4, col = "maroon") +
  
  geom_smooth(se = FALSE, method = "lm", col = "black") +
  
  geom_text_repel(    data = subset(df, as.numeric(interface_lit) > 200),
                      aes(label = label),   # change this if you prefer another label
                      size = 4,
                      fontface = "italic",
                      max.overlaps = Inf
  ) +
  scale_y_log10() +
  labs(x = "Mention to Interface in the Literature cited",
       y = "Citations (log scale)",
       title = "") +
  theme_minimal(base_size = 14)



df[df$ID==995, 'interface_lit']
#Outlier

df[df$ID==888, 'interface_lit']

str(df)

  
df[which.max(df$interface_lit),]

ggplot(df, aes(x = interface_lit,
               y = Number_of_citations)) +
  
  geom_point(alpha = 0.6, size = 4, col = "maroon") +
  
  geom_smooth(se = FALSE, method = "lm", col = "black") +
  
  geom_text_repel(    data = subset(df, interface_lit > 80),
                      aes(label = label),   # change this if you prefer another label
                      size = 4,
                      fontface = "italic",
                      max.overlaps = Inf
  ) +
  #scale_y_log10() +
   labs(x = "Mention to Interface in the Literature cited",
       y = "Citations (log scale)",
       title = "") +
  theme_minimal(base_size = 14)

# Buzz effect analysis without outliers
head(df)

dfn <- df %>% filter(flag_high == FALSE)


ggplot(dfn, aes(x = interface_lit,
               y = Number_of_citations)) +
  
  geom_point(alpha = 0.6, size = 4, col = "maroon") +
  
  geom_smooth(se = FALSE, method = "lm", col = "black") +
  
  geom_text_repel(    data = subset(dfn, interface_lit > 50),
                      aes(label = label),   # change this if you prefer another label
                      size = 4,
                      fontface = "italic",
                      max.overlaps = Inf
  ) +
  #scale_y_log10() +
  labs(x = "Mention to Interface in the Literature cited",
       y = "Citations (log scale)",
       title = "") +
  theme_minimal(base_size = 14)


# IDs plotted
head(dfn)

ggplot(dfn, aes(x = interface_lit,
                    y = Number_of_citations,
                    label = label )) +
  geom_point()+
  geom_text_repel(size = 5, color = "black") +
  labs(x = 'Interface mentions in references',
       y = "Citations received",
       title = "") +
  geom_smooth(se = FALSE, method = "lm", col = "black") +
  geom_text_repel(    data = subset(dfn, interface_lit > 50),
                      aes(label = label),   # change this if you prefer another label
                      size = 4,
                      fontface = "italic",
                      max.overlaps = Inf
  )+
  theme_minimal(base_size=14)


#
nrow(df)
is.na(df$interface_main_text)

t(df[248,])


boxplot(df$citations_per_year)

# Add type characteristics
df$Type_of_publication
df$Number_of_citations
mean(df$citations_per_year)


pubtype_citations <- df %>%
  filter(
    !is.na(Type_of_publication),
    !is.na(citations_per_year)
  ) %>%
  group_by(Type_of_publication) %>%
  summarise(
    n = n(),
    mean_citations = mean(citations_per_year, na.rm = TRUE),
    median_citations = median(citations_per_year, na.rm = TRUE),
    sd_citations = sd(citations_per_year, na.rm = TRUE),
    se = sd_citations / sqrt(n),
    .groups = "drop"
  )

pubtype_citations


fig_pubtype <- ggplot(
  pubtype_citations,
  aes(
    x = reorder(Type_of_publication, mean_citations),
    y = mean_citations
  )
) +
  geom_col(
    fill = "maroon",
    alpha = 0.8,
    width = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin = mean_citations - se,
      ymax = mean_citations + se
    ),
    width = 0.15
  ) +
  geom_text(
    aes(
      label = paste0("n = ", n),
      y = mean_citations
    ),
    hjust =1, vjust=2,
    colour = "black",
    size = 3.8,
    fontface = "italic"
  ) +
  coord_flip() +
  labs(
    x = "Publication type",
    y = "Mean citations per year"
  ) +
  theme_minimal(base_size = 14)

fig_pubtype

##
fig_citations4 <- patchwork::wrap_plots(
  cca,
  ccr, plot_citations_interface, fig_pubtype,
  ncol = 2 
) +
  patchwork::plot_annotation(tag_levels = "A")
fig_citations4


# TIFF — publication quality
ggsave(  filename = "Figure_citation_patterns.tiff",
  plot = fig_citations4,
  width = 12,
  height = 10,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

# JPEG — convenient version
ggsave(
  filename = "Figure_citation_patterns.jpg",
  plot = fig_citations4,
  width = 12,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)

#
ggplot(df, aes(x = as.numeric(Number_of_authors) )) +
  geom_histogram(bins = 20) +
  labs(x = "Number of authors",
       y = "Frequency",
       title = "") +
  theme_minimal(base_size = 14)


#
nrow(df)
df$Type_of_publication

df %>%   filter(Type_of_publication == "book-chapter") %>% 
  summarise(mean_citations = mean(Number_of_citations, na.rm = TRUE))

df %>%   filter(Type_of_publication == "journal-article") %>% 
  summarise(mean_citations = mean(Number_of_citations, na.rm = TRUE))

37/9

df %>%  filter(Type_of_publication == "posted-content") # Number_of_citation == 0,


table(df$DOI)

setwd(here())

setwd('master')

input_df <- openxlsx::read.xlsx('master_table.xlsx', sheet=2)


table(input_df$Journal)


diff_input <- input_df %>%
  filter(!(ID %in% df$ID)) %>%
  select(ID, Author, Journal, DOI)

diff_input$Journal
nrow(diff_input)


ggplot(df, aes(x = as.numeric(Number_of_citations) ) ) +
  geom_histogram(bins = 20) +
  scale_x_log10() +
  labs(x = "Citations (log scale)",
       y = "Frequency",
       title = "") +
  theme_minimal(base_size = 14)

#
df <- df %>%
  mutate(period = case_when(
    Year < 2015 ~ "Pre-2015",
    Year < 2020 ~ "2015–2019",
    TRUE ~ "2020+"
  ))

df$period <- factor(df$period,
                    levels = c("Pre-2015", "2015–2019", "2020+"))
comparisons <- list(
  c("Pre-2015", "2015–2019"),
  c("Pre-2015", "2020+"),
  c("2015–2019", "2020+")
)
# Buzz word post pandemic?

ggplot(df, aes(x = period, y = as.numeric(Number_of_citations) )) +
  geom_violin() +
  scale_y_log10() +
  labs(x = "Publication period",
       y = "Citations (log scale)",
       title = "Citations by publication period") +
  theme_minimal(base_size = 14)

require(ggpubr)

ggplot(df, aes(x = period, y = as.numeric(Number_of_citations) )) +
  geom_boxplot() +
  scale_y_log10() +
  stat_compare_means(
    comparisons = comparisons,
    method = "wilcox.test",
    p.adjust.method = "BH",
    label = "p.signif"
  ) +
  labs(
    x = "Publication period",
    y = "Citations (log scale)",
    title = "Citations by publication period"
  ) +
  theme_minimal(base_size = 14) 



current_year <- 2025

df$Number_of_citations <- as.numeric(df$Number_of_citations)
df$Year <- as.numeric(df$Year)

citation_census_year <- 2026

df <- df %>%
  mutate(
    years_since_pub = citation_census_year - Year,
    years_since_pub = pmax(years_since_pub, 1),
    citations_per_year = Number_of_citations / years_since_pub
  )

library(ggpubr)

comparisons <- list(
  c("Pre-2015", "2015–2019"),
  c("Pre-2015", "2020+"),
  c("2015–2019", "2020+")
)

ggplot(df, aes(x = period, y = citations_per_year)) +
  geom_boxplot() +
  #scale_y_log10() +
  stat_compare_means(
    method = "kruskal.test",
    label.y = max(df$citations_per_year)
  ) +
  stat_compare_means(
    comparisons = comparisons,
    method = "wilcox.test",
    p.adjust.method = "BH",
    label = "p.signif"
  ) +
  labs(
    x = "Publication period",
    y = "Citations per year",
    title = "Citation rate by publication period"
  ) +
  theme_minimal(base_size = 14)

model <- lm(log1p(Number_of_citations) ~ Year, data = df)
summary(model)

model2 <- lm(log1p(Number_of_citations) ~ years_since_pub, data = df)
summary(model2)
df <- df %>%
  mutate(pandemic = ifelse(Year >= 2020, 1, 0))

model3 <- lm(log1p(Number_of_citations) ~ years_since_pub + pandemic, data = df)
summary(model3)

# Wordcloud of titles
library(dplyr)
library(tidytext)
library(wordcloud)
library(RColorBrewer)

# assuming df$title is a character column
head(input_df$Title)
nrow(df)
nrow(input_df)
table(is.na(input_df$DOI))

input_df %>%  filter(is.na(DOI)) %>%  count()# Number_of_citation == 0,
input_df %>%  filter(is.na(DOI)) %>%  pull(Journal)

df %>%  filter(citations_per_year == 0) %>%  count()

summary(df$citations_per_year)
summary(df$Number_of_citations)

head(input_df)
head(df)

jc <- input_df %>% group_by(Journal) %>%  summarise(Articles = n())
jc

surveillance_df <- input_df %>%
  filter(str_detect(Title, regex("surveillance", ignore_case = TRUE))) #%>%  pull(Title)


chicken_df <- input_df %>%
  filter(str_detect(Title, regex("chicken", ignore_case = TRUE))) #%>%  pull(Title)
chicken_df

duck_df <- input_df %>%
  filter(str_detect(Title, regex("duck", ignore_case = TRUE))) #%>%  pull(Title)
duck_df

fowl_df <- input_df %>%
  filter(str_detect(Title, regex("waterfowl", ignore_case = TRUE))) #%>%  pull(Title)

fowl_df

wb_df <- input_df %>%
  filter(str_detect(Title, regex("wild bird", ignore_case = TRUE))) #%>%  pull(Title)
nrow(wb_df)

flu_df <- input_df %>%
  filter(str_detect(Title, regex("influenza", ignore_case = TRUE))) #%>%  pull(Title)

nrow(flu_df)

cow_df <- input_df %>%
  filter(str_detect(Title, regex("cattle", ignore_case = TRUE))) #%>%  pull(Title)
nrow(cow_df)

livestock_df <- input_df %>%
  filter(str_detect(Title, regex("livestock", ignore_case = TRUE))) #%>%  pull(Title)
nrow(livestock_df)

livestock_df$Title
class(df$ID)
class(surveillance_df$ID)

surv <- df %>%
  filter(ID %in% surveillance_df$ID) 

summary(surv$interface_main_text)
summary(df$interface_main_text)

input_df$Title

summary(surv$years_since_pub)
summary(df$years_since_pub)

surv$Number_of_authors
surv$Number_of_citations

summary(df$Number_of_citations)
summary(surv$Number_of_citations)


hist(df$Number_of_citations)
hist(surv$Number_of_citations)

mean(df$Number_of_citations, na.rm = TRUE)
sd(df$Number_of_citations, na.rm = TRUE)

mean(surv$Number_of_citations, na.rm = TRUE)
sd(surv$Number_of_citations, na.rm = TRUE)

table(df$Number_of_citations)

jc
# Too few surveillance studies to make a fair test, low dfs!
#Surveillance represented only X% of publications, 
#yet surveillance papers received an average of 48 citations compared with 35 citations across all studies.

wilcox.test(df$Number_of_citations,
            surv$Number_of_citations)

wilcox.test(surv$interface_main_text,
            df$interface_main_text)
#

text_df <- input_df %>%
  select(Title) %>%
  unnest_tokens(word, Title) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)


windowsFonts()

library(ggplot2)
library(treemapify)

#
ggplot(jc, aes(area = 
                 Articles, fill = Articles, label = Journal)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", reflow = TRUE, size = 4) +
  scale_fill_gradientn(colours = terrain.colors(10)) +
  labs(title = "Articles per Journal",
       fill = "Articles") +
  theme_minimal()

# Only top 20 journals
jc_top <- jc %>% 
  arrange(desc(Articles)) %>% 
  slice_head(n = 20)

jc_top


ggplot(jc_top, aes(area = Articles, fill = Articles, label = Journal)) +
  geom_treemap(colour = "white", size = 1) +
  geom_treemap_text(
    place = "centre",
    grow = TRUE,      # text scales with box
    reflow = TRUE,
    colour = "black"
  ) +
  scale_fill_gradient(low = "#dbe9f6", high = "#08306b") +
  labs(title = "Top 20 Journals", fill = "Articles") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "right")


#
set.seed(123)
wordcloud(words = text_df$word,
  freq = text_df$n,
  min.freq = 2,
  max.words = 200,
  random.order = FALSE,
  colors = brewer.pal(8, "PRGn"),  family = "TT Arial")

text_df %>% arrange(-n) %>% slice_head(n=20)


text_df %>% 
  arrange(desc(n))

text_df %>% 
  filter(word == "surveillance")


df %>% arrange(-Number_of_citations) %>% slice_head(n=5) %>%  
  pull(Number_of_citations, label)


df %>% arrange(-Number_of_citations) %>% slice_head(n=5) %>%  pull(Number_of_authors)
summary(df$Number_of_authors)

#-------------------------------------------------------------------------------