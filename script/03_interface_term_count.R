# Interface mention counts - runs fast


#install.packages(c("pdftools", "stringr", "dplyr"))

library(pdftools)
library(stringr)
library(dplyr)
library(here)
library(openxlsx)

setwd(here())
# Folder containing PDFs

pdf_folder <- "C://Users//rdel0062//OneDrive - The University of Sydney (Staff)//review_interface//papers_270//"

pdf_files <- list.files(pdf_folder, pattern="\\.pdf$", full.names=TRUE)

pdf_files


# Pattern for interface (case-insensitive, handles inter-face)
pattern_interface <- regex("\\binter[-\\s]?faces?\\b", ignore_case = TRUE)

# Pattern to detect references section
pattern_refs <- regex("\\b(references|bibliography|literature cited|works cited)\\b",
                      ignore_case = TRUE)

threshold <- 80

count_interface <- function(file) {
  
  text_pages <- pdf_text(file)
  full_text <- paste(text_pages, collapse=" ")
  
  # count everywhere
  total_count <- str_count(full_text, pattern_interface)
  
  # find start of references
  ref_pos <- str_locate(full_text, pattern_refs)[1]
  
  if(!is.na(ref_pos)) {
    main_text <- substr(full_text, 1, ref_pos - 1)
  } else {
    main_text <- full_text
  }
  
  # count only before references
  main_count <- str_count(main_text, pattern_interface)
  
  ref_count <- total_count - main_count
  
  file_name <- basename(file)
  base_name <- tools::file_path_sans_ext(file_name)
  
  data.frame(
    id = paste0("a", base_name),
    #file = file_name,
    interface_total = total_count,
    interface_main_text = main_count,
    interface_references = ref_count,
    flag_high = total_count > threshold
  )
}

results <- bind_rows(lapply(pdf_files, count_interface))

nrow(results)

head(results)

setwd(here())

setwd('./results')

#write.xlsx(results, "interface_term_counts.xlsx",  overwrite = TRUE)


results <- read.xlsx('interface_term_counts.xlsx')

print(results)

# Plot

table(results$flag_high)

library(ggplot2)
library(dplyr)
library(ggrepel)  # for nice non-overlapping labels

# Assume 'results' already exists

ggplot(results, aes(x = interface_main_text, 
                    y = interface_references,
                    color = flag_high)) +
  geom_point(size = 4) +
  # add labels only for flagged points
  geom_text_repel(data = results %>% filter(flag_high),
                  aes(label = id),
                  nudge_y = 0.5,
                  size = 4) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  labs(x = "Interface count in main text",
       y = "Interface count in references",
       color = "High Interface Count") +
  theme_minimal(base_size=14) +
  theme(legend.position = "top")


# GGplot
normal_results <- results %>% filter(flag_high == FALSE)

ggplot(normal_results, aes(x = interface_main_text,
                           y = interface_references)) +
  geom_point(color = "black", size = 4) +
  labs(y = "Interface count in main text",
       x = "Interface count in references",
       title = "") +
  theme_minimal(base_size=14)

cor.test(results$interface_main_text, results$interface_references, method='spearman')


#all

ggplot(results, aes(x = interface_main_text,
                           y = interface_references,
                           label = id)) +
  geom_text_repel(size = 5, color = "black") +
  labs(x = "Interface count in main text",
       y = "Interface count in references",
       title = "IDs for non-flagged PDFs") +
  theme_minimal(base_size=14)


#filtered

ggplot(normal_results, aes(x = interface_main_text,
                           y = interface_references,
                           label = id)) +
  geom_text_repel(size = 5, color = "black") +
  labs(x = "Interface count in main text",
       y = "Interface count in references",
       title = "IDs for non-flagged PDFs") +
  theme_minimal(base_size=14)

#-------------------------------------------------------------------------------