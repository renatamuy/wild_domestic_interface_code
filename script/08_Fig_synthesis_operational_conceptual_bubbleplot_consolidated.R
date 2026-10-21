rm(list = ls())
gc()

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(fs)
  library(here)
})

# ============================================================
# FIGURE 5 — SYNTHESIS RESEARCH SPACE
# Bubble size = number of papers at each exact coordinate
# Uses consolidated post-QC synthesis output:
# results_synthesis_consolidated/evidence_first_test_plot_data.csv
# ============================================================

setwd(here())
setwd("script")

root_dir <- normalizePath("..", winslash = "/", mustWork = TRUE)
results_dir <- file.path(root_dir, "results_synthesis_consolidated")

# ============================================================
# INPUT FILE
# ============================================================

plot_data_file <- file.path(
  results_dir,
  "evidence_first_test_plot_data.csv"
)

if (!file_exists(plot_data_file)) {
  stop(
    "Plot data file not found: ",
    plot_data_file
  )
}

# ============================================================
# LOAD DATA
# ============================================================

paper_space <- read_csv(
  plot_data_file,
  show_col_types = FALSE
)

required_columns <- c(
  "paper_id",
  "host_orientation",
  "interface_operationalisation",
  "host_focus_group"
)

missing_columns <- setdiff(
  required_columns,
  names(paper_space)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

# ============================================================
# SANITY CHECK
# ============================================================

message(
  "Rows in plot-data file: ",
  nrow(paper_space)
)


# ============================================================
# CONSOLIDATED-QC GUARDS
# ============================================================

if (nrow(paper_space) != 270) {
  stop(
    "Expected 270 papers in consolidated plot data; found ",
    nrow(paper_space),
    "."
  )
}

if (n_distinct(paper_space$paper_id) != 270) {
  stop("Expected 270 unique paper_id values in consolidated plot data.")
}

qc_targets <- paper_space |>
  filter(paper_id %in% c("a52", "a122"))

if (nrow(qc_targets) != 2) {
  stop("Expected both a52 and a122 in consolidated plot data.")
}

qc_expected <- tibble(
  paper_id = c("a52", "a122"),
  expected_x = c(0, 0),
  expected_y = c(1, 0)
)

qc_check <- qc_targets |>
  select(
    paper_id,
    host_orientation,
    interface_operationalisation
  ) |>
  left_join(qc_expected, by = "paper_id") |>
  mutate(
    qc_pass =
      dplyr::near(host_orientation, expected_x) &
      dplyr::near(interface_operationalisation, expected_y)
  )

if (!all(qc_check$qc_pass)) {
  stop(
    "Consolidated-QC coordinate check failed for a52/a122. ",
    "Expected a52 = (0, 1) and a122 = (0, 0)."
  )
}

message(
  "QC coordinate check passed: a52 = (0, 1); a122 = (0, 0)."
)

# ============================================================
# EXCLUDE PAPERS WITH MISSING COORDINATES
# ============================================================

plot_excluded <- paper_space |>
  filter(
    is.na(host_orientation) |
      is.na(interface_operationalisation)
  )

paper_space_plot <- paper_space |>
  filter(
    !is.na(host_orientation),
    !is.na(interface_operationalisation)
  )

if (nrow(paper_space_plot) == 0) {
  stop(
    "No papers have both final host and interface scores."
  )
}

message(
  "Papers available: ",
  nrow(paper_space)
)

message(
  "Papers plotted: ",
  nrow(paper_space_plot)
)

message(
  "Papers excluded for missing coordinates: ",
  nrow(plot_excluded)
)

# ============================================================
# COLLAPSE OVERLAPPING PAPERS INTO FREQUENCIES
# ============================================================

plot_counts <- paper_space_plot |>
  count(
    host_orientation,
    interface_operationalisation,
    host_focus_group,
    name = "n_papers"
  ) |>
  mutate(
    percent_corpus =
      100 * n_papers / nrow(paper_space_plot)
  )

message(
  "Unique occupied coordinate combinations: ",
  nrow(plot_counts)
)

# ============================================================
# COLOURS
# ============================================================

focus_palette <- c(
  "Wildlife-centred" = "#285C4D",
  "Balanced wild–domestic" = "#C89B3C",
  "Domestic-centred" = "#A6523F",
  "Unclassified" = "#747474"
)

# ============================================================
# BASE FIGURE
# ============================================================

interface_map <- ggplot(
  plot_counts,
  aes(
    x = host_orientation,
    y = interface_operationalisation
  )
) +
  
  # ----------------------------------------------------------
# Background conceptual regions
# ----------------------------------------------------------

annotate(
  "rect",
  xmin = -1.05,
  xmax = 0,
  ymin = 0.25,
  ymax = 1.05,
  fill = "#DDEAE3"
) +
  
  annotate(
    "rect",
    xmin = 0,
    xmax = 1.05,
    ymin = 0.25,
    ymax = 1.05,
    fill = "#EDDCD7"
  ) +
  
  annotate(
    "rect",
    xmin = -1.05,
    xmax = 0,
    ymin = -1.05,
    ymax = 0.25,
    fill = "#EEF4F1"
  ) +
  
  annotate(
    "rect",
    xmin = 0,
    xmax = 1.05,
    ymin = -1.05,
    ymax = 0.25,
    fill = "#F8EFEB"
  ) +
  
  # ----------------------------------------------------------
# Balanced host-focus band
# ----------------------------------------------------------

annotate(
  "rect",
  xmin = -0.18,
  xmax = 0.18,
  ymin = -1.05,
  ymax = 1.05,
  fill = "#E8D9AC",
  alpha = 0.80
) +
  
  # ----------------------------------------------------------
# Reference lines
# ----------------------------------------------------------

geom_vline(
  xintercept = 0,
  linewidth = 0.9,
  colour = "#5D5137"
) +
  
  # Lower region:
  # mention / conceptual / qualitative
  #
  # Upper region:
  # quantitative / operationalised
  geom_hline(
    yintercept = 0.25,
    linewidth = 0.8,
    colour = "#555555"
  ) +
  
  geom_hline(
    yintercept = c(
      -1,
      -0.5,
      0,
      0.5,
      1
    ),
    linewidth = 0.35,
    colour = "white",
    linetype = "dotted"
  )

# ============================================================
# FREQUENCY BUBBLES
# ============================================================

interface_map <- interface_map +
  
  geom_point(
    aes(
      size = n_papers,
      fill = host_focus_group
    ),
    shape = 21,
    colour = "white",
    stroke = 0.8,
    alpha = 0.90
  ) +
  
  geom_text(
    aes(
      label = n_papers
    ),
    colour = "white",
    fontface = "bold",
    size = 3.2
  ) +
  
  scale_size_area(
    name = "Number of papers",
    max_size = 25,
    breaks = pretty_breaks(
      n = 5
    )
  ) +
  
  scale_fill_manual(
    values = focus_palette,
    name = "Host focus",
    drop = FALSE
  )

# ============================================================
# REGION LABELS
# ============================================================

interface_map <- interface_map +
  
  annotate(
    "text",
    x = -0.66,
    y = 0.90,
    label =
      "\nHigher interface centrality\nWildlife-centred",
    fontface = "bold",
    colour = "#285C4D",
    size = 4
  ) +
  
  annotate(
    "text",
    x = 0.66,
    y = 0.90,
    label =
      "\nHigher interface centrality\nDomestic-centred",
    fontface = "bold",
    colour = "#A6523F",
    size = 4
  ) +
  
  annotate(
    "text",
    x = -0.66,
    y = -0.88,
    label =
      "Lower interface centrality\nWildlife-centred",
    fontface = "bold",
    colour = "#285C4D",
    size = 4
  ) +
  
  annotate(
    "text",
    x = 0.66,
    y = -0.88,
    label =
      "Lower interface centrality\nDomestic-centred",
    fontface = "bold",
    colour = "#A6523F",
    size = 4
  ) +
  
  annotate(
    "label",
    x = 0,
    y = 0.70,
    label =
      "More integrated\n and empirical research",
    fill = "#F7F0DD",
    colour = "#77581D",
    fontface = "bold",
    linewidth = 0,
    size = 3.3,
    lineheight = 0.9
  )

# ============================================================
# AXES
# ============================================================

interface_map <- interface_map +
  
  scale_x_continuous(
    limits = c(
      -1.05,
      1.05
    ),
    breaks = c(
      -1,
      -0.75,
      -0.5,
      0,
      0.5,
      0.75,
      1
    ),
    labels = c(
      "Wildlife\nonly",
      "Predominantly\nwildlife",
      "Wildlife-\ncentred",
      "Balanced",
      "Domestic-\ncentred",
      "Predominantly\ndomestic",
      "Domestic\nonly"
    )
  ) +
  
  scale_y_continuous(
    limits = c(
      -1.05,
      1.05
    ),
    breaks = c(
      -1,
      -0.5,
      0,
      0.5,
      1
    ),
    labels = c(
      "Introductory\nmention",
      "Conceptual\nframing",
      "Qualitative\nrepresentation",
      "Quantitative\nrepresentation",
      "Interface\noperationalised"
    )
  )

# ============================================================
# LABELS + THEME
# ============================================================

interface_map <- interface_map +
  
  labs(
    title =
      "The research space of the wild–domestic animal interface",
    subtitle = paste0(
      "Evidence synthesis (n = ",
      nrow(paper_space_plot),
      " papers)"
    ),
    x =
      "Host-domain orientation",
    y =
      "Interface centrality"
  ) +
  
  coord_cartesian(
    clip = "off"
  ) +
  
  theme_minimal(
    base_size = 12
  ) +
  
  theme(
    panel.grid = element_blank(),
    
    plot.title = element_text(
      face = "bold",
      size = 17
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    axis.text.x = element_text(
      size = 8.5
    ),
    
    legend.position = "bottom"
  ) +
  
  guides(
    fill = guide_legend(
      order = 1,
      override.aes = list(
        shape = 21,
        size = 5,
        alpha = 1,
        colour = "white"
      )
    ),
    
    size = guide_legend(
      order = 2
    )
  )

# ============================================================
# PRINT
# ============================================================

print(interface_map)

# ============================================================
# EXPORTS
# ============================================================

figure_jpg <- file.path(
  results_dir,
  "Fig_synthesis_research_space_bubbles.jpg"
)

figure_pdf <- file.path(
  results_dir,
  "Fig_synthesis_research_space_bubbles.pdf"
)

frequency_csv <- file.path(
  results_dir,
  "Fig_coordinate_frequencies.csv"
)

plotted_csv <- file.path(
  results_dir,
  "Fig_synthesis_plotted_data.csv"
)

excluded_csv <- file.path(
  results_dir,
  "Fig_synthesis_excluded_missing_coordinates.csv"
)


qc_summary_csv <- file.path(
  results_dir,
  "Fig_consolidated_QC_summary.csv"
)

# ============================================================
# SAVE FIGURES
# ============================================================

ggsave(
  filename = figure_jpg,
  plot = interface_map,
  width = 11,
  height = 8,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = figure_pdf,
  plot = interface_map,
  width = 11,
  height = 8,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

# ============================================================
# SAVE DATA
# ============================================================

write_csv(
  plot_counts,
  frequency_csv
)

write_csv(
  paper_space_plot,
  plotted_csv
)

write_csv(
  plot_excluded,
  excluded_csv
)


figure_qc_summary <- tibble(
  metric = c(
    "papers_available",
    "papers_plotted",
    "papers_excluded_missing_coordinates",
    "occupied_coordinate_combinations",
    "a52_host_orientation",
    "a52_interface_operationalisation",
    "a122_host_orientation",
    "a122_interface_operationalisation"
  ),
  value = c(
    nrow(paper_space),
    nrow(paper_space_plot),
    nrow(plot_excluded),
    nrow(plot_counts),
    qc_targets$host_orientation[qc_targets$paper_id == "a52"],
    qc_targets$interface_operationalisation[qc_targets$paper_id == "a52"],
    qc_targets$host_orientation[qc_targets$paper_id == "a122"],
    qc_targets$interface_operationalisation[qc_targets$paper_id == "a122"]
  )
)

write_csv(
  figure_qc_summary,
  qc_summary_csv
)

# ============================================================
# FINAL MESSAGES
# ============================================================

message(
  "Saved bubble Figure 5: ",
  figure_jpg
)

message(
  "Saved bubble Figure 5: ",
  figure_pdf
)

message(
  "Saved coordinate-frequency table: ",
  frequency_csv
)

message(
  "Saved consolidated Figure 5 QC summary: ",
  qc_summary_csv
)
