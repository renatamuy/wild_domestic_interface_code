# ============================================================
# FIGURE: wildlife-domestic interface landscape
# Heatmap colour = number of studies representing each host pair
# Bubble size    = number of distinct harmonised disease systems
#
# IMPORTANT INTERPRETATION
# Host pairs are study-level co-occurrences: both host groups occur
# in the same paper. Disease breadth means distinct harmonised disease
# systems represented among papers containing that host pair.
# Neither quantity alone demonstrates direct transmission between
# that exact wildlife and domestic group.
# ============================================================

rm(list = ls())
gc()

suppressPackageStartupMessages({
  library(tidyverse)
  library(openxlsx)
  library(fs)
  library(here)
  library(scales)
})

setwd(here())
setwd("script")

root_dir   <- normalizePath("..", winslash = "/", mustWork = TRUE)
results_dir <- file.path(root_dir, "results_synthesis_consolidated")

test_mode <- TRUE
final_prefix <- if (test_mode) "evidence_first_test" else "evidence_first_full"

summary_file <- file.path(
  results_dir,
  paste0(final_prefix, "_manuscript_summary_tables.xlsx")
)

if (!file_exists(summary_file)) {
  stop("Summary workbook not found: ", summary_file)
}

# ------------------------------------------------------------
# 1. Read authoritative sheets
# ------------------------------------------------------------
library(readxl)

paper_summary <- readxl::read_excel(
  summary_file, sheet = "paper_summary"
)

wildlife_dict <- readxl::read_excel(
  summary_file, sheet = "wildlife_taxa_dict"
)

domestic_dict <- readxl::read_excel(
  summary_file, sheet = "domestic_taxa_dict"
)

disease_harmonised <- readxl::read_excel(
  summary_file, sheet = "disease_harmonised"
)
# Only whitespace is cleaned. Biological classifications are NEVER
# inferred here: the frozen dictionaries are the source of truth.
wildlife_dict <- wildlife_dict |>
  transmute(
    wildlife_taxon_original = str_squish(wildlife_taxon_original),
    wildlife_host_group = str_squish(wildlife_host_group)
  ) |>
  distinct()

domestic_dict <- domestic_dict |>
  transmute(
    domestic_taxon_original = str_squish(domestic_taxon_original),
    domestic_host_group = str_squish(domestic_host_group)
  ) |>
  distinct()

# ------------------------------------------------------------
# 2. Dictionary QC
# ------------------------------------------------------------

wildlife_conflicts <- wildlife_dict |>
  count(wildlife_taxon_original, name = "n_groups") |>
  filter(n_groups > 1)

domestic_conflicts <- domestic_dict |>
  count(domestic_taxon_original, name = "n_groups") |>
  filter(n_groups > 1)

if (nrow(wildlife_conflicts) > 0) {
  stop("Conflicting mappings found in wildlife dictionary.")
}

if (nrow(domestic_conflicts) > 0) {
  stop("Conflicting mappings found in domestic dictionary.")
}

# ------------------------------------------------------------
# 3. Expand paper taxa and apply manual dictionaries
# ------------------------------------------------------------

wildlife_long <- paper_summary |>
  select(paper_id, animal_focus, wildlife_taxa) |>
  filter(!is.na(wildlife_taxa), nzchar(wildlife_taxa)) |>
  separate_rows(wildlife_taxa, sep = ";\\s*") |>
  transmute(
    paper_id,
    animal_focus,
    wildlife_taxon_original = str_squish(wildlife_taxa)
  ) |>
  filter(nzchar(wildlife_taxon_original)) |>
  distinct() |>
  left_join(wildlife_dict, by = "wildlife_taxon_original")

domestic_long <- paper_summary |>
  select(paper_id, animal_focus, domestic_taxa) |>
  filter(!is.na(domestic_taxa), nzchar(domestic_taxa)) |>
  separate_rows(domestic_taxa, sep = ";\\s*") |>
  transmute(
    paper_id,
    animal_focus,
    domestic_taxon_original = str_squish(domestic_taxa)
  ) |>
  filter(nzchar(domestic_taxon_original)) |>
  distinct() |>
  left_join(domestic_dict, by = "domestic_taxon_original")

wildlife_unmatched <- wildlife_long |>
  filter(is.na(wildlife_host_group))

domestic_unmatched <- domestic_long |>
  filter(is.na(domestic_host_group))

message("Wildlife unmatched records: ", nrow(wildlife_unmatched))
message("Domestic unmatched records: ", nrow(domestic_unmatched))

# Deliberate dictionary exclusions are retained in QC but excluded
# from the interface analysis.
wildlife_groups <- wildlife_long |>
  filter(!is.na(wildlife_host_group)) |>
  distinct(paper_id, wildlife_host_group)

domestic_groups <- domestic_long |>
  filter(
    !is.na(domestic_host_group),
    !str_starts(domestic_host_group, "REMOVE")
  ) |>
  distinct(paper_id, domestic_host_group)

# ------------------------------------------------------------
# 4. QC the 270 -> 252 -> mapped-interface denominator
# ------------------------------------------------------------

both_papers <- paper_summary |>
  filter(animal_focus == "Both") |>
  distinct(paper_id)

mapped_interface_papers <- wildlife_groups |>
  distinct(paper_id) |>
  inner_join(domestic_groups |> distinct(paper_id), by = "paper_id")

both_missing_from_interface <- both_papers |>
  anti_join(mapped_interface_papers, by = "paper_id") |>
  left_join(
    paper_summary |>
      select(paper_id, author, title, wildlife_taxa, domestic_taxa),
    by = "paper_id"
  )

message("Total papers: ", n_distinct(paper_summary$paper_id))
message("Animal focus = Both: ", nrow(both_papers))
message("Mapped wildlife + domestic interface papers: ",
        nrow(mapped_interface_papers))
message("Both papers missing a mapped interface: ",
        nrow(both_missing_from_interface))

# ------------------------------------------------------------
# 5. Construct study-level host-pair co-occurrences
# ------------------------------------------------------------
# IMPORTANT:
# This deliberately generates all wildlife x domestic host-group
# combinations represented within each paper. These are NOT claimed
# to be explicitly demonstrated biological interactions.

interface_pairs_by_paper <- wildlife_groups |>
  inner_join(domestic_groups, by = "paper_id") |>
  distinct(
    paper_id,
    wildlife_host_group,
    domestic_host_group
  )

pair_study_counts <- interface_pairs_by_paper |>
  count(
    wildlife_host_group,
    domestic_host_group,
    name = "n_studies"
  )

# ------------------------------------------------------------
# 6. Attach harmonised disease systems
# ------------------------------------------------------------

disease_by_paper <- disease_harmonised |>
  transmute(
    paper_id = article_id,
    disease_harmonised = str_squish(disease_harmonised)
  ) |>
  filter(
    !is.na(disease_harmonised),
    nzchar(disease_harmonised)
  ) |>
  distinct(paper_id, disease_harmonised)

# A disease is counted only once per paper x host-pair.
pair_disease_by_paper <- interface_pairs_by_paper |>
  inner_join(disease_by_paper, by = "paper_id") |>
  distinct(
    paper_id,
    wildlife_host_group,
    domestic_host_group,
    disease_harmonised
  )

pair_disease_summary <- pair_disease_by_paper |>
  group_by(wildlife_host_group, domestic_host_group) |>
  summarise(
    n_diseases = n_distinct(disease_harmonised),
    .groups = "drop"
  )

table(paper_summary$study_type)

pair_disease_counts <- pair_disease_by_paper |>
  left_join(
    paper_summary |>
      select(paper_id, study_type),
    by = "paper_id"
  ) |>
  mutate(
    is_review = str_detect(
      coalesce(as.character(study_type), ""),
      regex("review", ignore_case = TRUE)
    )
  ) |>
  group_by(
    wildlife_host_group,
    domestic_host_group,
    disease_harmonised
  ) |>
  summarise(
    disease_papers_n = n_distinct(paper_id),
    review_n = n_distinct(paper_id[is_review]),
    review_prop = review_n / disease_papers_n,
    .groups = "drop"
  )

#
pair_disease_counts$review_n


top_diseases <- pair_disease_counts |>
  group_by(wildlife_host_group, domestic_host_group) |>
  arrange(desc(disease_papers_n), disease_harmonised, .by_group = TRUE) |>
  slice_head(n = 3) |>
  summarise(
    top_diseases = paste0(
      disease_harmonised,
      " (", disease_papers_n, ")",
      collapse = "; "
    ),
    .groups = "drop"
  )

interface_summary <- pair_study_counts |>
  left_join(
    pair_disease_summary,
    by = c("wildlife_host_group", "domestic_host_group")
  ) |>
  left_join(
    top_diseases,
    by = c("wildlife_host_group", "domestic_host_group")
  ) |>
  mutate(
    n_diseases = replace_na(n_diseases, 0L),
    disease_breadth_per_study = n_diseases / n_studies
  ) |>
  arrange(desc(n_studies), desc(n_diseases))

# ------------------------------------------------------------
# 7. Prepare Panel A: complete interface heatmap
# ------------------------------------------------------------

wildlife_order <- interface_pairs_by_paper |>
  distinct(paper_id, wildlife_host_group) |>
  count(wildlife_host_group, sort = TRUE) |>
  pull(wildlife_host_group)

domestic_order <- interface_pairs_by_paper |>
  distinct(paper_id, domestic_host_group) |>
  count(domestic_host_group, sort = TRUE) |>
  pull(domestic_host_group)

panel_a_data <- pair_study_counts |>
  complete(
    wildlife_host_group = wildlife_order,
    domestic_host_group = domestic_order,
    fill = list(n_studies = 0L)
  ) |>
  mutate(
    wildlife_host_group = factor(
      wildlife_host_group,
      levels = rev(wildlife_order)
    ),
    domestic_host_group = factor(
      domestic_host_group,
      levels = domestic_order
    )
  )

# ------------------------------------------------------------
# 8. Prepare Panel B: diseases within major interfaces
# ------------------------------------------------------------
# Display choices are parameters, not scientific exclusions.
# The complete pair x disease table is exported below.
#
# Default:
#   - top 12 host combinations by number of studies
#   - top 12 disease systems across those host combinations
#
# Change these two numbers freely to test readability.

n_top_interfaces <- 12
n_top_diseases   <- 12

top_interfaces <- pair_study_counts |>
  arrange(desc(n_studies), wildlife_host_group, domestic_host_group) |>
  slice_head(n = n_top_interfaces) |>
  mutate(
    interface_label = paste(
      wildlife_host_group,
      domestic_host_group,
      sep = " × "
    )
  )

# Restrict only the DISPLAY to the major interfaces.
# The underlying disease table remains complete.
panel_b_all_diseases <- pair_disease_counts |>
  inner_join(
    top_interfaces |>
      select(
        wildlife_host_group,
        domestic_host_group,
        interface_label,
        interface_n_studies = n_studies
      ),
    by = c("wildlife_host_group", "domestic_host_group")
  )

# Rank diseases by the number of UNIQUE papers represented across
# the displayed interfaces. Because the same paper may occur in more
# than one host pair, this ranking is only for visual selection.
# Panel B dot sizes retain the pair-specific paper counts.

major_diseases <- pair_disease_by_paper |>
  semi_join(
    top_interfaces,
    by = c("wildlife_host_group", "domestic_host_group")
  ) |>
  distinct(paper_id, disease_harmonised) |>
  count(disease_harmonised, name = "papers_across_displayed_interfaces") |>
  arrange(desc(papers_across_displayed_interfaces), disease_harmonised) |>
  slice_head(n = n_top_diseases)

panel_b_data <- panel_b_all_diseases |>
  semi_join(major_diseases, by = "disease_harmonised") |>
  mutate(
    interface_label = factor(
      interface_label,
      levels = rev(top_interfaces$interface_label)
    )
  )

# Disease order: most represented across displayed interfaces first.
disease_order <- major_diseases$disease_harmonised

panel_b_data <- panel_b_data |>
  mutate(
    disease_harmonised = factor(
      disease_harmonised,
      levels = disease_order
    )
  )

# ------------------------------------------------------------
# 9. Panel A
# ------------------------------------------------------------
# Colour = number of UNIQUE studies containing both host groups.
# No disease information is forced into this panel.

max_studies <- max(panel_a_data$n_studies, na.rm = TRUE)

panel_a <- ggplot(
  panel_a_data,
  aes(
    x = domestic_host_group,
    y = wildlife_host_group
  )
) +
  geom_tile(
    aes(fill = n_studies),
    colour = "white",
    linewidth = 0.65
  ) +
  scale_fill_gradientn(
    colours = c(
      "#F4EBD8",  # pale sand
      "#D9E7C3",  # light meadow
      "#A9C89B",  # soft green
      "#789C72",  # muted forest
      "#536F55"   # deep terrain green
    ),
    values = scales::rescale(c(0, 0.10, 0.30, 0.60, 1)),
    trans = "sqrt",
    name = "Publications",
    breaks = pretty(c(0, max_studies), n = 5)
  ) +
  labs(
    title = "A",
    #subtitle = "Study representation across wildlife–domestic host-group combinations"
    ,
    x = "Domestic-associated group",
    y = "Wildlife-associated group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = ggtext::element_markdown(
      angle = 50,
      hjust = 1,
      vjust = 1,
      size = 11,
      colour = "#4E4637",
      fill = "#FFF1B8",
      box.colour = NA,
      linetype = 0,
      r = unit(2.5, "pt"),
      padding = margin(3, 4.5, 3, 4.5)
    ),
    
    axis.text.y = ggtext::element_markdown(
      size = 11,
      colour = "#34463A",
      fill = "#DCEED7",
      box.colour = NA,
      linetype = 0,
      r = unit(2.5, "pt"),
      padding = margin(3, 4.5, 3, 4.5)
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 13,
      margin = margin(t = 12)
    ),
    
    axis.title.y = element_text(
      face = "bold",
      size = 13,
      margin = margin(r = 10)
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  ) + 
  guides(
    fill = guide_colourbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(10, "cm"),
      barheight = unit(0.5, "cm")
    )
  )

# ------------------------------------------------------------
# 10. Panel B
# ------------------------------------------------------------
# Rows = major host combinations
# Columns = major harmonised disease systems
# Dot size = number of UNIQUE papers containing that host combination
#            and disease system.
#
# IMPORTANT:
# The wildlife/domestic labels are drawn as separate geom_label()
# columns rather than HTML inside an axis label. This is much more
# stable and gives clean, independently coloured label boxes.

# Explicit numeric positions make the label columns predictable.
interface_positions <- top_interfaces |>
  mutate(
    interface_y = rev(seq_len(n()))
  )

disease_positions <- tibble(
  disease_harmonised = disease_order,
  disease_x = seq_along(disease_order)
)

panel_b_plot_data <- panel_b_all_diseases |>
  semi_join(major_diseases, by = "disease_harmonised") |>
  left_join(
    interface_positions |>
      select(
        wildlife_host_group,
        domestic_host_group,
        interface_label,
        interface_y
      ),
    by = c(
      "wildlife_host_group",
      "domestic_host_group",
      "interface_label"
    )
  ) |>
  left_join(
    disease_positions,
    by = "disease_harmonised"
  )

# Coordinates for the two host-label columns.
# These sit to the LEFT of the first disease column.
wild_label_x <- -4.2
cross_x      <- -2.85
dom_label_x  <- -2.55

panel_b <- ggplot() +
  # Disease dots
  geom_point(
    data = panel_b_plot_data,
    aes(
      x = disease_x,
      y = interface_y,
      size = disease_papers_n,
      colour = review_prop
    )
  ) +
  scale_colour_gradient(
    low = "#3B82A0",
    high = "#D66B63",
    limits = c(0, 1),
    labels = scales::percent,
    name = "Reviews (%)"
  ) +

  # Wildlife label: pastel green, bold, right-aligned
  geom_label(
    data = interface_positions,
    aes(
      x = wild_label_x,
      y = interface_y,
      label = wildlife_host_group
    ),
    inherit.aes = FALSE,
    fill = "#DCEED7",
    colour = "#34463A",
    fontface = "bold",
    label.size = 0,
    label.padding = unit(0.18, "lines"),
    hjust = 1,
    size = 4
  ) +

  # Multiplication sign between the two host domains
  geom_text(
    data = interface_positions,
    aes(
      x = cross_x,
      y = interface_y,
      label = "\u00d7"
    ),
    inherit.aes = FALSE,
    colour = "#7A7467",
    size = 4
  ) +

  # Domestic label: light yellow, bold, left-aligned
  geom_label(
    data = interface_positions,
    aes(
      x = dom_label_x,
      y = interface_y,
      label = domestic_host_group
    ),
    inherit.aes = FALSE,
    fill = "#FFF1B8",
    colour = "#4E4637",
    fontface = "bold",
    label.size = 0,
    label.padding = unit(0.18, "lines"),
    hjust = 0,
    size = 3.1
  ) +

  scale_size_area(
    max_size = 12,
    name = "Publications"
  ) +

  # Disease labels remain the x-axis categories.
  scale_x_continuous(
    breaks = disease_positions$disease_x,
    labels = disease_positions$disease_harmonised,
    limits = c(-5.8, max(disease_positions$disease_x) + 0.5),
    expand = expansion(mult = c(0, 0))
  ) +

  scale_y_continuous(
    breaks = interface_positions$interface_y,
    labels = NULL,
    limits = c(0.5, nrow(interface_positions) + 0.5),
    expand = c(0, 0)
  ) +

  # Host labels are now inside the x scale; clipping is kept off only
  # as a safeguard for label-box edges.
  coord_cartesian(clip = "off") +

  labs(
    title = "B", #Disease systems within major interfaces
    #subtitle = paste0(
     # "Top ", n_top_interfaces,
      #" host combinations and top ", n_top_diseases,
      #" disease systems shown")
    ,
    x = "Disease system",
    y = NULL
  ) +

  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      linewidth = 0.25,
      colour = "grey88"
    ),
    panel.grid.major.x = element_line(
      linewidth = 0.25,
      colour = "grey90"
    ),
    axis.text.x = element_text(
      angle = 50,
      hjust = 1,
      vjust = 1,
      size = 9.5,
      colour = "#3F3F3F"
    ),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 9.5),
    legend.position = "bottom",

    # Deliberately generous left margin for the two host-label columns.
    plot.margin = margin(
      t = 8,
      r = 10,
      b = 8,
      l = 10,
      unit = "pt"
    )
  )

# ------------------------------------------------------------
# 11. Export individual panels
# ------------------------------------------------------------

panel_a_file <- file.path(
  results_dir,
  paste0(final_prefix, "_interface_panel_A_heatmap.png")
)

panel_b_file <- file.path(
  results_dir,
  paste0(final_prefix, "_interface_panel_B_diseases.png")
)

ggsave(
  panel_a_file,
  panel_a,
  width = 13.5,
  height = 8.5,
  dpi = 400,
  bg = "white"
)

ggsave(
  panel_b_file,
  panel_b,
  width = 13.5,
  height = 7.5,
  dpi = 400,
  bg = "white"
)

# ------------------------------------------------------------
# 12. Combined A + B figure
# ------------------------------------------------------------
# patchwork is used only for figure assembly.

if (!requireNamespace("patchwork", quietly = TRUE)) {
  stop(
    "Package 'patchwork' is required for the combined figure. ",
    "Install it with install.packages('patchwork'). ",
    "The individual panels can still be produced without it."
  )
}

combined_figure <- panel_a / panel_b +
  patchwork::plot_layout(heights = c(1.08, 1)) +
  patchwork::plot_annotation(
    title = "",
    #caption = paste(
     # "Panel A: colour indicates the number of unique studies containing each wildlife–domestic host-group combination.",
    #  "Panel B: dot size indicates the number of unique papers in which the displayed host combination and disease system co-occur.",
     # "Co-occurrence within a study does not necessarily demonstrate direct transmission between the host groups.")
    ,
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.caption = element_text(size = 8.5, hjust = 0)
    )
  )

combined_file <- file.path(
  results_dir,
  paste0(final_prefix, "_Figure_04.png")
)

ggsave(
  combined_file,
  combined_figure,
  width = 15,
  height = 16,
  dpi = 400,
  bg = "white"
)

#---------------------------------------------------------------------------------