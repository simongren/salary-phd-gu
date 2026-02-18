# ============================================================================
# plot_salaries.R
# AGENT ROLE: DATA VISUALISATION CODER
# ----------------------------------------------------------------------------
# Reads data/salaries.csv and produces a publication-quality visualisation
# of negotiated PhD salary development at Göteborgs Universitet, 2022–2025.
#
# Output: output/salary_evolution.png  (and .svg)
# ============================================================================

library(tidyverse)
library(scales)
library(ggrepel)
library(patchwork)

# ── Paths ─────────────────────────────────────────────────────────────────────
# Works both when run via Rscript and interactively inside RStudio.
this_file <- tryCatch(
  normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE),
  error = function(e) ""
)
if (!nzchar(this_file)) {
  # Try rstudioapi when in an interactive RStudio session
  this_file <- tryCatch(
    rstudioapi::getActiveDocumentContext()$path,
    error = function(e) ""
  )
}
if (nzchar(this_file)) {
  base_dir <- dirname(dirname(this_file))
} else {
  # Final fallback: project root relative to working directory
  base_dir <- "c:/Users/xlunsi/OneDrive - Göteborgs Universitet/Skrivbordet/Privat/Data_viz/salary_phd_gu"
}

data_path   <- file.path(base_dir, "data",   "salaries.csv")
output_dir  <- file.path(base_dir, "output")
dir.create(output_dir, showWarnings = FALSE)

# ── GU brand colour palette ───────────────────────────────────────────────────
# Primary colours from Göteborgs Universitets visual identity
GU_BLUE      <- "#003B6F"   # main navy
GU_CYAN      <- "#00B0D8"   # sky blue
GU_YELLOW    <- "#F5C220"   # gold
GU_TEAL      <- "#00A899"   # teal
GU_RED       <- "#C8102E"   # red
GU_GREY      <- "#6D7278"   # warm grey
GU_LIGHTGREY <- "#E8E8E8"

# ── Load data ─────────────────────────────────────────────────────────────────
raw <- read_csv(data_path, show_col_types = FALSE)

# ── Normalise faculty names ───────────────────────────────────────────────────
# 2025 merged IT + NatSci → map back to consistent short labels for plotting.
# We also create cleaner short labels for facets.
faculty_map <- tribble(
  ~faculty_raw,                                                                                                     ~faculty_short,
  "SAHLGRENSKA AKADEMIN",                                                                                           "Sahlgrenska",
  "NATURVETENSKAPLIGA FAKULTETEN",                                                                                  "Nat.sci / IT",
  "IT-FAKULTETEN",                                                                                                  "Nat.sci / IT",
  "FAKULTETEN f\u00f6r NATURVETENSKAP o TEKNIK",                                                                   "Nat.sci / IT",
  "KONSTN\u00c4RLIGA FAULTETEN / HUMANISTISKA FAKULTETEN / SAMH\u00c4LLSVETENSKAPLIGA FAKULTETEN / HANDELSH\u00d6GSKOLAN", "Humanities / Soc.sci / Arts / Business",
  "KONSTN\u00c5RLIGA FAULTETEN / HUMANISTISKA FAKULTETEN / SAMH\u00c4LLSVETENSKAPLIGA FAKULTETEN / HANDELSH\u00d6GSKOLAN", "Humanities / Soc.sci / Arts / Business",
  "UTBILDNINGSVETENSKAPLIGA FAKULTETEN",                                                                             "Education"
)

# ── Build a group_id for cross-year continuity ────────────────────────────────
# Within NATURVETENSKAP / IT, map letters to consistent group descriptions
# (In 2025, IT moved from its own faculty to letter C in the merged faculty.)
natit_group_map <- tribble(
  ~faculty_raw,                                            ~group_letter, ~group_id,
  "NATURVETENSKAPLIGA FAKULTETEN",                         "A",           "NatSci – Fysik/Matematik",
  "NATURVETENSKAPLIGA FAKULTETEN",                         "B",           "NatSci – Övriga",
  "IT-FAKULTETEN",                                         "A",           "IT – Samtliga",
  "FAKULTETEN f\u00f6r NATURVETENSKAP o TEKNIK",          "A",           "NatSci – Fysik/Matematik",
  "FAKULTETEN f\u00f6r NATURVETENSKAP o TEKNIK",          "B",           "NatSci – Övriga",
  "FAKULTETEN f\u00f6r NATURVETENSKAP o TEKNIK",          "C",           "IT – Samtliga"
)

# ── Pivot to long format ──────────────────────────────────────────────────────
long <- raw %>%
  pivot_longer(
    cols      = c(salary_0pct, salary_50pct, salary_80pct, salary_100pct),
    names_to  = "level",
    values_to = "salary"
  ) %>%
  filter(!is.na(salary)) %>%
  mutate(
    level = recode(level,
      salary_0pct   = "0%",
      salary_50pct  = "50%",
      salary_80pct  = "80%",
      salary_100pct = "100%"
    ),
    level = factor(level, levels = c("0%", "50%", "80%", "100%"))
  ) %>%
  # Attach short faculty label
  left_join(faculty_map, by = c("faculty" = "faculty_raw")) %>%
  mutate(faculty_short = coalesce(faculty_short, faculty)) %>%
  # Attach cross-year group_id for NatSci/IT, use group_name for others
  left_join(natit_group_map, by = c("faculty" = "faculty_raw", "group_letter")) %>%
  mutate(
    group_id = if_else(is.na(group_id),
                       paste0(group_letter, ") ", str_trunc(group_name, 40)),
                       group_id)
  )

# ── Colour scales ─────────────────────────────────────────────────────────────
level_colours <- c(
  "0%"   = GU_GREY,
  "50%"  = GU_CYAN,
  "80%"  = GU_BLUE,
  "100%" = GU_YELLOW
)

level_linetypes <- c(
  "0%"   = "dotted",
  "50%"  = "dashed",
  "80%"  = "solid",
  "100%" = "solid"
)

# ── Helper: pick last-year label positions ────────────────────────────────────
end_labels <- long %>%
  filter(year == max(year)) %>%
  group_by(faculty_short, group_id, level) %>%
  summarise(salary = last(salary), year = max(year), .groups = "drop")

# ============================================================================
# PLOT 1 – Overview: 80% salary by faculty and group, 2022–2025
# ============================================================================
p_overview <- long %>%
  filter(level == "80%") %>%
  ggplot(aes(x = year, y = salary, colour = group_id, group = group_id)) +
  geom_line(linewidth = 0.9, show.legend = FALSE) +
  geom_point(size = 2.2, show.legend = FALSE) +
  geom_text_repel(
    data = long %>% filter(level == "80%", year == max(year)),
    aes(label = group_id),
    hjust        = 0,
    nudge_x      = 0.15,
    size         = 2.8,
    direction    = "y",
    segment.size = 0.3,
    segment.color = GU_GREY,
    show.legend  = FALSE
  ) +
  facet_wrap(~ faculty_short, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = 2022:2025, limits = c(2022, 2026.5)) +
  scale_y_continuous(labels = label_number(big.mark = "\u00a0", suffix = " kr")) +
  labs(
    title    = "PhD salary at 80% completion \u2013 G\u00f6teborgs Universitet 2022\u20132025",
    subtitle = "Negotiated monthly salary (SEK) per faculty and PhD group",
    x        = NULL,
    y        = "Monthly salary (SEK)",
    caption  = "Source: Lokalt avtal om l\u00f6ness\u00e4ttningsprinciper f\u00f6r doktorander (GU)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", colour = GU_BLUE, size = 14),
    plot.subtitle     = element_text(colour = GU_GREY, margin = margin(b = 10)),
    plot.caption      = element_text(colour = GU_GREY, size = 8),
    strip.text        = element_text(face = "bold", colour = GU_BLUE, size = 10),
    strip.background  = element_rect(fill = GU_LIGHTGREY, colour = NA),
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(colour = GU_LIGHTGREY),
    axis.text         = element_text(colour = GU_GREY),
    axis.title.y      = element_text(colour = GU_GREY, margin = margin(r = 8))
  )

# ============================================================================
# PLOT 2 – All salary levels for each faculty/group (spaghetti by level)
#          Shows the full step-ladder progression per group over time.
# ============================================================================
make_faculty_plot <- function(fac_label) {
  d <- long %>% filter(faculty_short == fac_label)
  n_groups <- n_distinct(d$group_id)

  group_colours <- setNames(
    colorRampPalette(c(GU_BLUE, GU_CYAN, GU_TEAL, GU_RED, GU_YELLOW, GU_GREY))(n_groups),
    unique(d$group_id)
  )

  ggplot(d, aes(x = year, y = salary,
                colour = group_id, linetype = level, group = interaction(group_id, level))) +
    geom_line(linewidth = 0.7, alpha = 0.85) +
    geom_point(size = 1.8) +
    scale_x_continuous(breaks = 2022:2025) +
    scale_y_continuous(labels = label_number(big.mark = " ", suffix = " kr")) +
    scale_colour_manual(values = group_colours, name = "Group") +
    scale_linetype_manual(values = level_linetypes, name = "Level") +
    labs(
      title = fac_label,
      x = NULL, y = "Monthly salary (SEK)"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", colour = GU_BLUE, size = 11),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = GU_LIGHTGREY),
      legend.position  = "right",
      legend.key.width = unit(1.5, "cm"),
      legend.text      = element_text(size = 7),
      legend.title     = element_text(size = 8, face = "bold")
    )
}

faculties_ordered <- c(
  "Sahlgrenska",
  "Nat.sci / IT",
  "Humanities / Soc.sci / Arts / Business",
  "Education"
)

plots_by_faculty <- map(faculties_ordered, make_faculty_plot)

p_detail <- wrap_plots(plots_by_faculty, ncol = 2) +
  plot_annotation(
    title    = "PhD salary progression by level – Göteborgs Universitet 2022–2025",
    subtitle = "All salary levels (0 %, 50 %, 80 %, 100%) shown per group",
    caption  = "Source: Lokalt avtal om lönessättningsprinciper för doktorander (GU)\nNote: 2022 Sahlgrenska 100%-level was set individually (individuell) — not shown.\nNote: From 2025, NATURVETENSKAPLIGA FAKULTETEN and IT-FAKULTETEN merged.",
    theme    = theme(
      plot.title    = element_text(face = "bold", colour = GU_BLUE, size = 14),
      plot.subtitle = element_text(colour = GU_GREY),
      plot.caption  = element_text(colour = GU_GREY, size = 7.5)
    )
  )

# ============================================================================
# PLOT 3 – Percent salary increase 2022 → 2025 at 80% level (bar chart)
# ============================================================================
pct_change <- long %>%
  filter(level == "80%", year %in% c(2022, 2025)) %>%
  select(year, faculty_short, group_id, salary) %>%
  pivot_wider(names_from = year, values_from = salary) %>%
  filter(!is.na(`2022`) & !is.na(`2025`)) %>%
  mutate(
    pct_change = (`2025` - `2022`) / `2022` * 100,
    group_label = str_wrap(group_id, 25)
  ) %>%
  arrange(faculty_short, desc(pct_change))

p_change <- ggplot(pct_change,
                   aes(x = reorder(group_label, pct_change),
                       y = pct_change, fill = faculty_short)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = sprintf("+%.1f%%", pct_change)),
            hjust = -0.1, size = 3, colour = GU_GREY) +
  coord_flip(clip = "off") +
  scale_fill_manual(
    values = c(
      "Sahlgrenska"                            = GU_BLUE,
      "Nat.sci / IT"                           = GU_CYAN,
      "Humanities / Soc.sci / Arts / Business" = GU_TEAL,
      "Education"                              = GU_YELLOW
    ),
    name = "Faculty"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Cumulative salary increase 2022 → 2025 at 80% completion",
    subtitle = "Percentage growth in monthly salary",
    x        = NULL,
    y        = "Salary increase (%)",
    caption  = "Source: GU negotiated PhD salary agreements"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title        = element_text(face = "bold", colour = GU_BLUE, size = 13),
    plot.subtitle     = element_text(colour = GU_GREY),
    plot.caption      = element_text(colour = GU_GREY, size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "right"
  )

# ── Save outputs ──────────────────────────────────────────────────────────────
# Use png/svg devices directly to avoid a ggplot2 3.5.1 add_guides bug with ggsave.
save_plot <- function(p, filename, width_in, height_in, dpi = 180) {
  # PNG
  png_path <- file.path(output_dir, paste0(filename, ".png"))
  png(png_path, width = width_in * dpi, height = height_in * dpi, res = dpi, bg = "white")
  print(p)
  dev.off()
  # SVG
  svg_path <- file.path(output_dir, paste0(filename, ".svg"))
  svg(svg_path, width = width_in, height = height_in, bg = "white")
  print(p)
  dev.off()
  message("Saved: ", png_path)
}

save_plot(p_overview, "01_overview_80pct",       width_in = 13, height_in = 9)
save_plot(p_detail,   "02_detail_all_levels",     width_in = 15, height_in = 11)
save_plot(p_change,   "03_pct_increase_2022_2025", width_in = 11, height_in = 7)

message("\nDone! All plots saved to: ", output_dir)
