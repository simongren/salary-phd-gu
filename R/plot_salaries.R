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
# Maps (faculty_raw, group_letter) → canonical group_id so that OCR artefacts
# in 2025 group names don't create phantom new groups.
# Sahlgrenska group names are particularly affected by OCR (ö→o, Ö→G, etc.).
natit_group_map <- tribble(
  ~faculty_raw,                                            ~group_letter, ~group_id,
  # NatSci / IT (2022–2024 separate faculties → 2025 merged)
  "NATURVETENSKAPLIGA FAKULTETEN",                         "A",           "NatSci – Fysik/Matematik",
  "NATURVETENSKAPLIGA FAKULTETEN",                         "B",           "NatSci – \u00d6vriga",
  "IT-FAKULTETEN",                                         "A",           "IT – Samtliga",
  "FAKULTETEN f\u00f6r NATURVETENSKAP o TEKNIK",          "A",           "NatSci – Fysik/Matematik",
  "FAKULTETEN f\u00f6r NATURVETENSKAP o TEKNIK",          "B",           "NatSci – \u00d6vriga",
  "FAKULTETEN f\u00f6r NATURVETENSKAP o TEKNIK",          "C",           "IT – Samtliga",
  # Sahlgrenska (group names garbled by OCR in 2025)
  "SAHLGRENSKA AKADEMIN",                                  "A",           "Sahlgrenska A – Medicinsk basvet.",
  "SAHLGRENSKA AKADEMIN",                                  "B",           "Sahlgrenska B – Medicinsk basvet. m. l\u00e4karex.",
  "SAHLGRENSKA AKADEMIN",                                  "C",           "Sahlgrenska C – Medicinsk basvet. leg. l\u00e4kare",
  "SAHLGRENSKA AKADEMIN",                                  "D",           "Sahlgrenska D – \u00d6vriga",
  "SAHLGRENSKA AKADEMIN",                                  "E",           "Sahlgrenska E – \u00d6vriga m. l\u00e4karex.",
  "SAHLGRENSKA AKADEMIN",                                  "F",           "Sahlgrenska F – \u00d6vriga leg. l\u00e4kare"
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
  ) %>%
  # Safety: strip residual OCR transposition artefacts (e.g. "373 00") from group labels
  mutate(group_id = str_squish(str_remove(group_id, "\\s*\\d{3}\\s+\\d{2}\\s*$")))

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
  facet_wrap(~ faculty_short, ncol = 2) +
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

# ============================================================================
# INDEXED PLOTS  (2022 = 100)
# Standardise each group×level salary so 2022 = 100, then track development.
# A Swedish CPIF reference line (Oct-to-Oct) is overlaid for comparison.
# ============================================================================

# ── Swedish CPIF reference (Oct-to-Oct, base Oct 2022 = 100) ─────────────────
# Source: Statistics Sweden (SCB), CPIF (inflation excl. mortgage interest)
# Oct22→Oct23: +6.5 %   Oct23→Oct24: +1.6 %   Oct24→Oct25: +1.2 % (prelim.)
cpif <- tibble(
  year  = 2022:2025,
  cpif  = c(100.0,
            100.0 * 1.065,
            100.0 * 1.065 * 1.016,
            100.0 * 1.065 * 1.016 * 1.012)
)

# ── Build indexed long dataset ────────────────────────────────────────────────
# Use an explicit left_join on the 2022 baseline so groups without a 2022 value
# (e.g. Sahlgrenska/IT individuell 100%-level) are cleanly excluded.
base_year <- long %>%
  filter(year == 2022) %>%
  select(faculty_short, group_id, level, base_salary = salary)

indexed <- long %>%
  left_join(base_year, by = c("faculty_short", "group_id", "level")) %>%
  filter(!is.na(base_salary)) %>%
  mutate(index = salary / base_salary * 100)

# ── Faculty colour palette (consistent across indexed plots) ──────────────────
faculty_colours <- c(
  "Sahlgrenska"                            = GU_BLUE,
  "Nat.sci / IT"                           = GU_CYAN,
  "Humanities / Soc.sci / Arts / Business" = GU_TEAL,
  "Education"                              = GU_YELLOW
)

# ============================================================================
# PLOT 4 – Indexed salary at 80% level, all groups, with CPIF reference
# ============================================================================
p_index_80 <- indexed %>%
  filter(level == "80%") %>%
  ggplot(aes(x = year, y = index,
             colour = faculty_short, group = group_id)) +
  # CPIF band / reference line drawn first (behind salary lines)
  geom_line(data = cpif, aes(x = year, y = cpif),
            inherit.aes = FALSE,
            colour = GU_RED, linewidth = 1.1, linetype = "dashed") +
  annotate("text", x = 2025.05, y = cpif$cpif[4] + 0.4,
           label = "CPIF\n(inflation)", hjust = 0, size = 2.8,
           colour = GU_RED, lineheight = 0.9) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 2) +
  # Label each line at 2025
  geom_text_repel(
    data = indexed %>% filter(level == "80%", year == 2025),
    aes(label = group_id),
    hjust = 0, nudge_x = 0.1, size = 2.6, direction = "y",
    segment.size = 0.25, segment.colour = GU_GREY,
    show.legend = FALSE
  ) +
  geom_hline(yintercept = 100, linetype = "dotted", colour = GU_GREY, linewidth = 0.5) +
  scale_x_continuous(breaks = 2022:2025, limits = c(2022, 2027.2)) +
  scale_y_continuous(
    labels = function(x) paste0(x),
    breaks = seq(98, 116, by = 2)
  ) +
  scale_colour_manual(values = faculty_colours, name = "Faculty") +
  labs(
    title    = "Indexed PhD salary at 80% completion vs. inflation \u2013 GU 2022\u20132025",
    subtitle = "Index: 2022 = 100 per group. Dashed red = Swedish CPIF (Oct-to-Oct).",
    x        = NULL,
    y        = "Index (2022 = 100)",
    caption  = "Source: GU salary agreements; SCB CPIF (Oct-to-Oct). 2025 CPIF preliminary."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", colour = GU_BLUE, size = 13),
    plot.subtitle    = element_text(colour = GU_GREY, margin = margin(b = 10)),
    plot.caption     = element_text(colour = GU_GREY, size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = GU_LIGHTGREY),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 9),
    axis.text        = element_text(colour = GU_GREY)
  )

# ============================================================================
# PLOT 5 – Indexed salary, all levels, faceted by faculty  (with CPIF)
# Each panel shows every group×level combination for that faculty.
# ============================================================================
make_index_faculty_plot <- function(fac_label) {
  d        <- indexed %>% filter(faculty_short == fac_label)
  n_groups <- n_distinct(d$group_id)

  group_colours <- setNames(
    colorRampPalette(c(GU_BLUE, GU_CYAN, GU_TEAL, GU_RED, GU_YELLOW, GU_GREY))(n_groups),
    unique(d$group_id)
  )

  ggplot(d, aes(x = year, y = index,
                colour = group_id, linetype = level,
                group = interaction(group_id, level))) +
    geom_line(data = cpif, aes(x = year, y = cpif),
              inherit.aes = FALSE,
              colour = GU_RED, linewidth = 0.9, linetype = "dashed", alpha = 0.7) +
    geom_hline(yintercept = 100, linetype = "dotted", colour = GU_GREY, linewidth = 0.4) +
    geom_line(linewidth = 0.65, alpha = 0.85) +
    geom_point(size = 1.6) +
    scale_x_continuous(breaks = 2022:2025) +
    scale_y_continuous(labels = function(x) paste0(x),
                       breaks = seq(98, 118, by = 2)) +
    scale_colour_manual(values = group_colours, name = "Group") +
    scale_linetype_manual(values = level_linetypes, name = "Level") +
    labs(title = fac_label, x = NULL, y = "Index (2022 = 100)") +
    theme_minimal(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", colour = GU_BLUE, size = 11),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = GU_LIGHTGREY),
      legend.position  = "right",
      legend.key.width = unit(1.2, "cm"),
      legend.text      = element_text(size = 7),
      legend.title     = element_text(size = 8, face = "bold")
    )
}

index_plots_by_faculty <- map(faculties_ordered, make_index_faculty_plot)

p_index_detail <- wrap_plots(index_plots_by_faculty, ncol = 2) +
  plot_annotation(
    title    = "Indexed PhD salary (all levels) vs. inflation \u2013 GU 2022\u20132025",
    subtitle = "Index: 2022 = 100 per group \u00d7 level. Dashed red = Swedish CPIF (Oct-to-Oct).",
    caption  = paste0(
      "Source: GU salary agreements; SCB CPIF Oct-to-Oct: +6.5% (2023), +1.6% (2024), +1.2% (2025 prelim.).\n",
      "Note: 2022 Sahlgrenska 100%-level was individual \u2014 no baseline, excluded from index."
    ),
    theme = theme(
      plot.title    = element_text(face = "bold", colour = GU_BLUE, size = 14),
      plot.subtitle = element_text(colour = GU_GREY),
      plot.caption  = element_text(colour = GU_GREY, size = 7.5)
    )
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

save_plot(p_overview,      "01_overview_80pct",            width_in = 13, height_in = 9)
save_plot(p_detail,        "02_detail_all_levels",         width_in = 15, height_in = 11)
save_plot(p_change,        "03_pct_increase_2022_2025",    width_in = 11, height_in = 7)
save_plot(p_index_80,      "04_index_80pct_vs_cpif",       width_in = 13, height_in = 8)
save_plot(p_index_detail,  "05_index_all_levels_vs_cpif",  width_in = 15, height_in = 11)

# ============================================================================
# PLOT 6 – Spotlight: "\u00d6vriga doktorander" in Humanities / Soc.sci
#          Gray = all other groups; blue = highlighted group; red = CPIF.
#          Four panels (one per salary level), indexed 2022 = 100.
# ============================================================================
HIGHLIGHT_FACULTY <- "Humanities / Soc.sci / Arts / Business"

spotlight <- indexed %>%
  mutate(
    is_highlight = faculty_short == HIGHLIGHT_FACULTY &
                   str_detect(group_id, regex("\u00d6vriga|\u00f6vriga|Ovriga|ovriga", ignore_case = TRUE))
  )

# Index values for the highlighted group at year 2025 (for label annotation)
spotlight_labels <- spotlight %>%
  filter(is_highlight, year == 2025)

# The four levels as labelled panels
level_labels <- c("0%" = "Entry (0%)", "50%" = "Mid (50%)", "80%" = "Senior (80%)", "100%" = "Post-defence (100%)")

p_spotlight <- ggplot() +
  # ── Gray background: all other groups ──────────────────────────────────────
  geom_line(
    data    = spotlight %>% filter(!is_highlight),
    mapping = aes(x = year, y = index,
                  group = interaction(faculty_short, group_id, level)),
    colour  = "#CCCCCC", linewidth = 0.4, alpha = 0.85
  ) +
  # ── CPIF reference line ─────────────────────────────────────────────────────
  geom_line(
    data    = cpif,
    mapping = aes(x = year, y = cpif),
    inherit.aes = FALSE,
    colour  = GU_RED, linewidth = 1.0, linetype = "dashed"
  ) +
  # ── Horizontal baseline ─────────────────────────────────────────────────────
  geom_hline(yintercept = 100, linetype = "dotted", colour = GU_GREY, linewidth = 0.4) +
  # ── Highlighted group ───────────────────────────────────────────────────────
  geom_line(
    data    = spotlight %>% filter(is_highlight),
    mapping = aes(x = year, y = index,
                  group = interaction(faculty_short, group_id, level)),
    colour  = GU_BLUE, linewidth = 1.3
  ) +
  geom_point(
    data    = spotlight %>% filter(is_highlight),
    mapping = aes(x = year, y = index),
    colour  = GU_BLUE, size = 2.8
  ) +
  # ── Index value label at 2025 ───────────────────────────────────────────────
  geom_label(
    data    = spotlight_labels,
    mapping = aes(x = year, y = index, label = sprintf("%.1f", index)),
    hjust   = -0.12, size = 3.2, colour = GU_BLUE,
    fill    = "white", linewidth = 0.3, label.padding = unit(0.18, "lines"),
    label.r = unit(0.1, "lines")
  ) +
  # ── CPIF label (once, in the first panel only) ───────────────────────────── 
  geom_label(
    data    = cpif %>% filter(year == 2025) %>% mutate(level = factor("0%", levels = levels(indexed$level))),
    mapping = aes(x = year, y = cpif, label = "CPIF"),
    hjust   = -0.12, size = 2.8, colour = GU_RED,
    fill    = "white", linewidth = 0.3, label.padding = unit(0.15, "lines"),
    label.r = unit(0.1, "lines")
  ) +
  facet_wrap(
    ~ level, ncol = 2,
    labeller = as_labeller(level_labels)
  ) +
  scale_x_continuous(breaks = 2022:2025, limits = c(2022, 2026.1)) +
  scale_y_continuous(
    labels = function(x) paste0(x),
    breaks = seq(98, 120, by = 2)
  ) +
  labs(
    title    = "Spotlight: \u00d6vriga doktorander \u2013 Humanities / Soc.sci / Arts / Business",
    subtitle = paste0(
      "Index 2022 = 100. \u2014  ",
      "\u25ac\u25ac Blue = \u00d6vriga doktorander (Hum/Soc.sci)    ",
      "\u25ac\u25ac Red dashed = CPIF (inflation)    ",
      "\u25ac\u25ac Gray = all other PhD groups"
    ),
    x       = NULL,
    y       = "Index (2022 = 100)",
    caption = "Source: GU salary agreements; SCB CPIF Oct-to-Oct: +6.5% (2023), +1.6% (2024), +1.2% (2025 prelim.)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", colour = GU_BLUE, size = 14),
    plot.subtitle     = element_text(colour = GU_GREY, size = 9, margin = margin(b = 10)),
    plot.caption      = element_text(colour = GU_GREY, size = 8),
    strip.text        = element_text(face = "bold", colour = GU_BLUE, size = 11),
    strip.background  = element_rect(fill = GU_LIGHTGREY, colour = NA),
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(colour = GU_LIGHTGREY),
    axis.text         = element_text(colour = GU_GREY),
    axis.title.y      = element_text(colour = GU_GREY, margin = margin(r = 8))
  )

save_plot(p_spotlight, "06_spotlight_ovriga_hum_socsci", width_in = 12, height_in = 9)

# ============================================================================
# PLOT 7 – Spotlight (absolute SEK): "Övriga doktorander" in Hum / Soc.sci
#          Same layout as Plot 6 but y-axis = actual monthly salary in SEK.
#          CPIF reference is the 2022 baseline salary × cumulative inflation.
# ============================================================================

# Build per-level CPIF reference in SEK from the Övriga 2022 baseline salary
ovriga_base_sek <- long %>%
  filter(
    faculty_short == HIGHLIGHT_FACULTY,
    str_detect(group_id, regex("\u00d6vriga|\u00f6vriga|Ovriga|ovriga", ignore_case = TRUE)),
    year == 2022
  ) %>%
  select(level, base_salary = salary)

cpif_sek <- ovriga_base_sek %>%
  crossing(cpif) %>%
  mutate(cpif_salary = base_salary * cpif / 100)

spotlight_sek <- long %>%
  mutate(
    is_highlight = faculty_short == HIGHLIGHT_FACULTY &
                   str_detect(group_id, regex("\u00d6vriga|\u00f6vriga|Ovriga|ovriga", ignore_case = TRUE))
  )

spotlight_sek_labels <- spotlight_sek %>%
  filter(is_highlight, year == 2025)

p_spotlight_sek <- ggplot() +
  # ── Gray background: all other groups ──────────────────────────────────────
  geom_line(
    data    = spotlight_sek %>% filter(!is_highlight),
    mapping = aes(x = year, y = salary,
                  group = interaction(faculty_short, group_id, level)),
    colour  = "#CCCCCC", linewidth = 0.4, alpha = 0.85
  ) +
  # ── CPIF reference line (inflation-adjusted 2022 salary) ───────────────────
  geom_line(
    data    = cpif_sek,
    mapping = aes(x = year, y = cpif_salary, group = level),
    inherit.aes = FALSE,
    colour  = GU_RED, linewidth = 1.0, linetype = "dashed"
  ) +
  # ── Highlighted group ───────────────────────────────────────────────────────
  geom_line(
    data    = spotlight_sek %>% filter(is_highlight),
    mapping = aes(x = year, y = salary,
                  group = interaction(faculty_short, group_id, level)),
    colour  = GU_BLUE, linewidth = 1.3
  ) +
  geom_point(
    data    = spotlight_sek %>% filter(is_highlight),
    mapping = aes(x = year, y = salary),
    colour  = GU_BLUE, size = 2.8
  ) +
  # ── Salary label at 2025 ────────────────────────────────────────────────────
  geom_label(
    data    = spotlight_sek_labels,
    mapping = aes(x = year, y = salary,
                  label = paste0(format(salary, big.mark = "\u00a0", scientific = FALSE), " kr")),
    hjust   = -0.08, size = 3.0, colour = GU_BLUE,
    fill    = "white", linewidth = 0.3, label.padding = unit(0.18, "lines"),
    label.r = unit(0.1, "lines")
  ) +
  # ── CPIF label (once, in the 0% panel only) ─────────────────────────────── 
  geom_label(
    data    = cpif_sek %>% filter(year == 2025, level == "0%"),
    mapping = aes(x = year, y = cpif_salary, label = "CPIF\nbaseline"),
    hjust   = -0.08, size = 2.6, colour = GU_RED,
    fill    = "white", linewidth = 0.3, label.padding = unit(0.15, "lines"),
    label.r = unit(0.1, "lines")
  ) +
  facet_wrap(
    ~ level, ncol = 2, scales = "free_y",
    labeller = as_labeller(level_labels)
  ) +
  scale_x_continuous(breaks = 2022:2025, limits = c(2022, 2026.2)) +
  scale_y_continuous(
    labels = function(x) paste0(format(x, big.mark = "\u00a0", scientific = FALSE), " kr")
  ) +
  labs(
    title    = "Spotlight: \u00d6vriga doktorander \u2013 Humanities / Soc.sci / Arts / Business",
    subtitle = paste0(
      "Monthly salary in SEK. \u2014  ",
      "\u25ac\u25ac Blue = \u00d6vriga doktorander (Hum/Soc.sci)    ",
      "\u25ac\u25ac Red dashed = 2022 salary \u00d7 CPIF (inflation baseline)    ",
      "\u25ac\u25ac Gray = all other PhD groups"
    ),
    x       = NULL,
    y       = "Monthly salary (SEK)",
    caption = "Source: GU salary agreements; SCB CPIF Oct-to-Oct: +6.5% (2023), +1.6% (2024), +1.2% (2025 prelim.)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", colour = GU_BLUE, size = 14),
    plot.subtitle     = element_text(colour = GU_GREY, size = 9, margin = margin(b = 10)),
    plot.caption      = element_text(colour = GU_GREY, size = 8),
    strip.text        = element_text(face = "bold", colour = GU_BLUE, size = 11),
    strip.background  = element_rect(fill = GU_LIGHTGREY, colour = NA),
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(colour = GU_LIGHTGREY),
    axis.text         = element_text(colour = GU_GREY),
    axis.title.y      = element_text(colour = GU_GREY, margin = margin(r = 8))
  )

save_plot(p_spotlight_sek, "07_spotlight_ovriga_hum_socsci_sek", width_in = 12, height_in = 9)

message("\nDone! All plots saved to: ", output_dir)
