# Load packages -----------------------------------------------------------

library(dplyr)
library(ggplot2)
library(scales)
library(cowplot)
library(grid)


# Load Data ---------------------------------------------------------------
#load in df
npp_variables <- read.csv("Derived Data/NPP Variables.csv") 

# Model 1- NPP Timeseries Plot Symbols ------------------------------------

# ─────────────────────────────────────────────
# 1. Build line pairs for all metrics
# ─────────────────────────────────────────────
line_pairs <- npp_variables %>%
  arrange(Site, kelp.year) %>%
  group_by(Site) %>%
  mutate(
    year_lead     = lead(kelp.year),
    rec_lead      = lead(KELP_RECRUITS_M2),
    biomass_lead  = lead(APRIL_BIOMASS),
    growth_lead   = lead(KELP_GROWTH),
    npp_lead      = lead(NPP_dry_per.year)
  ) %>%
  filter(!is.na(year_lead)) %>%
  ungroup()

# ─────────────────────────────────────────────
# 2. Define "same color group, slightly different shades" per plot
# ─────────────────────────────────────────────

# ─────────────────────────────────────────────
# Color palettes (original color = middle shade)
# ─────────────────────────────────────────────

# NPP (blue family)
npp_cols <- c(
  "ABUR" = "#1F4E79",   # darker blue
  "AQUE" = "blue",     # ORIGINAL blue (middle)
  "MOHK" = "#9CCAE1"   # lighter blue
)

# Biomass (purple family)
biomass_cols <- c(
  "ABUR" = "#4B0082",  # darker purple
  "AQUE" = "purple",  # ORIGINAL purple (middle)
  "MOHK" = "#C3A6E4"  # lighter purple
)

# Recruitment (orange family)
recruit_cols <- c(
  "ABUR" = "#C2410C",  # darker orange
  "AQUE" = "orange",  # ORIGINAL orange (middle)
  "MOHK" = "#FED7AA"  # lighter orange
)

# Growth (green family)
growth_cols <- c(
  "ABUR" = "#0B2E1A",     # much darker green (near-forest)
  "AQUE" = "darkgreen",  # ORIGINAL darkgreen (middle)
  "MOHK" = "#A7CDB5"     # muted sage (light)
)


shape_vals <- c(
  "ABUR" = 16,  # circle
  "AQUE" = 15,  # square
  "MOHK" = 17   # triangle
)

# ─────────────────────────────────────────────
# 3. NPP Plot
# ─────────────────────────────────────────────
ymax_npp <- ceiling(max(npp_variables$NPP_dry_per.year, na.rm = TRUE))

npp_year_plot <- ggplot() +
  geom_point(
    data = npp_variables,
    aes(kelp.year, NPP_dry_per.year, shape = Site, color = Site),
    size = 5, show.legend = FALSE
  ) +
  geom_segment(
    data = line_pairs %>% filter(!(kelp.year == 2019 & year_lead == 2021)),
    aes(x = kelp.year, y = NPP_dry_per.year,
        xend = year_lead, yend = npp_lead, color = Site),
    size = 1.5, show.legend = FALSE
  ) +
  geom_segment(
    data = line_pairs %>% filter(kelp.year == 2019 & year_lead == 2021),
    aes(x = kelp.year, y = NPP_dry_per.year,
        xend = year_lead, yend = npp_lead, color = Site),
    linetype = "dashed", size = 1.5, show.legend = FALSE
  ) +
  scale_color_manual(values = npp_cols) +
  scale_shape_manual(values = shape_vals) +
  ylab(bquote(atop('Daily NPP', ~'(kg dry mass'%.%~m^-2%.%y^-1~')'))) +
  xlab(NULL) +
  theme_classic(base_size = 22) +
  labs(tag = "(d)") +
  scale_y_continuous(
    limits = c(0, ymax_npp),
    breaks = seq(0, ymax_npp, length.out = 4),
    labels = function(x) paste0("  ", formatC(x, format = "f", digits = 2))
  ) +
  theme(
    text = element_text(family = "sans"),
    axis.text = element_text(color = "black", size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(color = "black", size = 20),
    plot.tag = element_text(size = 24, face = "bold")
  )

# ─────────────────────────────────────────────
# 4. Biomass Plot
# ─────────────────────────────────────────────
ymax_biomass <- max(npp_variables$APRIL_BIOMASS, na.rm = TRUE)
breaks_biomass <- seq(0, ymax_biomass, length.out = 4)

biomass_year_plot <- ggplot() +
  geom_point(
    data = npp_variables,
    aes(kelp.year, APRIL_BIOMASS, shape = Site, color = Site),
    size = 5, show.legend = FALSE
  ) +
  geom_segment(
    data = line_pairs %>% filter(!(kelp.year == 2019 & year_lead == 2021)),
    aes(x = kelp.year, y = APRIL_BIOMASS,
        xend = year_lead, yend = biomass_lead, color = Site),
    size = 1.5, show.legend = FALSE
  ) +
  geom_segment(
    data = line_pairs %>% filter(kelp.year == 2019 & year_lead == 2021),
    aes(x = kelp.year, y = APRIL_BIOMASS,
        xend = year_lead, yend = biomass_lead, color = Site),
    linetype = "dashed", size = 1.5, show.legend = FALSE
  ) +
  scale_color_manual(values = biomass_cols) +
  scale_shape_manual(values = shape_vals) +
  ylab(bquote(atop('Initial biomass', ~'(kg dry mass'%.%~m^-2~')'))) +
  xlab(NULL) +
  theme_classic(base_size = 22) +
  labs(tag = "(a)") +
  scale_y_continuous(
    limits = c(0, ymax_biomass),
    breaks = breaks_biomass,
    labels = scales::label_number(accuracy = 0.01)
  ) +
  theme(
    text = element_text(family = "sans"),
    axis.text = element_text(color = "black", size = 18),
    axis.title.y = element_text(color = "black", size = 20),
    axis.title.x = element_text(size = 20),
    plot.tag = element_text(size = 24, face = "bold")
  )

# ─────────────────────────────────────────────
# 5. Recruitment Plot
# ─────────────────────────────────────────────
ymax_recruits <- max(npp_variables$KELP_RECRUITS_M2, na.rm = TRUE)
breaks_recruits <- seq(0, ymax_recruits, length.out = 4)

recruit_year_plot <- ggplot() +
  geom_point(
    data = npp_variables,
    aes(kelp.year, KELP_RECRUITS_M2, shape = Site, color = Site),
    size = 5, show.legend = FALSE
  ) +
  geom_segment(
    data = line_pairs %>% filter(!(kelp.year == 2019 & year_lead == 2021)),
    aes(x = kelp.year, y = KELP_RECRUITS_M2,
        xend = year_lead, yend = rec_lead, color = Site),
    size = 1.5, show.legend = FALSE
  ) +
  geom_segment(
    data = line_pairs %>% filter(kelp.year == 2019 & year_lead == 2021),
    aes(x = kelp.year, y = KELP_RECRUITS_M2,
        xend = year_lead, yend = rec_lead, color = Site),
    linetype = "dashed", size = 1.5, show.legend = FALSE
  ) +
  scale_color_manual(values = recruit_cols) +
  scale_shape_manual(values = shape_vals) +
  ylab(bquote(atop('Recruitment', ~'(no. Plants'%.%~m^-2~')'))) +
  xlab(NULL) +
  theme_classic(base_size = 22) +
  labs(tag = "(b)") +
  scale_y_continuous(
    limits = c(0, ymax_recruits),
    breaks = breaks_recruits,
    labels = scales::label_number(accuracy = 0.01)
  ) +
  theme(
    text = element_text(family = "sans"),
    axis.text = element_text(color = "black", size = 18),
    axis.title.y = element_text(color = "black", size = 20),
    axis.title.x = element_text(size = 20),
    plot.tag = element_text(size = 24, face = "bold")
  )

# ─────────────────────────────────────────────
# 6. Growth Plot
# ─────────────────────────────────────────────
ymax_growth <- max(npp_variables$KELP_GROWTH, na.rm = TRUE)
breaks_growth <- seq(0, ymax_growth, length.out = 4)

growth_year_plot <- ggplot() +
  geom_point(
    data = npp_variables,
    aes(kelp.year, KELP_GROWTH, shape = Site, color = Site),
    size = 5, show.legend = FALSE
  ) +
  geom_segment(
    data = line_pairs %>% filter(!(kelp.year == 2019 & year_lead == 2021)),
    aes(x = kelp.year, y = KELP_GROWTH,
        xend = year_lead, yend = growth_lead, color = Site),
    size = 1.5, show.legend = FALSE
  ) +
  geom_segment(
    data = line_pairs %>% filter(kelp.year == 2019 & year_lead == 2021),
    aes(x = kelp.year, y = KELP_GROWTH,
        xend = year_lead, yend = growth_lead, color = Site),
    linetype = "dashed", size = 1.5, show.legend = FALSE
  ) +
  scale_color_manual(values = growth_cols) +
  scale_shape_manual(values = shape_vals) +
  ylab(bquote(atop('Mass specific', ~'growth rate ('~d^-1~')'))) +
  xlab(NULL) +
  theme_classic(base_size = 22) +
  labs(tag = "(c)") +
  scale_y_continuous(
    limits = c(0, ymax_growth),
    breaks = breaks_growth,
    labels = scales::label_number(accuracy = 0.01)
  ) +
  theme(
    text = element_text(family = "sans"),
    axis.text = element_text(color = "black", size = 18),
    axis.title.y = element_text(color = "black", size = 20),
    axis.title.x = element_text(size = 20),
    plot.tag = element_text(size = 24, face = "bold")
  )

# ─────────────────────────────────────────────
# 7. Shape-only legend in BLACK (no color legend)
# ─────────────────────────────────────────────
growth_year_plot2 <- ggplot(npp_variables, aes(kelp.year, KELP_GROWTH)) +
  geom_point(aes(shape = Site), color = "black", size = 5, show.legend = TRUE) +
  geom_line(aes(group = Site), color = "black", size = 1.5, show.legend = FALSE) +
  ylab(bquote(atop('Average Specific Growth Rate', ~'('~d^-1~')'))) +
  xlab(NULL) +
  theme_classic(base_size = 22) +
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  scale_shape_manual(
    values = shape_vals,
    labels = c("Arroyo Burro", "Arroyo Quemada", "Mohawk")
  ) +
  theme(
    text = element_text(family = "sans"),
    axis.text = element_text(color = "black", size = 18),
    axis.title = element_text(size = 18),
    plot.tag = element_text(size = 18, face = "bold")
  )

legend2 <- get_legend(growth_year_plot2)

# ─────────────────────────────────────────────
# 8. Final Arrangement
# ─────────────────────────────────────────────
plots_only <- cowplot::plot_grid(
  biomass_year_plot,
  recruit_year_plot,
  growth_year_plot,
  npp_year_plot,
  ncol = 1
)

x.grob <- ggdraw() +
  draw_label(
    "Growth year",
    fontfamily = "sans",
    fontface = "plain",
    size = 20,
    vjust = 1
  )

plots_with_xaxis <- cowplot::plot_grid(
  plots_only,
  x.grob,
  ncol = 1,
  rel_heights = c(1, 0.05)
)

final_plot <- cowplot::plot_grid(
  plots_with_xaxis, legend2,
  ncol = 2,
  rel_widths = c(2.5, 0.6)
)

# --- Title grob ---
title_grob_maintext <- textGrob(
  "Figure 2",
  x = 0, y = 1,
  just = c("left", "top"),
  gp = gpar(fontsize = 30, fontface = "bold", family = "sans")
)

# --- Combined figure using grid.arrange() ---
final_fig2 <- grid.arrange(
  title_grob_maintext,
  final_plot,
  ncol = 1,
  heights = c(0.05, 1)
)

print(final_fig2)
