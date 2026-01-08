

# Load packages -----------------------------------------------------------

library(ggplot2)
library(dplyr)
library(ggeffects)
library(MASS)
library(cowplot)
library(scales)
library(DHARMa)
library(grid)
library(gridExtra)



# Load Data ---------------------------------------------------------------
#load in df
npp_variables <- read.csv("Derived Data/NPP Variables.csv") 

# Model 2- Biomass Plot redone ---------------------------------------------------

# 1. Fit linear model
biomass_m2 <- lm(APRIL_BIOMASS ~ l_lagged + URCHIN_GRAZING_AVE + Site, 
                 data = npp_variables, na.action = na.omit)

# 2. Generate model predictions
model_predictions1 <- ggpredict(biomass_m2, terms = c("l_lagged"))

# 3. Determine shared ymax across both plots
ymax_shared <- max(
  max(npp_variables_allsites$APRIL_BIOMASS, na.rm = TRUE),
  max(npp_variables$APRIL_BIOMASS, na.rm = TRUE)
)

y_breaks <- seq(0, ymax_shared, length.out = 4)

biomass_plot2 <- ggplot(npp_variables_allsites, aes(l_lagged, APRIL_BIOMASS, shape = Site)) +
  geom_point(color = "purple", size = 5, show.legend = FALSE) +
  geom_line(
    data = model_predictions1,
    aes(x = x, y = predicted, group = group),
    color = "purple", inherit.aes = FALSE
  ) +
  geom_ribbon(
    data = model_predictions1,
    aes(x = x, ymin = conf.low, ymax = conf.high, group = group),
    fill = "purple", alpha = 0.3, inherit.aes = FALSE
  ) +
  scale_shape_manual(values = c(
    "ABUR" = 16,  # circle
    "AQUE" = 15,  # square
    "MOHK" = 17   # triangle
  )) +
  xlab(bquote(atop("Winter loss rate", (d^{-1})))) +
  ylab(bquote('Initial biomass (kg dry mass'%.%~m^-2~')')) +
  scale_y_continuous(
    breaks = y_breaks,
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_x_continuous(
    labels = scales::label_number(accuracy = 0.01)
  ) +
  coord_cartesian(ylim = c(0, ymax_shared)) +
  theme_classic(base_size = 25) +
  theme(
    text = element_text(family = "sans"),
    axis.text = element_text(colour = "black", size = 25),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(color = "black", size = 25),
    plot.tag = element_text(size = 29, face = "bold"),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25)
  ) +
  annotate("text", x = 0.08, y = ymax_shared * 0.9,
           label = "Partial~R^{2} == 0.25",
           parse = TRUE,
           size = 7,
           fontface = "bold",
           family = "sans")

# Model 3- Loss Plot Redone ------------------------------------------------------

# Get ymax for y-axis only
ymax_loss <- max(npp_variables_allsites$l_lagged, na.rm = TRUE)

y_breaks <- seq(0, ymax_shared, length.out = 4)

loss_plot <- ggplot(npp_variables_allsites, aes(LARGE_WAVES_MAX, l_lagged)) +
  geom_point(aes(shape = Site), color = "purple", size = 5, show.legend = FALSE) +
  stat_smooth(method = "lm", color = "purple", fill = "purple", alpha = 0.3, linewidth = 0.8, se = TRUE) +
  scale_shape_manual(values = c(
    "ABUR" = 16,  # circle
    "AQUE" = 15,  # square
    "MOHK" = 17   # triangle
  )) +
  ylab(expression("Winter loss rate ("*d^-1*")")) +
  xlab(expression(atop("Maximum wave height", "(m)"))) +
  theme_classic(base_size = 18) +
  theme(
    legend.position = "none",
    text = element_text(family = "sans"),
    axis.text = element_text(size = 25, color = "black"),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(color = "black", size = 25),
    plot.tag = element_text(size = 25, face = "bold"),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25)
  ) +
  scale_y_continuous(
    limits = c(0, ymax_loss),
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.02, 0.05)),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  annotate("text", x = 1.55, y = 0.15, 
           label = "R^2 == 0.08", 
           parse = TRUE, 
           size = 8, 
           fontface = "bold", 
           family = "sans",
           color = "black")


# combine for biomass -----------------------------------------------------

biomass_2 <- cowplot::plot_grid(biomass_plot2, loss_plot, ncol = 1)

biomass_2

# Step 3: Create a split title row with "A" (left) and "Initial Biomass" (center)
label_left <- ggplot() +
  annotate("text", x = 0, y = 1, label = "(a)",
           hjust = 0, vjust = 1,
           fontface = "bold", family = "sans", size = 10,
           color = "black") +
  theme_void()

title_center <- ggplot() +
  annotate("text", x = 0.5, y = 1, label = "Initial Biomass",
           hjust = 0.5, vjust = 1,
           fontface = "bold", family = "sans", size = 10,
           color = "black") +
  theme_void()

# Step 4: Stack both text pieces into one row
title_row <- cowplot::plot_grid(label_left, title_center, ncol = 2, rel_widths = c(0.2, 0.8))

# Step 5: Stack the title row above the bordered plot
final_plot <- cowplot::plot_grid(
  title_row,
  biomass_2,
  ncol = 1,
  rel_heights = c(0.08, 0.92)
)

# Step 6: Display or export
print(final_plot)

# recruitment fix ---------------------------------------------------------


# --- Data Preparation ---
npp_variables_allsites <- npp_variables%>% 
  dplyr::mutate(REC_1000 = KELP_RECRUITS_M2 * 1000) %>% 
  dplyr::mutate(REC_round = round(REC_1000, digits = 0))

rec.df <- npp_variables_allsites %>% 
  dplyr::select(Site, kelp.year, REC_round, KELP_RECRUITS_M2, SAND_COVER, URCHIN_GRAZING_AVE, 
                TEMP_THRESHOLD_DAY, NITRATE_THRESHOLD_RECRUIT, APRIL_BIOMASS) %>% na.omit()

recruit_m4 <- glm.nb(REC_round  ~ SAND_COVER + URCHIN_GRAZING_AVE +
                       TEMP_THRESHOLD_DAY + NITRATE_THRESHOLD_RECRUIT + 
                       APRIL_BIOMASS + 
                       Site, data = npp_variables_allsites, na.action = na.omit)

# --- Sand Plot ---
model_predictions1 <- ggpredict(recruit_m4, terms = "SAND_COVER") %>%
  as_tibble() %>%
  mutate(across(predicted:conf.high, ~ . / 1000))

sand_plot <- ggplot(rec.df, aes(SAND_COVER, KELP_RECRUITS_M2, shape = Site)) +
  geom_point(color = "orange", size = 5, show.legend = FALSE) +
  geom_line(data = model_predictions1,
            aes(x = x, y = predicted),
            color = "orange",
            inherit.aes = FALSE) +
  geom_ribbon(data = model_predictions1,
              aes(x = x, ymin = conf.low, ymax = conf.high),
              fill = "orange", alpha = 0.3,
              inherit.aes = FALSE) +
  ylab(bquote('Recruitment (no. plants'%.%~m^-2~')')) +
  xlab(bquote(atop("Sand cover", "(%)"))) + 
  scale_shape_manual(values = c("ABUR" = 16, "AQUE" = 15, "MOHK" = 17)) +
  theme_classic(base_size = 25) +
  scale_y_continuous(
    limits = c(0, 1.5),
    breaks = seq(0, 1.5, length.out = 4),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  theme(
    text = element_text(family = "sans"),
    axis.text.y = element_text(color = "black", size = 25),
    axis.title.y = element_text(color = "black", size = 25),
    axis.ticks.y = element_line(color = "black"),
    axis.text.x = element_text(color = "black", size = 25),
    axis.title.x = element_text(color = "black", size = 25),
    plot.tag = element_text(size = 29, face = "bold"),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25)
  ) +
  annotate("text", x = 50, y = 1.3, 
           label = "Partial~R^{2} == 0.06", 
           parse = TRUE, 
           size = 7, 
           fontface = "bold", 
           family = "sans",
           color = "black")



# --- Biomass Plot ---
model_predictions1 <- ggpredict(recruit_m4, terms = "APRIL_BIOMASS") %>%
  as_tibble()

model_predictions1 <- model_predictions1 %>%
  mutate(across(predicted:conf.high, ~ . / 1000))


# Define axis breaks
xmax_biomass <- max(rec.df$APRIL_BIOMASS, na.rm = TRUE)
x_breaks <- seq(0, xmax_biomass, length.out = 5)
y_breaks <- seq(0, 1.5, length.out = 4)

biomass_plot2 <- ggplot(rec.df, aes(APRIL_BIOMASS, KELP_RECRUITS_M2, shape = Site)) +
  geom_point(color = "orange", size = 5, show.legend = FALSE) +
  geom_line(data = model_predictions1,
            aes(x = x, y = predicted),
            color = "orange",
            linewidth = 0.8,
            inherit.aes = FALSE) +
  geom_ribbon(data = model_predictions1,
              aes(x = x, ymin = conf.low, ymax = conf.high),
              fill = "orange", alpha = 0.3,
              inherit.aes = FALSE) +
  xlab(bquote(atop('Initial biomass', ~'(kg dry mass'%.%~m^-2~')'))) +
  ylab(bquote('Recruitment (no. plants'%.%~m^-2~')')) +
  scale_shape_manual(values = c("ABUR" = 16, "AQUE" = 15, "MOHK" = 17)) +
  scale_y_continuous(
    breaks = y_breaks,
    labels = scales::label_number(accuracy = 0.1)
  ) +
  scale_x_continuous(
    limits = c(0, xmax_biomass),
    breaks = x_breaks,
    labels = scales::label_number(accuracy = 0.1)
  ) +
  coord_cartesian(ylim = c(0, 1.5)) +
  theme_classic(base_size = 25) +
  theme(
    text = element_text(family = "sans"),
    axis.text.x = element_text(color = "black", size = 25),
    axis.text.y = element_text(color = "black", size = 25),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(color = "black", size = 25),
    plot.tag = element_text(size = 29, face = "bold"),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25)
  ) +
  annotate("text", x = 0.52, y = 1.3, 
           label = "Partial~R^{2} == 0.14", 
           parse = TRUE, 
           size = 7, 
           fontface = "bold", 
           family = "sans",
           color = "black")



# --- Combine Plots ---
rec2 <- cowplot::plot_grid(
  biomass_plot2, sand_plot,
  ncol = 1
)



# recruitment fix plot ----------------------------------------------------

library(ggplot2)
library(cowplot)
library(grid)

# Step 1: Combine your two plots for this panel
rec2 <- cowplot::plot_grid(biomass_plot2, sand_plot, ncol = 1)



# Step 3: Create split title row: "B" on the left, "Recruitment" centered
label_left_B <- ggplot() +
  annotate("text", x = 0, y = 1, label = "(b)",
           hjust = 0, vjust = 1,
           fontface = "bold", family = "sans", size = 10,
           color = "black") +
  theme_void()

title_center_B <- ggplot() +
  annotate("text", x = 0.5, y = 1, label = "Recruitment",
           hjust = 0.5, vjust = 1,
           fontface = "bold", family = "sans", size = 10,
           color = "black") +
  theme_void()

# Step 4: Stack label and title together as one title row
title_row_B <- cowplot::plot_grid(label_left_B, title_center_B, ncol = 2, rel_widths = c(0.2, 0.8))

# Step 5: Stack title row above the bordered plot
final_plot_B <- cowplot::plot_grid(
  title_row_B,
  rec2,
  ncol = 1,
  rel_heights = c(0.08, 0.92)
)

# Step 6: Display or save
print(final_plot_B)
# ggsave("panel_B_recruitment.png", final_plot_B, width = 8, height = 10, dpi = 300)


# growth fix --------------------------------------------------------------

#biomass

growth_m1 <- lm(KELP_GROWTH ~ PLANT_DENSITY_M2 + PLANT_SIZE +
                  NITRATE_THRESHOLD_ANNUAL + TEMP_THRESHOLD_DAY_annual +
                  Site, data = npp_variables, na.action = na.omit)



summary(growth_m1)
simulationOutput <- simulateResiduals(fittedModel = growth_m1) #shows normality and variance
plot(simulationOutput)

growth_plot.df <- npp_variables %>% 
  dplyr::select(Site, kelp.year, KELP_GROWTH, PLANT_DENSITY_M2, PLANT_SIZE, NITRATE_THRESHOLD_ANNUAL, TEMP_THRESHOLD_DAY_annual)


# Shared ymax for all plots
ymax <- max(growth_plot.df$KELP_GROWTH, na.rm = TRUE)
y_breaks <- seq(0, ymax, length.out = 4)  # force 4 breaks including ymax

# --- Biomass Plot ---
xmax_biomass <- max(growth_plot.df$PLANT_SIZE, na.rm = TRUE)
x_breaks_biomass <- seq(0, xmax_biomass, length.out = 4)

model_predictions1 <- ggpredict(growth_m1, terms = c("PLANT_SIZE"))
annual_size_plot <- ggplot(growth_m1, aes(PLANT_SIZE, KELP_GROWTH, shape = Site)) +
  geom_point(color = "darkgreen", size = 5, show.legend = FALSE) +
  geom_line(data = model_predictions1,
            aes(x = x, y = predicted),
            color = "darkgreen",
            linewidth = 0.8,
            inherit.aes = FALSE) +
  geom_ribbon(data = model_predictions1,
              aes(x = x, ymin = conf.low, ymax = conf.high),
              fill = "darkgreen", alpha = 0.3,
              inherit.aes = FALSE) +
  ylab(bquote("Mass specific growth rate ("~d^-1~")")) +
  xlab(bquote(atop('Plant size',
                   ~'(no. fronds'%.% ~plant^-1~')'))) + 
  scale_shape_manual(values = c(
    "ABUR" = 16,  # circle
    "AQUE" = 15,  # square
    "MOHK" = 17   # triangle
  )) +
  scale_y_continuous(
    limits = c(0, 0.06),
    breaks = seq(0, 0.06, length.out = 4),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_x_continuous(
    breaks = c(0, 20, 40, 60), 
    labels = scales::label_number(accuracy = 1.0)
  ) +
  theme_classic(base_size = 25) + 
  theme(
    text = element_text(family = "sans"),
    axis.text.x = element_text(color = "black", size = 25),
    axis.text.y = element_text(color = "black", size = 25),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(color = "black", size = 25),
    plot.tag = element_text(size = 29, face = "bold"),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25)
  ) +
  annotate("text", x = 30, y = 0.058, 
           label = "Partial~R^{2} == 0.15", 
           parse = TRUE, 
           size = 7, 
           fontface = "bold", 
           family = "sans",
           color = "black")

annual_size_plot



# legend ------------------------------------------------------------------

growth_year_plot3 <- ggplot(npp_variables, aes(kelp.year, KELP_GROWTH)) +
  geom_point(aes(shape = Site), color = "black", size = 5, show.legend = TRUE) +
  geom_line(aes(group = Site), color = "black", size = 1.5, show.legend = FALSE) +
  ylab(bquote(atop('Average Specific Growth Rate',
                   ~'('~d^-1~')'))) +
  xlab(NULL) +
  theme_classic(base_size = 25) +
  guides(
    shape = guide_legend(override.aes = list(size = 5))
  ) +
  scale_shape_manual(
    values = c("ABUR" = 16, "AQUE" = 15, "MOHK" = 17),
    labels = c("Arroyo Burro", "Arroyo Quemada", "Mohawk")
  ) +
  theme(
    text = element_text(family = "sans"),
    axis.text = element_text(color = "black", size = 25),
    axis.title = element_text(size = 25),
    plot.tag = element_text(size = 25, face = "bold")
  )


legend3 <- get_legend(growth_year_plot3)

# growth fix plot ---------------------------------------------------------

library(ggplot2)
library(cowplot)
library(grid)

growth_with_legend <- cowplot::plot_grid(
  annual_size_plot,
  legend3,
  ncol = 1,
  rel_heights = c(0.5, 0.5)
)

# Step 2: Create the top row with label and title
label_left_C <- ggplot() +
  annotate("text", x = 0, y = 1, label = "(c)",
           hjust = 0, vjust = 1,
           fontface = "bold", family = "sans", size = 10,
           color = "black") +
  theme_void()

title_center_C <- ggplot() +
  annotate("text", x = 0.5, y = 1, label = "Growth",
           hjust = 0.5, vjust = 1,
           fontface = "bold", family = "sans", size = 10,
           color = "black") +
  theme_void()

# Step 3: Combine label and title into one row
title_row_C <- cowplot::plot_grid(label_left_C, title_center_C, ncol = 2, rel_widths = c(0.2, 0.8))

# Step 4: Stack title and plot with border
final_plot_C <- cowplot::plot_grid(
  title_row_C,
  growth_with_legend,
  ncol = 1,
  rel_heights = c(0.08, 0.92)
)

# Step 5: Display or export
print(final_plot_C)
# ggsave("panel_C_kelp_growth.png", final_plot_C, width = 8, height = 8, dpi = 300)




# combine -----------------------------------------------------------------

combined_all_panels <- cowplot::plot_grid(
  final_plot,
  final_plot_B,
  final_plot_C,
  ncol = 3,
  align = "v"
)

# --- Title grob ---
title_grob_multip <- textGrob(
  "Figure S1",
  x = 0, y = 1,
  just = c("left", "top"),
  gp = gpar(fontsize = 30, fontface = "bold", family = "sans")
)

# --- Combined figure using grid.arrange() ---
final_fig_S1 <- grid.arrange(
  title_grob_multip,
  combined_all_panels,
  ncol = 1,
  heights = c(0.05, 1)
)

