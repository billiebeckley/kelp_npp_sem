# load libraries ----------------------------------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(performance)
library(DHARMa)
library(cowplot)
library(glmmTMB)
library(gridExtra)
library(car)
library(extrafont)  # Required for system fonts
library(grid)

# Load system fonts (only needed once)
loadfonts(device = "win")  # Windows users
loadfonts(device = "pdf")  # If saving as PDF

# Load Data ---------------------------------------------------------------
#load in df
npp_variables <- read.csv("Derived Data/NPP Variables.csv") 



# S2 -----------------------------------------------------------------

# 1. Fit linear model
biomass_m2 <- lm(APRIL_BIOMASS ~ l_lagged + URCHIN_GRAZING_AVE + Site, 
                 data = npp_variables, na.action = na.omit)

# 2. Generate model predictions
model_predictions1 <- ggpredict(biomass_m2, terms = c("l_lagged"))

# 3. Determine shared ymax across both plots
ymax_shared <- max(
  max(npp_variables$APRIL_BIOMASS, na.rm = TRUE),
  max(npp_variables$APRIL_BIOMASS, na.rm = TRUE)
)

y_breaks <- seq(0, ymax_shared, length.out = 4)
# 5. Urchin plot


urchin_plot <- ggplot(npp_variables, aes(URCHIN_GRAZING_AVE, APRIL_BIOMASS)) +
  geom_point(aes(shape = Site), color = "purple", size = 5, show.legend = TRUE) +
  ggtitle("Figure S2") +   # ← ADD TITLE ABOVE FIGURE
  ylab(bquote('Initial biomass (kg dry mass'%.%~m^-2~')')) +
  xlab(bquote('Grazing capacity (g kelp consumed'%.%~m^-2~d^-1~')')) +
  scale_y_continuous(breaks = y_breaks, labels = scales::label_number(accuracy = 0.01)) +
  coord_cartesian(ylim = c(0, ymax_shared)) +
  theme_classic(base_size = 25) +
  guides(size = FALSE, color = guide_legend(override.aes = list(size = 5))) +
  scale_color_discrete(labels = c('Arroyo Burro', 'Arroyo Quemada', 'Mohawk')) +
  scale_shape_manual(
    values = c("ABUR" = 16, "AQUE" = 15, "MOHK" = 17),
    labels = c("Arroyo Burro", "Arroyo Quemada", "Mohawk")
  ) +
  theme(
    text = element_text(family = "sans"),
    axis.text = element_text(colour = "black", size = 25),
    axis.title = element_text(size = 25),
    
    # Title formatting (top-left by default)
    plot.title = element_text(size = 30, face = "bold", hjust = 0),
    
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25)
  )



# S3 -------------------------------------------------------------

# --- Data Preparation ---
npp_variables <- npp_variables %>% 
  dplyr::mutate(REC_1000 = KELP_RECRUITS_M2 * 1000) %>% 
  dplyr::mutate(REC_round = round(REC_1000, digits = 0))

rec.df <- npp_variables %>% 
  dplyr::select(Site, kelp.year, REC_round, KELP_RECRUITS_M2, SAND_COVER, URCHIN_GRAZING_AVE, 
                TEMP_THRESHOLD_DAY, NITRATE_THRESHOLD_RECRUIT, APRIL_BIOMASS) %>% na.omit()


# --- Urchin Plot ---

# Define 5 x-axis breaks
urchin_max <- max(rec.df$URCHIN_GRAZING_AVE, na.rm = TRUE)
urchin_breaks <- seq(0, urchin_max, length.out = 5)

urchin_plot2 <- ggplot(rec.df, aes(URCHIN_GRAZING_AVE, KELP_RECRUITS_M2)) +
  geom_point(aes(shape = Site), color = "orange", size = 5, show.legend = FALSE) +
  ylab(element_blank()) +
  xlab(bquote(atop('Grazing capacity', ~ '(g kelp consumed'%.%~m^-2~d^-1~')'))) + 
  scale_x_continuous(
    breaks = urchin_breaks,
    limits = c(0, urchin_max),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  scale_shape_manual(values = c(
    "ABUR" = 16,  # circle
    "AQUE" = 15,  # square
    "MOHK" = 17   # triangle
  )) +
  theme_classic(base_size = 25) +
  labs(tag = "(a)") +
  ylim(0, 1.5) +
  theme(
    text = element_text(family = "sans"),
    axis.text = element_text(color = "black", size = 25),
    axis.title = element_text(size = 25),
    plot.tag = element_text(size = 29, face = "bold"),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25)
  )


# --- Temp Plot ---
temp_plot <- ggplot(rec.df, aes(TEMP_THRESHOLD_DAY, KELP_RECRUITS_M2)) +
  geom_point(aes(shape = Site), color = "orange", size = 5, show.legend = FALSE) +
  ylab(element_blank()) +
  xlab(bquote(atop('Thermal stress',
                   '(no. days > 19'*degree*'C)'))) +  
  theme_classic(base_size = 25) +
  labs(tag = "(b)") +
  ylim(0, 1.5) +
  scale_shape_manual(values = c(
    "ABUR" = 16,  # circle
    "AQUE" = 15,  # square
    "MOHK" = 17   # triangle
  )) +
  theme(
    text = element_text(family = "sans"),
    axis.text = element_text(color = "black", size = 25),
    axis.title = element_text(size = 25),
    plot.tag = element_text(size = 29, face = "bold"),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25)
  )

# --- Nitrate Plot ---
nitrate_max <- max(rec.df$NITRATE_THRESHOLD_RECRUIT, na.rm = TRUE)
nitrate_breaks <- seq(0, nitrate_max, length.out = 5)

nitrate_plot <- ggplot(rec.df, aes(NITRATE_THRESHOLD_RECRUIT, KELP_RECRUITS_M2)) +
  geom_point(aes(shape = Site), color = "orange", size = 5, show.legend = FALSE) +
  ylab(element_blank()) +
  xlab(bquote(atop('Nitrate stress', ~'(no. days < 1'*mu~'mol'%.%~L^-1~')'))) +
  theme_classic(base_size = 25) +
  labs(tag = "(c)") +
  scale_shape_manual(values = c(
    "ABUR" = 16,  # circle
    "AQUE" = 15,  # square
    "MOHK" = 17   # triangle
  )) +
  scale_x_continuous(
    limits = c(0, 100),  # enough room for full CI
    breaks = seq(0, 100, length.out = 5),
    labels = scales::label_number(accuracy = 1.0)
  ) +
  ylim(0, 1.5) +
  theme(
    text = element_text(family = "sans"),
    axis.text = element_text(color = "black", size = 25),
    axis.title = element_text(size = 25),
    plot.tag = element_text(size = 29, face = "bold"),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25)
  )

# legend color
growth_year_plot_or <- ggplot(npp_variables, aes(kelp.year, KELP_GROWTH)) +
  geom_point(aes(shape = Site), color = "orange", size = 5, show.legend = TRUE) +
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


legend_or <- get_legend(growth_year_plot_or)

# --- Combine Plots ---
rec2 <- cowplot::plot_grid(
  urchin_plot2,
  temp_plot, nitrate_plot, legend_or,
  ncol = 2)

# Shared Y-axis label
y.grob <- textGrob(
  bquote('Recruitment (no. plants'%.%~m^-2~')'), 
  gp = gpar(fontsize = 25, fontfamily = "sans"),
  rot = 90
)


# Display final figure
grid.arrange(arrangeGrob(rec2, left = y.grob))

# --- Title grob ---
title_grob <- textGrob(
  "Figure S3",
  x = 0, y = 1,
  just = c("left", "top"),
  gp = gpar(fontsize = 30, fontface = "bold", family = "sans")
)

# --- Combined figure using grid.arrange() ---
final_fig_S3 <- grid.arrange(
  title_grob,
  arrangeGrob(rec2, left = y.grob),
  ncol = 1,
  heights = c(0.05, 1)   # adjust title vs figure space
)



# S4 ------------------------------------------------------------------

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
y_breaks <- seq(0, ymax, length.out = 4)


annual_density_plot <- ggplot(growth_plot.df, aes(PLANT_DENSITY_M2, KELP_GROWTH)) +
  geom_point(aes(shape = Site), color = "darkgreen", size = 5, show.legend = FALSE) +
  ylab(element_blank()) +
  xlab(bquote(atop('Plant density',
                   ~'(no.'%.%~m^-2~')'))) + 
  scale_y_continuous(
    limits = c(0, 0.06),  # enough room for full CI
    breaks = seq(0, 0.06, length.out = 4),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_x_continuous(
    limits = c(0, 25),  # enough room for full CI
    breaks = seq(0, 24, length.out = 4),
    labels = scales::label_number(accuracy = 1.0)
  ) +
  scale_shape_manual(values = c(
    "ABUR" = 16,  # circle
    "AQUE" = 15,  # square
    "MOHK" = 17   # triangle
  )) +
  theme_classic(base_size = 25) + 
  labs(tag = "(a)") +
  # Apply Serif to all text elements
  theme(
    text = element_text(family = "sans"),
    axis.text = element_text(colour = "black", size = 25),
    axis.title = element_text(size = 25),
    plot.tag = element_text(size = 29, face = "bold"),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25)
  )

annual_density_plot

temp_annual_plot <- ggplot(growth_plot.df, aes(TEMP_THRESHOLD_DAY_annual, KELP_GROWTH)) +
  geom_point(aes(shape = Site), color = "darkgreen", size = 5, show.legend = FALSE) +
  ylab(element_blank()) +
  xlab(bquote(atop('Thermal stress',
                   '(no. days > 19'*degree*'C)'))) +  
  scale_y_continuous(
    limits = c(0, 0.06),  # enough room for full CI
    breaks = seq(0, 0.06, length.out = 4),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_y_continuous(
    limits = c(0, 0.06),  # enough room for full CI
    breaks = seq(0, 0.06, length.out = 4),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_x_continuous(
    limits = c(0, 90),  # enough room for full CI
    breaks = seq(0, 75, length.out = 4),
    labels = scales::label_number(accuracy = 1.0)
  ) +
  scale_shape_manual(values = c(
    "ABUR" = 16,  # circle
    "AQUE" = 15,  # square
    "MOHK" = 17   # triangle
  )) +
  theme_classic(base_size = 25) + 
  labs(tag = "(b)") +
  # Apply Serif to all text elements
  theme(
    text = element_text(family = "sans"),
    axis.text = element_text(colour = "black", size = 25),
    axis.title = element_text(size = 25),
    plot.tag = element_text(size = 29, face = "bold"),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25)
  )

temp_annual_plot

n_annual_plot <- ggplot(growth_plot.df, aes(NITRATE_THRESHOLD_ANNUAL, KELP_GROWTH)) +
  geom_point(aes(shape = Site), color = "darkgreen", size = 5, show.legend = FALSE) +
  ylab(element_blank()) +
  xlab(bquote(atop('Nitrate stress',
                   '(no. days < 1 '*mu~'mol'%.%~L^-1~')'))) +  
  scale_y_continuous(
    limits = c(0, 0.06),  # enough room for full CI
    breaks = seq(0, 0.06, length.out = 4),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_x_continuous(
    limits = c(0, 300),  # enough room for full CI
    breaks = seq(0, 300, length.out = 4),
    labels = scales::label_number(accuracy = 1.0)
  ) +
  scale_shape_manual(values = c(
    "ABUR" = 16,  # circle
    "AQUE" = 15,  # square
    "MOHK" = 17   # triangle
  )) +
  theme_classic(base_size = 25) + 
  labs(tag = "(c)") +
  # Apply Serif to all text elements
  theme(
    text = element_text(family = "sans"),
    axis.text = element_text(colour = "black", size = 25),
    axis.title = element_text(size = 25),
    plot.tag = element_text(size = 29, face = "bold"),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25)
  )


n_annual_plot 

# legend color
growth_year_plot_gr <- ggplot(npp_variables, aes(kelp.year, KELP_GROWTH)) +
  geom_point(aes(shape = Site), color = "darkgreen", size = 5, show.legend = TRUE) +
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


legend_gr <- get_legend(growth_year_plot_gr)
# --- Combine Plots ---
growth2 <- cowplot::plot_grid(annual_density_plot, temp_annual_plot, n_annual_plot, legend_gr,
                              ncol = 2
)


# --- Y-axis Label ---
y.grob <- textGrob(
  bquote("Mass specific growth rate ("~d^-1~")"), 
  gp = gpar(fontsize = 25, fontfamily = "sans"),
  rot = 90
)

# --- Final Layout ---
grid.arrange(arrangeGrob(growth2, left = y.grob))

# --- Title grob ---
title_grob2 <- textGrob(
  "Figure S4",
  x = 0, y = 1,
  just = c("left", "top"),
  gp = gpar(fontsize = 30, fontface = "bold", family = "sans")
)

# --- Combined figure using grid.arrange() ---
final_fig_S4 <- grid.arrange(
  title_grob2,
  arrangeGrob(growth2, left = y.grob),
  ncol = 1,
  heights = c(0.05, 1)   # adjust title vs figure space
)

# S5 ----------------------------------------------

# Model
npp_m1 <- lm(NPP_dry_per.year ~ APRIL_BIOMASS + KELP_RECRUITS_M2 +
               KELP_GROWTH + Site, data = npp_variables, na.action = na.omit) 

# Clean data
npp_plot.df <- npp_variables %>% 
  dplyr::select(Site, kelp.year, NPP_dry_per.year, APRIL_BIOMASS, KELP_RECRUITS_M2, KELP_GROWTH) %>% 
  na.omit()

# Shared y-axis max
ymax <- max(npp_plot.df$NPP_dry_per.year)
y_breaks <- seq(0, ymax, length.out = 4)

# Color and shape mapping
site_shapes <- c("ABUR" = 16, "AQUE" = 15, "MOHK" = 17)

ggplot(npp_plot.df, aes(APRIL_BIOMASS, NPP_dry_per.year, shape = Site)) +
  geom_point(size = 5, color = "purple") +
  scale_shape_manual(values = c(
    "ABUR" = 16,
    "AQUE" = 15,
    "MOHK" = 17
  ))

# biomass plot
model_predictions1 <- ggpredict(npp_m1, terms = c("APRIL_BIOMASS"))
biomass_plot <- ggplot(npp_plot.df, aes(APRIL_BIOMASS, NPP_dry_per.year, shape = Site)) +
  geom_point(color = "purple", size = 5, show.legend = FALSE) +
  geom_line(
    data = model_predictions1,
    aes(x = x, y = predicted, group = group),
    color = "purple", inherit.aes = FALSE
  ) +
  geom_ribbon(
    data = model_predictions1,
    aes(x = x, ymin = conf.low, ymax = conf.high, group = group),
    fill = "purple", alpha = 0.5,
    inherit.aes = FALSE
  ) +
  scale_shape_manual(values = c(
    "ABUR" = 16,       # circle
    "AQUE" = 15,     # square
    "MOHK" = 17              # triangle
  )) +
  ylab(NULL) +
  xlab(bquote(atop('Initial biomass', ~'(kg dry mass'%.%~m^-2~')'))) + 
  scale_y_continuous(
    limits = c(0, 10.5),
    breaks = seq(0, 10, length.out = 5),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  scale_x_continuous(
    breaks = c(0, 0.37, 0.73, 1.1),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  theme_classic(base_size = 25) +
  labs(tag = "(a)") +
  theme(
    text = element_text(family = "sans"),
    axis.text = element_text(color = "black"),
    axis.title.x = element_text(color = "black", size = 25),
    plot.tag = element_text(size = 29, face = "bold")
  ) +
  annotate("text", x = 0.21, y = 10,
           label = "Partial~R^{2} == 0.46",
           parse = TRUE,
           size = 7,
           fontface = "bold",
           family = "sans")


# recruits plot
# Generate predictions for the recruitment variable
model_predictions_recruits <- ggpredict(npp_m1, terms = "KELP_RECRUITS_M2")

# Recruitment plot
recruits_plot <- ggplot(npp_plot.df, aes(KELP_RECRUITS_M2, NPP_dry_per.year, shape = Site)) +
  geom_point(color = "orange", size = 5, show.legend = FALSE) +
  geom_line(
    data = model_predictions_recruits,
    aes(x = x, y = predicted, group = group),
    color = "orange", inherit.aes = FALSE
  ) +
  geom_ribbon(
    data = model_predictions_recruits,
    aes(x = x, ymin = conf.low, ymax = conf.high, group = group),
    fill = "orange", alpha = 0.5,
    inherit.aes = FALSE
  ) +
  scale_shape_manual(values = c(
    "ABUR" = 16,       # circle
    "AQUE" = 15,       # square
    "MOHK" = 17        # triangle
  )) +
  ylab(NULL) +
  xlab(bquote(atop('Recruitment', ~'(no. plants'%.%~m^-2~')'))) + 
  scale_y_continuous(
    limits = c(0, 10.5),
    breaks = seq(0, 10, length.out = 5),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  scale_x_continuous(
    breaks = seq(0, 1.4, length.out = 4),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  theme_classic(base_size = 25) +
  labs(tag = "(b)") +
  theme(
    text = element_text(family = "sans"),
    axis.text = element_text(color = "black"),
    axis.title.x = element_text(size = 25, color = "black"),
    plot.tag = element_text(size = 29, face = "bold")
  ) +
  annotate("text", x = 0.25, y = 10,
           label = "Partial~R^{2} == 0.26",
           parse = TRUE,
           size = 7,
           fontface = "bold",
           family = "sans")



# growth plot
xmax_growth <- max(npp_plot.df$KELP_GROWTH, na.rm = TRUE)
x_breaks_growth <- seq(0, xmax_growth, length.out = 4)

growth_plot <- ggplot(npp_plot.df, aes(KELP_GROWTH, NPP_dry_per.year)) +
  geom_point(aes(shape = Site), color = "darkgreen", size = 5, show.legend = FALSE) +
  scale_shape_manual(values = c(
    "ABUR" = 16,  # circle
    "AQUE" = 15,  # square
    "MOHK" = 17   # triangle
  )) +
  ylab(NULL) +
  xlab(bquote(atop('Mass specific growth rate', ~'('~d^-1~')'))) + 
  theme_classic(base_size = 25) + 
  guides(size = FALSE) +
  coord_cartesian(xlim = c(0, 0.061)) +
  scale_y_continuous(
    limits = c(0, 10.5),
    breaks = seq(0, 10, length.out = 5),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  scale_x_continuous(
    breaks = seq(0, 0.06, length.out = 4),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(tag = "(c)") +
  theme(
    text = element_text(family = "sans"),
    axis.text = element_text(colour = "black"),
    axis.title.x = element_text(size = 25, color = "black"),
    axis.title.y = element_text(size = 25),
    plot.tag = element_text(size = 29, face = "bold")
  )


# combine plots
npp2 <- cowplot::plot_grid(
  biomass_plot, recruits_plot, growth_plot,
  legend2, ncol = 2, rel_widths = c(3, 3, 3, 0.4)
)

# y axis label
y.grob <- textGrob(
  bquote('Annual NPP (g dry mass'%.%~m^-2%.%~y^-1~')'), 
  gp = gpar(fontsize = 25, fontfamily = "sans", col = "black"),
  rot = 90
)


#final layout
grid.arrange(arrangeGrob(npp2, left = y.grob))

# --- Title grob ---
title_grob3 <- textGrob(
  "Figure S5",
  x = 0, y = 1,
  just = c("left", "top"),
  gp = gpar(fontsize = 30, fontface = "bold", family = "sans")
)

# --- Combined figure using grid.arrange() ---
final_fig_S5 <- grid.arrange(
  title_grob3,
  arrangeGrob(npp2, left = y.grob),
  ncol = 1,
  heights = c(0.05, 1)   # adjust title vs figure space
)

