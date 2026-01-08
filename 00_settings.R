
        #### 1. LOADING PACKAGES ####
        
if (!require("pacman")) install.packages("pacman")

pacman::p_load(dplyr, tidyr,tidyverse, ggplot2, data.table,readxl,
               lavaan,psych,parallel,purrr,stringr,lme4, lmerTest,tools,
               broom.mixed,ggplot2,ggpubr,gridExtra,parallel, corrplot, 
               performance, grid, reshape2)

        #### 2. PLOT SETTINGS ####

color_boys <- "#439ace"
color_girls <- "#c32929"

color_noncog <- "#518D3F"
color_cog <- "#7b3f8d"

#defining theme for ggplot
theme_sverdo <- function() {
  theme_classic(base_family = "serif") +
    theme(
      axis.text.x = element_text(size = 6),  # X-axis numbers
      axis.text.y = element_text(size = 6),  # Y-axis numbers  
      axis.title.x = element_text(size = 7), # X-axis title
      axis.title.y = element_text(size = 7), # Y-axis title
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 7),
      plot.title = element_text(size = 8),
      strip.text = element_text(size = 6),
      strip.background = element_blank(),
      panel.border = element_blank(),  # No border
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      legend.key.width = unit(0.5, "cm"),
      legend.key.height = unit(0.5, "cm"),
      legend.background = element_blank()
    )
}

plot_colors <- c(
  "Boys.Low (-2 SD)"   = "#1f5b83",
  "Girls.Low (-2 SD)"   = "#7a0f17",
  "Boys.Average"        = "#439ace",
  "Girls.Average"       = "#c32929",
  "Boys.High (+2 SD)"   = "#93c4e3",
  "Girls.High (+2 SD)"  = "#e48b8b"
)

plot_colors_boys <- c(
  "Boys.Low (-2 SD)"   = "#1f5b83",
  "Boys.Average"        = "#439ace",
  "Boys.High (+2 SD)"   = "#93c4e3"
)

plot_colors_girls <- c(
  "Girls.Low (-2 SD)"   = "#7a0f17",
  "Girls.Average"       = "#c32929",
  "Girls.High (+2 SD)"  = "#e48b8b"
)


#size for figure annotations
figure_annotation_size <- 2.5

        #### 3. MISC ####
options(scipen = 999)
