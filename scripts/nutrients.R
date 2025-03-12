### NUTRIENT ANALYSIS FOR DYKMAN ET AL 2025 GIANT KELP RECIPROCAL TRANSPLANT STUDY ###

# Lauren Dykman
# Created: April 24, 2024
# Last Edited: March 11, 2025

rm(list=ls())

# INSTALLING PACKAGES

#install.packages("ggplot2")
#install.packages("tidyverse")

library(ggplot2)
library(tidyverse)

# SETTING WORKING DIRECTORY

path <- "/Users/laurendykman/Desktop/github/Dykman_etal_2025_RestorationEcology"

getwd()
setwd(path)
getwd()

# IMPORTING DATA

input_file = "nutrient_analysis_barkley_2023.csv"
data <- read.csv(paste(path, "data", input_file, sep = "/"), header=TRUE)

data$date <- factor(data$date, levels = c("March", "April", "May", "June", "August"))
data$depth <- factor(data$depth, levels = c("top", "bottom"))
data$site[data$site == "Dixon"] <- "warm"
data$site[data$site == "Ed King"] <- "cool"
data$site <- factor(data$site, levels = c("warm", "cool"))

summary <- data %>%
  group_by(site, date, depth) %>%
  summarise_at(vars(Nitrate_plus_Nitrite),
               list(mean = ~mean(as.numeric(.), na.rm = TRUE),
                    sd = ~sd(as.numeric(.), na.rm = TRUE)))

summary$se <-summary$sd/sqrt(3)

ggsave(paste(path, "Figures", paste0("Figure_Nutrients_Nitrate_plus_Nitrite_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(summary, aes(fill = site, x=date, y=mean)) +
         geom_bar(stat = "identity", position = position_dodge(), color = "black") +
         geom_errorbar(aes(x = date,
                           ymin = ifelse(mean - se <= 0, 0, mean - se),
                           ymax = mean + se), linewidth = 0.5, width = 0.4, stat = "identity", position = position_dodge(0.9)) +
         xlab("month") +
         ylab("nitrate + nitrite [µm]") +
         ggtitle("Nutrients") +
         theme_bw() +
         ylim(0,13) +
         facet_grid(rows = vars(depth)) +
         theme(text=element_text(size = 16),
               aspect.ratio=0.25,
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"),
               panel.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA)), height=6, width=7) # Setting the theme

ggsave(paste(path, "Figures", paste0("Figure_Nutrients_Silicate_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(data, aes(x=date, y=Silicate, fill=site)) + 
         geom_boxplot() +
         ggtitle("Silicate") +
         facet_grid(rows = vars(depth)))

ggsave(paste(path, "Figures", paste0("Figure_Nutrients_Phosphate_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(data, aes(x=date, y=Phosphate, fill=site)) + 
         geom_boxplot() +
         ggtitle("Phosphate") +
         facet_grid(rows = vars(depth)))
