### TEMPERATURE PROFILES FOR DYKMAN ET AL 2025 GIANT KELP RECIPROCAL TRANSPLANT STUDY ###

# This script plots depth profiles of temperature and salinity at the research sites over time.
# Lauren Dykman
# Created: March 5, 2023
# Last Modified: Jan 31, 2025

rm(list=ls())

# INSTALLING PACKAGES

#install.packages("tidyverse")
#install.packages("hrbrthemes")
#install.packages("viridis")
#install.packages("gdtools")
#install.packages("lme4")
#install.packages("ggfortify")

library(tidyverse)
library(hrbrthemes)
library(viridis)
library(lme4)
library(ggfortify)
library(survival)

# Setting working directory

path <- "/Users/laurendykman/Desktop/github/Dykman_etal_2025_RestorationEcology"

getwd()
setwd(path)
getwd()

# Importing data

input_file = "ysi_measurements_2023.csv"
data <- read.csv(paste(path, "data", input_file, sep = "/"), header=TRUE)

data$month <- c(rep(NA, dim(data)[1]))
data$month[data$date %in% c("2023-03-07", "2023-03-09")] <- "March"
data$month[data$date %in% c("2023-04-04", "2023-04-03", "2023-04-05", "2023-04-06")] <- "April"
data$month[data$date %in% c("2023-05-18", "2023-05-19")] <- "May"
data$month[data$date %in% c("2023-06-28")] <- "June"
data$month[data$date %in% c("2023-08-30")] <- "August"

data$site[data$site == "DX"] <- "Dixon"
data$site[data$site == "EK"] <- "Ed King"

data$month <- factor(data$month, levels = c("March", "April", "May", "June", "August"))

data <- data[!data$date %in% c("2023-04-04", "2023-04-03"),]
data <- data[!is.na(data$salinity_ppt) & !data$month == "March",]

# Plotting temperature as a function of depth and reverse axis

ggsave(paste(path, "Figures", paste0("Figure_Temperature_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
ggplot(data, aes(colour = site, x = depth_m, y = temp_C, shape = date)) +
  geom_line(linewidth = 2) + # Set up the asthetics, the line must be broken because this is technically discrete data
  theme_bw() +
  xlab("depth (m)") +
  ylab("temperature (C)") +
  facet_grid(rows = vars(month)) +
  scale_x_reverse() + # Reverse depth so it starts at zero
  scale_y_continuous(position="right") + # Put the y axis labels on the opposite side so when its flipped it will appear at top
  coord_flip() + # This is how you reverse the look and order or the coordinates for the graph
  theme(text = element_text(size = 20),
        aspect.ratio=0.8,
        legend.key = element_rect(colour = "transparent", fill = NA),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)), height=9, width=7)
