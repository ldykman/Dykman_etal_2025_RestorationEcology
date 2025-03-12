### NURSERY KELP LENGTHS FOR DYKMAN ET AL 2025 GIANT KELP RECIPROCAL TRANSPLANT STUDY ###

# This script runs statistical analyses for the impact of number of parents and source site on green gravel growth in the nursery.
# Lauren Dykman
# Created: March 05, 2023
# Last Edited: Feb 03, 2025

rm(list=ls())

# Installing packages

#install.packages("tidyverse")
#install.packages("hrbrthemes")
#install.packages("viridis")
#install.packages("gdtools")
#install.packages("lme4")

library(tidyverse)
library(hrbrthemes)
library(viridis)
library(lme4)

# Setting working directory

path <- "/Users/laurendykman/Desktop/github/Dykman_etal_2025_RestorationEcology"

getwd()
setwd(path)
getwd()

# Importing data

input_file = paste0("green_gravel_nursery_data_2023.csv")
data <- read.csv(paste(path, "data", input_file, sep = "/"), header=TRUE)

data$gravel <- as.character(data$gravel)
data$length_mm <- as.numeric(data$length_mm)
data$date <- as.Date(data$date)
data$population <- factor(data$population, levels = c("warm", "cool"))

data$date[data$date == "2023-03-04"] <- "2023-03-03"

data$days_old <- as.character(data$date - as.Date("2023-02-07"))

# Subset date range

data.april <- data[data$date == "2023-04-04" & !is.na(data$length_mm),]

# Count number of data points for treatment

counts <- data.april %>%
  group_by(parent_number, population) %>%
  tally()

counts$parent_temp <- paste(counts$parent_number, counts$population)

# Calculate the mean and standard deviation for each treatment

summary <- data.april %>%
  group_by(parent_number, population) %>%
  summarise_at(vars(length_mm),
               list(mean = ~mean(as.numeric(.), na.rm = TRUE),
                    sd = ~sd(as.numeric(.), na.rm = TRUE)))

summary$parent_temp <- paste(summary$parent_number, summary$population)

# Merge the counts of population data with standard deviation and calculate standard error for each population

data.error <- merge(counts[c("parent_temp", "n")], summary, by = "parent_temp")
data.error$se <-data.error$sd/sqrt(data.error$n)

ggsave(paste(path, "Figures", paste0("Figure_Length_Nursery_SourcePopulation_ParentNumber_April_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(data.error, aes(fill = population, x=parent_number, y=mean)) +
         geom_bar(stat = "identity", position = position_dodge(), color = "black") +
         geom_errorbar(aes(x = parent_number,
                           ymin = ifelse(mean - se <= 0, 0, mean - se),
                           ymax = mean + se), linewidth = 0.5, width = 0.4, stat = "identity", position = position_dodge(0.9)) +
         xlab("number of parents") +
         ylab("total length (cm)") +
         theme_bw() +
         labs(fill = "source\npopulation") +
         labs(color = "source\npopulation") +
         theme(text=element_text(size = 20),
               aspect.ratio=0.6,
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"),
               legend.title = element_text(size=18),
               legend.text = element_text(size=16),
               panel.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA)), height=5, width=7) # Setting the theme

# Count number of data points for each location (to use in standard error)

counts <- data.april %>%
  group_by(parent_number) %>%
  tally()

# Calculate the mean and standard deviation kelp length by parent number

summary <- data.april %>%
  group_by(parent_number) %>%
  summarise_at(vars(length_mm),
               list(mean = ~mean(as.numeric(.), na.rm = TRUE),
                    sd = ~sd(as.numeric(.), na.rm = TRUE)))

# Merge the counts of population data with standard deviation and calculate standard error for each population

data.error <- merge(counts, summary, by = "parent_number")
data.error$se <-data.error$sd/sqrt(data.error$n)

# Plot lengths of kelp with two vs ten parents in the nursery in April

ggsave(paste(path, "Figures", paste0("Figure_Length_Nursery_ParentNumber_April_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(data.error, aes(x=parent_number, y=mean)) +
         geom_bar(stat = "identity", position = position_dodge(), fill = "steelblue", color = "black", width = 0.5) +
         geom_errorbar(aes(x = parent_number,
                           ymin = ifelse(mean - se <= 0, 0, mean - se),
                           ymax = mean + se), linewidth = 0.5, width = 0.2, stat = "identity", position = position_dodge(0.9)) +
         xlab("number of parents") +
         ylab("total length (cm)") +
         theme_bw() +
         theme(text=element_text(size = 20),
               aspect.ratio=0.6,
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"),
               legend.title = element_text(size=18),
               legend.text = element_text(size=16),
               panel.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA)), height=6, width=6) # Setting the theme

glm.results <- glmer(length_mm ~ parent_number * population + (1|tank/gravel), data.april, family = Gamma)
summary(glm.results) # Significant interaction between parent number and source population
