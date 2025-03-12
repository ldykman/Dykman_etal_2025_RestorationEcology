### DATA TIDYING SCRIPT FOR DYKMAN ET AL 2025 GIANT KELP RECIPROCAL TRANSPLANT STUDY ###

# This script edits and tidies the raw green gravel data sheet and produces an output file data_all_tidy_yyyy-mm-dd.csv that will be used in subsequent analyses.
# This file will be saved to the output folder
# This script must be rerun every day you run analyses.

# Lauren Dykman
# Created: March 05, 2023
# Last Edited: March 07, 2025

rm(list=ls())

# Installing packages

#install.packages("tidyverse")

library(tidyverse)

# Setting working directory

path <- "/Users/laurendykman/Desktop/github/Dykman_etal_2025_RestorationEcology"

getwd()
setwd(path)
getwd()

# Importing data

input_file = "green_gravel_monitoring_data_2023.csv"
data <- read.csv(paste(path, "data", input_file, sep = "/"), header=TRUE)

# Setting some columns as numeric

data$replicate_number <- as.character(data$replicate_number)
data$length_mm <- as.numeric(data$length_mm)
data$urchins <- as.numeric(data$urchins)
data$date <- as.Date(data$date)

# Standardizing survey dates

data$days_old <- as.character(data$date - as.Date("2023-02-07"))
data$days_old[data$days_old %in% c("56", "57")] <- "55"
data$days_old[data$days_old %in% c("98", "99", "101")] <- "100"
data$days_old[data$days_old %in% c("139", "141")] <- "140"
data$days_old[data$days_old %in% c("202", "203")] <- "200"
data$days_old <- factor(data$days_old, levels = c("55", "100", "140", "200")) # Check unique(data$days_old) and write the days as factors here

# Changing site names

data$site[data$site == "Dixon"] <- "warm"
data$site[data$site == "Ed King"] <- "cool"
data$site <- factor(data$site, levels = c("warm", "cool"))
data$source_population <- factor(data$source_population, levels = c("warm", "cool"))

# Creating new columns for later analysis

data$total_length_cm <- data$length_mm/10
data$parent_temp_days <- paste(data$parent_number, data$source_population, data$days_old, sep = "_")
data$site_temp_days <- paste(data$site, data$source_population, data$days_old, sep = "_")
data$site_plot_id <- paste(data$site, data$plot_id, sep = "_")
data$site_replicate <- paste(data$site, data$replicate_number, sep = "_")
data$transplant <- paste(data$site, data$source_population, sep = "_")

# Changing some columns to factor

data$site_plot_id <- factor(data$site_plot_id)
data$parent_number <- factor(data$parent_number)
data$transplant <- factor(data$transplant)
data$site <- factor(data$site)

# Checking dataset

plots.surveyed <- data %>%
  group_by(days_old, site, source_population, parent_number, treatment) %>%
  tally()

# Removing control

data <- data[!data$treatment == "Control",]

# Saving a tidy data file in the output folder for use in subsequent analyses

filename = paste0("data_all_tidy_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
write.csv(data, paste(path, "Output", filename, sep = "/"))

# Saving a survivorship data table in the output folder for use in later analyses

data.surv <- data %>%
  group_by(site, site_plot_id, replicate_number, treatment, source_population, parent_number, days_old, number_gravels_remaining, number_gravels_with_kelp, urchins) %>%
  tally()

data.surv$site_replicate <- paste(data.surv$site, data.surv$replicate_number, sep = " ")
data.surv <- data.surv[order(data.surv$site_plot_id, data.surv$days_old),]

# Filling in some missing survey data assuming that when a plot went to zero kelp the number of gravels stayed the same for the rest of the study

for (i in c(1:dim(data.surv)[1])) {
  if (is.na(data.surv[i,8])) {
    data.surv[i,8] <- data.surv[i-1,8]
  }
}

# Make sure the column indexing number corresponds to the column titled "number_gravels_remaining"
# In this cases column 8, but you might need to change number

data.surv$survival <- data.surv$number_gravels_with_kelp/data.surv$number_gravels_remaining*100

filename2 = paste0("data_survival_tidy_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
write.csv(data.surv, paste(path, "output", filename2, sep = "/"))
