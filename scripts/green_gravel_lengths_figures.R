### LENGTH FIGURES FOR DYKMAN ET AL 2025 GIANT KELP RECIPROCAL TRANSPLANT STUDY ###

# This script produces figures for the influence of number of parents and source site on giant kelp growth.
# Lauren Dykman
# Created: March 05, 2023
# Last Edited: March 07, 2025

rm(list=ls())

# Installing packages

#install.packages("tidyverse")
#install.packages("hrbrthemes")
#install.packages("viridis")
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

input_file = paste0("data_all_tidy_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
data <- read.csv(paste(path, "output", input_file, sep = "/"), header=TRUE)

# Creating table of mean and count of kelp growth by source population and parent number

summary <- data %>%
  group_by(parent_temp_days, source_population, parent_number, days_old) %>%
  summarise_at(vars(total_length_cm),
               list(mean = ~mean(as.numeric(.), na.rm = TRUE),
                    sd = ~sd(as.numeric(.), na.rm = TRUE)))

counts <- data %>% 
  group_by(parent_temp_days, source_population, parent_number, days_old) %>% 
  summarise(non_na_count = sum(!is.na(total_length_cm)))

# Calculating standard error

data.error <- merge(counts[c("non_na_count", "parent_temp_days")], summary, by = "parent_temp_days")
data.error$se <-data.error$sd/sqrt(data.error$non_na_count)

# Cleaning the table

data.error <- data.error[!data.error$days_old == 200,]
data.error$source_population <- factor(data.error$source_population, levels = c("warm", "cool"))

# Making a figure of kelp lengths by source population and number of parents

ggsave(paste(path, "Figures", paste0("Figure_Length_SourcePopulation_ParentNumber_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(data.error, aes(fill = source_population, x=parent_number, y=mean)) +
         geom_bar(stat = "identity", position = position_dodge(), color = "black", linewidth = 0.3) +
         geom_errorbar(aes(x = parent_number,
                           ymin = ifelse(mean - se <= 0, 0, mean - se),
                           ymax = mean + se), linewidth = 0.3, width = 0.4, stat = "identity", position = position_dodge(0.9)) +
         xlab("number of parents") +
         ylab("total length (cm)") +
         theme_bw() +
         facet_grid(rows = vars(days_old)) +
         labs(fill = "source\npopulation") +
         theme(text=element_text(size = 12),
               aspect.ratio=1.1,
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"),
               legend.title = element_text(size=10),
               panel.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA)), height=5, width=5) # Setting the theme

# Creating table of mean and count of kelp growth by source population and site

summary <- data %>%
  group_by(site_temp_days, source_population, site, days_old) %>%
  summarise_at(vars(total_length_cm),
               list(mean = ~mean(as.numeric(.), na.rm = TRUE),
                    sd = ~sd(as.numeric(.), na.rm = TRUE)))

counts <- data %>% 
  group_by(site_temp_days, source_population, site, days_old) %>% 
  summarise(non_na_count = sum(!is.na(total_length_cm)))

# Calculating standard error

data.error <- merge(counts[c("non_na_count", "site_temp_days")], summary, by = "site_temp_days")
data.error$se <-data.error$sd/sqrt(data.error$non_na_count)

# Cleaning the table

data.error <- data.error[!data.error$days_old == 200,]
data.error$source_population <- factor(data.error$source_population, levels = c("warm", "cool"))
data.error$site <- factor(data.error$site, levels = c("warm", "cool"))

# Making a figure of kelp lengths by source population and site

ggsave(paste(path, "Figures", paste0("Figure_Length_SourcePopulation_Site_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(data.error, aes(fill = source_population, x=site, y=mean)) +
         geom_bar(stat = "identity", position = position_dodge(), color = "black", linewidth = 0.3) +
         geom_errorbar(aes(x = site,
                           ymin = ifelse(mean - se <= 0, 0, mean - se),
                           ymax = mean + se), linewidth = 0.3, width = 0.4, stat = "identity", position = position_dodge(0.9)) +
         xlab("outplant site") +
         ylab("total length (cm)") +
         theme_bw() +
         labs(fill = "source\npopulation") +
         facet_grid(rows = vars(days_old)) +
         theme(text=element_text(size = 12),
               aspect.ratio=1.1,
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"),
               legend.title = element_text(size=10),
               panel.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA)), height=5, width=5) # Setting the theme

w.w.55 <- (5.808750 - 3.955714)/3.955714 # Calculating percent difference for manuscript
c.c.55 <- (5.763750 - 5.167089)/5.167089 # Calculating percent difference for manuscript

length.mean.pop <- aggregate(data$total_length_cm, by = list(data$source_population, data$days_old), FUN=mean, na.rm = TRUE)
colnames(length.mean.pop) <- c("source_population", "days_old", "length_mean")

w.w.100 <- (38.298148 - 35.364444)/35.364444 # Calculating percent difference for manuscript

length.mean.site <- aggregate(data$total_length_cm, by = list(data$site, data$days_old), FUN=mean, na.rm = TRUE)
colnames(length.mean.site) <- c("site", "days_old", "length_mean")

c.c.140 <- (122.825000 - 47.621212)/47.621212 # Calculating percent difference for manuscript
c.c.200 <- (285.333333 - 88.333333)/88.333333 # Calculating percent difference for manuscript
