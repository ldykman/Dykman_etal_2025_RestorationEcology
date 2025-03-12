### SURVIVORSHIP FIGURES FOR DYKMAN ET AL 2025 GIANT KELP RECIPROCAL TRANSPLANT STUDY ###

# This script runs statistical analyses for the influence of number of parents and source site on green gravel survivorship
# Using the current proportion of gravels with kelp over gravels left in plot to calculate mortality events.
# Lauren Dykman
# Created: March 05, 2023
# Last Edited: Jan 29, 2025

rm(list=ls())

# Installing packages

#install.packages("tidyverse")
#install.packages("hrbrthemes")
#install.packages("viridis")
#install.packages("gdtools")
#install.packages("lme4")
#install.packages("ggfortify")
#install.packages("blme")

library(tidyverse)
library(hrbrthemes)
library(viridis)
library(lme4)
library(ggfortify)
library(survival)
library(multcomp)
library(blme)

# Setting working directory

path <- "/Users/laurendykman/Desktop/github/Dykman_etal_2025_RestorationEcology"

getwd()
setwd(path)
getwd()

# Importing data

input_file = paste0("data_survival_tidy_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
data.surv <- read.csv(paste(path, "output", input_file, sep = "/"), header=TRUE)

# Calculate the mean and standard deviation by parent number and source population

summary <- data.surv %>%
  group_by(days_old, parent_number, source_population) %>%
  summarise_at(vars(survival),
               list(mean = ~mean(as.numeric(.), na.rm = TRUE),
                    sd = ~sd(as.numeric(.), na.rm = TRUE)))

summary$se <-summary$sd/sqrt(16)
summary <- summary[!(summary$days_old == "200"),]
summary$source_population <- factor(summary$source_population, levels = c("warm", "cool"))

# Plotting kelp survivorship by parent number and source population

ggsave(paste(path, "Figures", paste0("Figure_Survivorship_SourcePopulation_ParentNumber_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(summary, aes(fill = source_population, x=parent_number, y=mean)) +
         geom_bar(stat = "identity", position = position_dodge(), color = "black", linewidth = 0.3) +
         geom_errorbar(aes(x = parent_number,
                           ymin = ifelse(mean - se <= 0, 0, mean - se),
                           ymax = mean + se), linewidth = 0.3, width = 0.4, stat = "identity", position = position_dodge(0.9)) +
         xlab("number of parents") +
         ylab("% gravels remaining with kelp") +
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

# Calculate the mean and standard deviation by site and source population

summary <- data.surv %>%
  group_by(days_old, site, source_population) %>%
  summarise_at(vars(survival),
               list(mean = ~mean(as.numeric(.), na.rm = TRUE),
                    sd = ~sd(as.numeric(.), na.rm = TRUE)))

summary$se <-summary$sd/sqrt(16)
summary <- summary[!(summary$days_old == "200"),]
summary$source_population <- factor(summary$source_population, levels = c("warm", "cool"))
summary$site <- factor(summary$site, levels = c("warm", "cool"))

# Plotting kelp survivorship by source population and site

ggsave(paste(path, "Figures", paste0("Figure_Survivorship_SourcePopulation_Site_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(summary, aes(fill = source_population, x=site, y=mean)) +
         geom_bar(stat = "identity", position = position_dodge(), color = "black", linewidth = 0.3) +
         geom_errorbar(aes(x = site,
                           ymin = mean - se,
                           ymax = mean + se), linewidth = 0.3, width = 0.4, stat = "identity", position = position_dodge(0.9)) +
         xlab("outplant site") +
         ylab("% gravels remaining with kelp") +
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

# Calculations of percent difference for manuscript

c.c.55 <- (78.508496-61.260009)/61.260009
w.w.55 <- (90.634065-48.984241)/48.984241

# Source population calculations

summary <- data.surv %>%
  group_by(days_old, source_population) %>%
  summarise_at(vars(survival),
               list(mean = ~mean(as.numeric(.), na.rm = TRUE),
                    sd = ~sd(as.numeric(.), na.rm = TRUE)))
summary$se <-summary$sd/sqrt(16)

cool.100 <- (17.403726-15.027435)/15.027435

# Parent number calculations

summary <- data.surv %>%
  group_by(days_old, parent_number) %>%
  summarise_at(vars(survival),
               list(mean = ~mean(as.numeric(.), na.rm = TRUE),
                    sd = ~sd(as.numeric(.), na.rm = TRUE)))
summary$se <-summary$sd/sqrt(16)
