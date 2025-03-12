### LENGTH STATISTICS FOR DYKMAN ET AL 2025 GIANT KELP RECIPROCAL TRANSPLANT STUDY ###

# This script runs statistical analyses for the impact of number of parents and source site on green gravel growth.
# Lauren Dykman
# Created: March 05, 2023
# Last Edited: March 07, 2025

rm(list=ls())

# Installing packages

#install.packages("lme4")
library(lme4)

# Setting working directory

path <- "/Users/laurendykman/Desktop/github/Dykman_etal_2025_RestorationEcology"

getwd()
setwd(path)
getwd()

# Importing data

input_file = paste0("data_all_tidy_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
data <- read.csv(paste(path, "output", input_file, sep = "/"), header=TRUE)

# The first timepoint

data.length.first <- data[data$days_old == "55",]

glm.results.first.1 <- glmer(total_length_cm ~ parent_number * source_population * site + (1|site_replicate/site_plot_id), data = data.length.first, family = Gamma, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(glm.results.first.1)

glm.results.first.2 <- glmer(total_length_cm ~ parent_number * source_population + site + parent_number : site + source_population : site + (1|site_replicate/site_plot_id), data = data.length.first, family = Gamma)
summary(glm.results.first.2)

glm.results.first.3 <- glmer(total_length_cm ~ parent_number * source_population + site + source_population : site + (1|site_replicate/site_plot_id), data = data.length.first, family = Gamma)
summary(glm.results.first.3)

glm.results.first.4 <- glmer(total_length_cm ~ parent_number + source_population * site + (1|site_replicate/site_plot_id), data = data.length.first, family = Gamma)
summary(glm.results.first.4)

# The second timepoint

data.length.second <- data[data$days_old == "100",]

glm.results.second.1 <- glmer(total_length_cm ~ parent_number * source_population * site + (1|site_replicate/site_plot_id), data = data.length.second, family = Gamma, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) # Error message: boundary (singular) fit: see help('isSingular')
summary(glm.results.second.1)

glm.results.second.2 <- glmer(total_length_cm ~ parent_number * source_population + site + parent_number:site + source_population:site + (1|site_replicate/site_plot_id), data = data.length.second, family = Gamma) # boundary (singular) fit: see help('isSingular')
summary(glm.results.second.2)

glm.results.second.3 <- glmer(total_length_cm ~ parent_number * source_population + site + source_population:site + (1|site_replicate/site_plot_id), data = data.length.second, family = Gamma) # boundary (singular) fit: see help('isSingular')
summary(glm.results.second.3)

glm.results.second.4 <- glmer(total_length_cm ~ parent_number * source_population + site + (1|site_replicate/site_plot_id), data = data.length.second, family = Gamma) # Says is singular but checked with just site_plot_id not nested in site_replicate and same result was returned so I accept this as correct
summary(glm.results.second.4)

# The third timepoint

data.length.third <- data[data$days_old == "140",]

glm.results.third.1 <- glmer(total_length_cm ~ parent_number * source_population * site + (1|site_replicate/site_plot_id), data = data.length.third, family = Gamma, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) # Error message: fixed-effect model matrix is rank deficient so dropping 1 column / coefficient boundary (singular) fit: see help('isSingular')
summary(glm.results.third.1)

glm.results.third.2 <- glmer(total_length_cm ~ parent_number * source_population + site + site:parent_number + site:source_population + (1|site_replicate/site_plot_id), data = data.length.third, family = Gamma, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) # boundary (singular) fit: see help('isSingular')
summary(glm.results.third.2)

glm.results.third.3 <- glmer(total_length_cm ~ parent_number + source_population + site + site:parent_number + site:source_population + (1|site_replicate/site_plot_id), data = data.length.third, family = Gamma, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) # boundary (singular) fit: see help('isSingular')
summary(glm.results.third.3)

glm.results.third.4 <- glmer(total_length_cm ~ parent_number + source_population + site + site:source_population + (1|site_replicate/site_plot_id), data = data.length.third, family = Gamma, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) # boundary (singular) fit: see help('isSingular')
summary(glm.results.third.4)

glm.results.third.5 <- glmer(total_length_cm ~ parent_number + source_population + site + (1|site_replicate/site_plot_id), data = data.length.third, family = Gamma, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) # boundary (singular) fit: see help('isSingular')
summary(glm.results.third.5)

# The fourth timepoint

data.length.fourth <- data[data$days_old == "200",]

glm.results.fourth.1 <- glm(total_length_cm ~ parent_number * source_population * site, data = data.length.fourth, family = Gamma)
summary(glm.results.fourth.1)

glm.results.fourth.2 <- glm(total_length_cm ~ parent_number * source_population + site + site:parent_number + site:source_population, data = data.length.fourth, family = Gamma)
summary(glm.results.fourth.2)

glm.results.fourth.3 <- glm(total_length_cm ~ parent_number * source_population + site + site:source_population, data = data.length.fourth, family = Gamma)
summary(glm.results.fourth.3)

glm.results.fourth.4 <- glm(total_length_cm ~ parent_number * source_population + site, data = data.length.fourth, family = Gamma)
summary(glm.results.fourth.4)

glm.results.fourth.5 <- glmer(total_length_cm ~ parent_number + source_population + site + (1|site_replicate/site_plot_id), data = data.length.fourth, family = Gamma) # Error message: boundary (singular) fit: see help('isSingular') - but compared it to glm and results similar.
summary(glm.results.fourth.5)
