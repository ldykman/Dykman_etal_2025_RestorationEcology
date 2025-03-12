### SURVIVORSHIP STATISTICS FOR DYKMAN ET AL 2025 GIANT KELP RECIPROCAL TRANSPLANT STUDY ###

# Runs glmer with binomial distribution and cox proportional hazards model on green gravel survivorship data
# Uses for proportion of surviving gravel: Number of surviving gravel out of number of gravel left in plot
# Lauren Dykman
# Created: July 18, 2023
# Last Edited: March 11, 2025

rm(list=ls())

# Installing packages

#install.packages("ggfortify")
#install.packages("tidyr")
#install.packages("lme4")
#install.packages("coxme")
#install.packages("frailtyHL")

library(ggfortify)
library(tidyr)
library(lme4)
library(coxme)
library(frailtyHL)

# Setting working directory

path <- "/Users/laurendykman/Desktop/github/Dykman_etal_2025_RestorationEcology"

getwd()
setwd(path)
getwd()

# Importing data

input_file = paste0("data_survival_tidy_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
data <- read.csv(paste(path, "output", input_file, sep = "/"), header=TRUE)

# Survivorship stats using glmer

# First timepoint

data.first <- data[data$days_old == "55",]

# Statistical models

glm.results.first.1 <- glmer(cbind(number_gravels_with_kelp, number_gravels_remaining - number_gravels_with_kelp) ~ parent_number * source_population * site + (1|site_replicate), data.first, family = binomial)
summary(glm.results.first.1)

glm.results.first.2 <- glmer(cbind(number_gravels_with_kelp, number_gravels_remaining - number_gravels_with_kelp) ~ parent_number * source_population + site + parent_number:site + source_population:site + (1|site_replicate), data.first, family = binomial)
summary(glm.results.first.2)

glm.results.first.3 <- glmer(cbind(number_gravels_with_kelp, number_gravels_remaining - number_gravels_with_kelp) ~ parent_number + source_population + site + parent_number:site + source_population:site + (1|site_replicate), data.first, family = binomial)
summary(glm.results.first.3)

glm.results.first.4 <- glmer(cbind(number_gravels_with_kelp, number_gravels_remaining - number_gravels_with_kelp) ~ parent_number + source_population * site + (1|site_replicate), data.first, family = binomial)
summary(glm.results.first.4)

# Second timepoint

data.second <- data[data$days_old == "100",]

glm.results.second.1 <- glmer(cbind(number_gravels_with_kelp, number_gravels_remaining - number_gravels_with_kelp) ~ parent_number * source_population * site + (1|site_replicate), data.second, family = binomial)
summary(glm.results.second.1)

glm.results.second.2 <- glmer(cbind(number_gravels_with_kelp, number_gravels_remaining - number_gravels_with_kelp) ~ parent_number * source_population + site + parent_number:site + source_population:site + (1|site_replicate), data.second, family = binomial)
summary(glm.results.second.2)

glm.results.second.3 <- glmer(cbind(number_gravels_with_kelp, number_gravels_remaining - number_gravels_with_kelp) ~ parent_number * source_population + site + parent_number:site + (1|site_replicate), data.second, family = binomial)
summary(glm.results.second.3)

glm.results.second.4 <- glmer(cbind(number_gravels_with_kelp, number_gravels_remaining - number_gravels_with_kelp) ~ parent_number * source_population + site + (1|site_replicate), data.second, family = binomial)
summary(glm.results.second.4)

# Third timepoint

data.third <- data[data$days_old == "140",]

glm.results.third.1 <- glmer(cbind(number_gravels_with_kelp, number_gravels_remaining - number_gravels_with_kelp) ~ parent_number * source_population * site + (1|site_replicate), data.third, family = binomial) # Error messages: unable to evaluate scaled gradient, Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
summary(glm.results.third.1)

glm.results.third.2 <- glmer(cbind(number_gravels_with_kelp, number_gravels_remaining - number_gravels_with_kelp) ~ parent_number * source_population + site + parent_number:site + source_population:site + (1|site_replicate), data.third, family = binomial)
summary(glm.results.third.2)

glm.results.third.3 <- glmer(cbind(number_gravels_with_kelp, number_gravels_remaining - number_gravels_with_kelp) ~ parent_number * source_population + site + source_population:site + (1|site_replicate), data.third, family = binomial)
summary(glm.results.third.3)

glm.results.third.4 <- glmer(cbind(number_gravels_with_kelp, number_gravels_remaining - number_gravels_with_kelp) ~ parent_number * source_population + site + (1|site_replicate), data.third, family = binomial)
summary(glm.results.third.4)

glm.results.third.5 <- glmer(cbind(number_gravels_with_kelp, number_gravels_remaining - number_gravels_with_kelp) ~ parent_number + source_population + site + (1|site_replicate), data.third, family = binomial)
summary(glm.results.third.5)

# Fourth timepoint

data.fourth <- data[data$days_old == "200",]

glm.results.fourth.1 <- glmer(cbind(number_gravels_with_kelp, number_gravels_remaining - number_gravels_with_kelp) ~ parent_number * source_population * site + (1|site_replicate), data.fourth, family = binomial) # Error messages: unable to evaluate scaled gradient; Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
summary(glm.results.fourth.1)

glm.results.fourth.2 <- glmer(cbind(number_gravels_with_kelp, number_gravels_remaining - number_gravels_with_kelp) ~ parent_number * source_population + site + parent_number:site + source_population:site + (1|site_replicate), data.fourth, family = binomial) # Error message: Model failed to converge with max|grad| = 0.0481621 (tol = 0.002, component 1)
summary(glm.results.fourth.2)

glm.results.fourth.3 <- glmer(cbind(number_gravels_with_kelp, number_gravels_remaining - number_gravels_with_kelp) ~ parent_number * source_population + site + source_population:site + (1|site_replicate), data.fourth, family = binomial) # Error message: unable to evaluate scaled gradient; Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
summary(glm.results.fourth.3)

glm.results.fourth.4 <- glmer(cbind(number_gravels_with_kelp, number_gravels_remaining - number_gravels_with_kelp) ~ parent_number * source_population + site + (1|site_replicate), data.fourth, family = binomial)
summary(glm.results.fourth.4)

glm.results.fourth.5 <- glmer(cbind(number_gravels_with_kelp, number_gravels_remaining - number_gravels_with_kelp) ~ parent_number + source_population + site + (1|site_replicate), data = data.fourth, family = binomial)
summary(glm.results.fourth.5)

### Cox proportional hazards model Version 1

# Count missing gravels as unknown status

time <- c(1,2,3)
plots <- sort(unique(data$site_plot_id))

surv.data.frame <- as.data.frame(matrix(nrow=0,ncol=7))

for (plot in plots) { # Iterate through each plot

  print(plot)

  subset <- data[data$site_plot_id == plot,] # Subset data set for each plot - note that each time should be its own row ordered by increasing time.
  print(subset$days_old)

  results.lost <- c()
  results.died <- c()

  for (t in time) { # Iterate through the three timepoints by indexing the dataset

    if (t - 1 <= 0) { # At the first timepoint cannot index back but there were 20 gravels initially in each plot.

      number.lost <- 20 - subset$number_gravels_remaining[t]
      number.died <- 20 - subset$number_gravels_with_kelp[t]

      print(number.lost)
      print(number.died)

    } else { # All other timepoints calculate loss by subtracting the number at time t from the number at time t-1.

      number.lost <- subset$number_gravels_remaining[t-1] - subset$number_gravels_remaining[t]
      number.died <- subset$number_gravels_with_kelp[t-1] - subset$number_gravels_with_kelp[t]

      print(number.lost)
      print(number.died)

      if (is.na(number.lost)) { # Handling cases where divers recorded more gravels than at the prior timepoint.

        number.lost <- 0

      }

      if (number.lost < 0 | is.na(number.lost)) { # Handling cases where divers recorded more gravels than at the prior timepoint.

        results.lost[length(results.lost)] <- results.lost[length(results.lost)] + number.lost
        number.lost <- 0

      }

    }

    results.died <- c(results.died, number.died)
    results.lost <- c(results.lost, number.lost)
  }

  results.data.frame <- cbind(rep(plot, length(results.lost)), rep(unique(subset$site), length(results.lost)), rep(unique(subset$source_population), length(results.lost)), rep(unique(subset$parent_number), length(results.lost)), rep(unique(subset$replicate_number), length(results.lost)), c(55, 100, 140), results.lost, results.died)
  surv.data.frame <- rbind(surv.data.frame, results.data.frame)

}

colnames(surv.data.frame) <- c("site_plot_id", "site", "source_population", "parent_number", "replicate", "time", "num_lost", "num_died")
surv.data.frame$num_lost <- as.integer(surv.data.frame$num_lost)
surv.data.frame$num_died <- as.integer(surv.data.frame$num_died)
surv.data.frame$time <- as.integer(surv.data.frame$time)
surv.data.frame$actual.death <- surv.data.frame$num_died - surv.data.frame$num_lost
surv.data.frame$actual.death[surv.data.frame$actual.death < 0] <- 0
surv.data.frame$censor0 <- surv.data.frame$num_died - surv.data.frame$actual.death
surv.data.frame[is.na(surv.data.frame)] <- 0

# Creating a cox data frame

cox.frame.final <- as.data.frame(matrix(nrow=0, ncol=7))

times <- sort(unique(surv.data.frame$time))
plots <- sort(unique(surv.data.frame$site_plot_id))

for (plot in plots) {

  print(plot)

  j <- 20

  for (t in times) {

    print(t)

    surv.data.subset <- surv.data.frame[surv.data.frame$site_plot_id == plot & surv.data.frame$time == t,]

    site <- surv.data.subset$site
    origin_temp <- surv.data.subset$source_population
    num_parents <- surv.data.subset$parent_number
    replicate <- surv.data.subset$replicate
    num.died <- surv.data.subset$actual.death
    censor0 <- surv.data.subset$censor0

    print(c(num.died, censor0))

    if (num.died > 0) {

      for (i in c(1:num.died)){

        cox.frame.final <- rbind(cox.frame.final, c(plot, site, origin_temp, num_parents, replicate, t, 1))

        j <- j - 1

        }
      }

    if (censor0 > 0) {

      for (i in c(1:censor0)){

        cox.frame.final <- rbind(cox.frame.final, c(plot, site, origin_temp, num_parents, replicate, t, 0))

        j <- j - 1

      }
    }
  }

  print(j)

  if (j > 0) {

    for (i in c(1:j)){

      cox.frame.final <- rbind(cox.frame.final, c(plot, site, origin_temp, num_parents, replicate, 140, 0))}
    }
  }

colnames(cox.frame.final) <- c("site_plot_id", "site", "source_population", "parent_number", "replicate", "time", "status")
cox.frame.final$time <- as.integer(cox.frame.final$time)
cox.frame.final$status <- as.integer(cox.frame.final$status)
cox.frame.final$site_replicate <- paste(cox.frame.final$site, cox.frame.final$replicate, sep = "_")

cox.check <- cox.frame.final %>%
  group_by(site_plot_id) %>%
  tally()

cox.fit.1 <- coxme(Surv(time, status) ~ parent_number * source_population * site + (1|site_replicate/site_plot_id), data = cox.frame.final)
summary(cox.fit.1)

cox.fit.2 <- coxme(Surv(time, status) ~ parent_number * source_population + site + parent_number:site + source_population:site + (1|site_replicate/site_plot_id), data = cox.frame.final)
summary(cox.fit.2)

cox.fit.3 <- coxme(Surv(time, status) ~ parent_number + source_population + site + parent_number:site + source_population:site + (1|site_replicate/site_plot_id), data = cox.frame.final)
summary(cox.fit.3)

cox.fit.4 <- coxme(Surv(time, status) ~ parent_number + source_population + site + source_population:site + (1|site_replicate/site_plot_id), data = cox.frame.final)
summary(cox.fit.4)

cox.fit.5 <- coxme(Surv(time, status) ~ parent_number + source_population + site + (1|site_replicate/site_plot_id), data = cox.frame.final)
summary(cox.fit.5)

# Nothing significant
