### ENVIRONMENTAL VARIABLE FIGURES FOR DYKMAN ET AL 2025 GIANT KELP RECIPROCAL TRANSPLANT STUDY ###

# This script creates figures for urchin density and substrate composition.
# Lauren Dykman
# Created: March 5, 2023
# Last Edited: March 11, 2025

rm(list=ls())

# Installing packages

#install.packages("tidyverse")
#install.packages("hrbrthemes")
#install.packages("viridis")
#install.packages("gdtools")
#install.packages("lme4")
#install.packages("ggfortify")
#install.packages("nnet")
#install.packages("lmtest")

library(tidyverse)
library(hrbrthemes)
library(viridis)
library(lme4)
library(ggfortify)
library(survival)
library(nnet)
library(lmtest)

# Setting working directory

path <- "/Users/laurendykman/Desktop/github/Dykman_etal_2025_RestorationEcology"

getwd()
setwd(path)
getwd()

# Analyzing survivorship as a function of percent sand in substrate

# Importing and tidying plot substrate monitoring data

input_file_1 = "plot_characterization_2023.csv"
data.substrate <- read.csv(paste(path, "data", input_file_1, sep = "/"), header=TRUE)

data.substrate$site[data.substrate$site == "Dixon"] <- "warm"
data.substrate$site[data.substrate$site == "Ed King"] <- "cool"
data.substrate$site_plot_id <- paste(data.substrate$site, data.substrate$plot_id, sep = "_")
data.substrate <- data.substrate[!data.substrate$treatment == "Control",]

# Inputting and tidying green gravel survivorship data

input_file_2 = paste0("data_survival_tidy_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
data.survival <- read.csv(paste(path, "output", input_file_2, sep = "/"), header=TRUE)

data.survival$month <- rep(NA, dim(data.survival)[1])
data.survival$month[data.survival$days_old == 55] <- "April"
data.survival$month[data.survival$days_old == 100] <- "May"
data.survival$month[data.survival$days_old == 140] <- "June"
data.survival$month[data.survival$days_old == 200] <- "August"

# Checking the two data frames have all the same plots

data.substrate$site_plot_id %in% data.survival$site_plot_id

data.substrate.ordered <- data.substrate[order(data.substrate$percent_sand, data.substrate$percent_cobble),]
ordered.by.sand <- data.substrate.ordered$site_plot_id

data.substrate.2 <- gather(data.substrate.ordered, substrate_type, percent_composition, percent_sand:percent_cobble)

data.substrate.2$site_plot_id <- factor(data.substrate.2$site_plot_id, levels = unique(ordered.by.sand))

# Figure of substrate composition of all plots

ggsave(paste(path, "Figures", paste0("Figure_Plot_Substrate_Composition_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(data.substrate.2, aes(fill = substrate_type, x=site_plot_id, y=percent_composition)) +
         geom_bar(stat = "identity", position = "stack", color = "black") +
         xlab("Plot") +
         ylab("% substrate") +
         ggtitle("Substrate Composition by Plot") +
         theme_bw() +
         labs(fill = "substrate") +
         labs(color = "substrate") +
         theme(text=element_text(size = 16),
               aspect.ratio=0.6,
               panel.border = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"),
               panel.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA)), height=6, width=8) # Setting the theme

sand.by.survival <- merge(data.survival[c("site_plot_id", "survival", "site", "month")], data.substrate[c("site_plot_id", "percent_sand")], by = "site_plot_id")

sand.by.survival$site_plot_id <- factor(sand.by.survival$site_plot_id, levels = unique(ordered.by.sand))

sand.by.survival$site <- factor(sand.by.survival$site, levels = c("warm", "cool"))
sand.by.survival <- sand.by.survival[!sand.by.survival$month == "August",]
sand.by.survival$month <- factor(sand.by.survival$month, levels = c("April", "May", "June"))

# Figure of survival by the percent of sand in a plot

ggsave(paste(path, "Figures", paste0("Figure_Sand_Survival_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
ggplot(sand.by.survival, aes(x = percent_sand, y = survival, colour = site)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("% sand") +
  ylab("% gravels remaining with kelp") +
  labs(color = "site") +
  facet_grid(rows = vars(month)) +
  theme_bw() +
  theme(text=element_text(size = 22),
        aspect.ratio=0.6,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)), height=6, width=8)

### Analyzing urchin density

# Importing data

input_file_3 = "urchin_surveys_2023.csv"
data.urchin <- read.csv(paste(path, "data", input_file_3, sep = "/"), header=TRUE)

data.urchin$month <- c(rep(NA, dim(data.urchin)[1]))
data.urchin$month[data.urchin$date %in% c("2023-05-17", "2023-05-18")] <- "May"
data.urchin$month[data.urchin$date %in% c("2023-06-29")] <- "June"
data.urchin$month[data.urchin$date %in% c("2023-08-28", "2023-08-29", "2023-08-30")] <- "August"
data.urchin <- data.urchin[!is.na(data.urchin$month),]

data.urchin$site_substrate_month <- paste(data.urchin$site, data.urchin$substrate, data.urchin$month, sep = "_")

summary <- data.urchin %>%
  group_by(site_substrate_month, site, substrate, month) %>%
  summarise_at(vars(urchins),
               list(mean = ~mean(as.numeric(.), na.rm = TRUE),
                    sd = ~sd(as.numeric(.), na.rm = TRUE)))

counts <- data.urchin %>%
  group_by(site_substrate_month, site, substrate, month) %>%
  tally()

urchins2 <- merge(summary, counts[c("site_substrate_month", "n")], by = "site_substrate_month")
urchins2$se <- urchins2$sd/sqrt(urchins2$n)

urchins2$month <- factor(urchins2$month, level = c("May", "June", "August"))

# Figure of urchin density

ggsave(paste(path, "Figures", paste0("Figure_Urchin_Density_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(urchins2, aes(fill = substrate, x=site, y=mean)) +
         geom_bar(stat = "identity", position = position_dodge(), color = "black", size=0.8) +
         geom_errorbar(aes(x = site,
                           ymin = ifelse(mean - se <= 0, 0, mean - se),
                           ymax = mean + se), linewidth = 0.8, width = 0.4, stat = "identity", position = position_dodge(0.9))+ #, size = 0.2, width=0.2, alpha=0.6) +# Setting the theme
         scale_fill_manual("substrate", values = c("wheat", "wheat4")) +
         scale_color_manual("substrate", values = c("wheat", "wheat4")) +
         xlab("site") +
         ylab("urchins per square meter") +
         ggtitle("Urchin Density") +
         theme_bw() +
         facet_grid(rows = vars(month)) +
         theme(text=element_text(size = 20),
               aspect.ratio=0.6,
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"),
               panel.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA)), height=6, width=8) # Setting the theme

urchin.data.first <- data.urchin[data.urchin$month == "May",]
glm.results.first <- glmer(urchins ~ site * substrate + (1|region), urchin.data.first, family = poisson)
summary(glm.results.first)
# Substrate significant, interaction significant

urchin.data.second <- data.urchin[data.urchin$month == "June",]
glm.results.second <- glmer(urchins ~ site + substrate + (1|region), urchin.data.second, family = poisson)
summary(glm.results.second)
# Site significant, interaction not significant

urchin.data.third <- data.urchin[data.urchin$month == "August",]
glm.results.third <- glmer(urchins ~ site * substrate + (1|region), urchin.data.third, family = poisson)
summary(glm.results.third)
# Substrate significant, interaction significant

glm.results.all <- glmer(urchins ~ site * substrate + month + (1|region), data.urchin, family = poisson)
summary(glm.results.all)
# Substrate significant, interaction with site and substrate significant, month not significant
