setwd("/Users/charlescostanzo/College/Su 2023/Chopra/Poster Code/")
set.seed(123456)
library(tidyverse)
library(readxl)
library(ggpubr)
library(rstatix)
library(permuco)

# read in data containing titer values for 6 vaccinated monkeys
data <- read_csv("Data/titer.csv")

# add a new column for log10(titer)
data$log10_titer <- log(data$titer, base = 10)

# convert columns to factors
data$time <- factor(data$time)
data$id <- factor(data$id)

################################################################################
# F1 Antigen Analysis
################################################################################

# create data frames containing only two of the time groups each
Pre_Prime_f1 <- filter(data, treatment == "F1", time != "Boost")

Pre_Boost_f1 <- filter(data, treatment == "F1", time != "Prime")

Prime_Boost_f1 <- filter(data, treatment == "F1", time != "Pre")

################################################################################  
# Permutation RM ANOVA
################################################################################  

# create a new data frame for F1 antigen 
data_f1 <- data %>%
  filter(treatment == "F1") %>%
  dplyr::select(-c(id_treatment_sex, treatment,  name))

# convert columns to factors
data_f1$time <- factor(data_f1$time)
data_f1$id <- factor(data_f1$id)

full <- aovperm(log10_titer ~ time*sex + Error(id/(time)),
               data = data_f1, method = "Rd_kheradPajouh_renaud")
full
# time:sex interaction not significant --> remove and fit main effects model

main <- aovperm(log10_titer ~ time + sex + Error(id/(time)),
                data = data_f1, method = "Rd_kheradPajouh_renaud")
main
# sex not significant --> remove and fit reduced model with just time

set.seed(123456)
reduced <- aovperm(log10_titer ~ time + Error(id/(time)),
                data = data_f1, method = "Rd_kheradPajouh_renaud")
reduced
# final model, time is significant

################################################################################  
# Try Fitting reduced model again, but with only observations from two groups at a time
# (not sure if this is something we can do)
################################################################################  


## Pre vs. Prime (Before Vacc. vs. After 1st Dose)
################################################################################  
mod_Pre_Prime_f1 <- aovperm(log10_titer ~ time + Error(id/(time)),
                data = Pre_Prime_f1, method = "Rd_kheradPajouh_renaud")
mod_Pre_Prime_f1

# extract p-value
Pre_Prime_pval_f1 <- mod_Pre_Prime_f1$table$`resampled P(>F)`

## Pre vs. Boost (Before Vacc. vs. After 2nd Dose)
################################################################################  
mod_Pre_Boost_f1 <- aovperm(log10_titer ~ time + Error(id/(time)),
                            data = Pre_Prime_f1, method = "Rd_kheradPajouh_renaud")
mod_Pre_Boost_f1

# extract p-value
Pre_Boost_pval_f1 <- mod_Pre_Boost_f1$table$`resampled P(>F)`

## Prime vs. Boost (After 1st Dose vs. After 2nd Dose)
################################################################################  
mod_Prime_Boost_f1 <- aovperm(log10_titer ~ time + Error(id/(time)),
                            data = Prime_Boost_f1, method = "Rd_kheradPajouh_renaud")
mod_Prime_Boost_f1

# extract p-value
Prime_Boost_pval_f1 <- mod_Prime_Boost_f1$table$`resampled P(>F)`

################################################################################  

p_f1 <- c(Pre_Prime_pval_f1, Pre_Boost_pval_f1, Prime_Boost_pval_f1)
names(p_f1) <- c("Pre - Prime", "Pre - Boost", "Prime - Boost")

p.adj.signif_f1 <- p.adjust(p = c(Pre_Prime_pval_f1, 
                                 Pre_Boost_pval_f1, 
                                 Prime_Boost_pval_f1), 
                         method = "bonferroni")
names(p.adj.signif_f1) <- c("Pre - Prime", "Pre - Boost", "Prime - Boost")
p.adj.signif_f1
