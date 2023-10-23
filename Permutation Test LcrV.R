setwd("/Users/charlescostanzo/College/Su 2023/Chopra/Poster Code/")
set.seed(123456)
library(tidyverse)
library(readxl)
library(ggpubr)
library(rstatix)
library(permuco)

# read in data
data <- read_csv("Data/titer.csv")

# add a new column for log10(titer)
data$log10_titer <- log(data$titer, base = 10)

# convert columns to factors
data$time <- factor(data$time)
data$id <- factor(data$id)

################################################################################
# LcrV Antigen Analysis
################################################################################

# create a new data frame for LcrV antigen 
data_lcrv <- data %>%
  filter(treatment == "LcrV") %>%
  dplyr::select(-c(id_treatment_sex, treatment,  name))

# convert columns to factors
data_lcrv$time <- factor(data_lcrv$time)
data_lcrv$id <- factor(data_lcrv$id)

# create data frames containing only two of the time groups each
Pre_Prime_lcrv <- filter(data, treatment == "LcrV", time != "Boost")

Pre_Boost_lcrv <- filter(data, treatment == "LcrV", time != "Prime")

Prime_Boost_lcrv <- filter(data, treatment == "LcrV", time != "Pre")

################################################################################  
# Permutation RM ANOVA
################################################################################  

full <- aovperm(log10_titer ~ time*sex + Error(id/(time)),
                data = data_lcrv, method = "Rd_kheradPajouh_renaud")
full
# time:sex interaction not significant --> remove and fit main effects model

main <- aovperm(log10_titer ~ time + sex + Error(id/(time)),
                data = data_lcrv, method = "Rd_kheradPajouh_renaud")
main
# sex not significant --> remove and fit reduced model with just time

reduced <- aovperm(log10_titer ~ time + Error(id/(time)),
                   data = data_lcrv, method = "Rd_kheradPajouh_renaud")
reduced
# final model, time is significant

################################################################################  
# Try Fitting reduced model again, but with only observations from two groups at a time
# (not sure if this is something we can do)
################################################################################  


## Pre vs. Prime (Before Vacc. vs. After 1st Dose)
################################################################################  
mod_Pre_Prime_lcrv <- aovperm(log10_titer ~ time + Error(id/(time)),
                              data = Pre_Prime_lcrv, method = "Rd_kheradPajouh_renaud")
mod_Pre_Prime_lcrv

# extract p-value
Pre_Prime_pval_lcrv <- mod_Pre_Prime_lcrv$table$`resampled P(>F)`

## Pre vs. Boost (Before Vacc. vs. After 2nd Dose)
################################################################################  
mod_Pre_Boost_lcrv <- aovperm(log10_titer ~ time + Error(id/(time)),
                              data = Pre_Prime_lcrv, method = "Rd_kheradPajouh_renaud")
mod_Pre_Boost_lcrv

# extract p-value
Pre_Boost_pval_lcrv <- mod_Pre_Boost_lcrv$table$`resampled P(>F)`

## Prime vs. Boost (After 1st Dose vs. After 2nd Dose)
################################################################################  
mod_Prime_Boost_lcrv <- aovperm(log10_titer ~ time + Error(id/(time)),
                                data = Prime_Boost_lcrv, method = "Rd_kheradPajouh_renaud")
mod_Prime_Boost_lcrv

# extract p-value
Prime_Boost_pval_lcrv <- mod_Prime_Boost_lcrv$table$`resampled P(>F)`

################################################################################  

p_lcrv <- c(Pre_Prime_pval_lcrv, Pre_Boost_pval_lcrv, Prime_Boost_pval_lcrv)
names(p_lcrv) <- c("Pre - Prime", "Pre - Boost", "Prime - Boost")

p.adj.signif_lcrv <- p.adjust(p = c(Pre_Prime_pval_lcrv, 
                                    Pre_Boost_pval_lcrv, 
                                    Prime_Boost_pval_lcrv), 
                              method = "bonferroni")
names(p.adj.signif_lcrv) <- c("Pre - Prime", "Pre - Boost", "Prime - Boost")
p.adj.signif_lcrv

