setwd("/Users/charlescostanzo/College/Su 2023/Chopra/Poster Code/")

library(tidyverse)
library(readxl)
library(ez)
library(MASS) # for negative binomial glm
library(AER) # for dispersion test
library(stargazer) # for generating regression tables
################################################################################
# Data Management
################################################################################
elispot <- read_csv("Data/elispot.csv")
elispot$sex <- factor(elispot$sex)
elispot$vaccinated <- factor(elispot$vaccinated)
elispot$id <- factor(elispot$id)

glm.data <- elispot %>%
  group_by(id) %>%
  mutate(avg_spots = round(mean(spots)),1) %>% # take mean of technical replicates and round to nearest integer
  dplyr::select(id, sex, avg_spots, vaccinated) %>%
  distinct()

################################################################################
# Poisson GLM
################################################################################

# fit full model
pois.mod.full <- glm(avg_spots ~ as.factor(vaccinated) * as.factor(sex),
                    family = "poisson", data = glm.data)
summary(pois.mod.full)

# fit main effects model
pois.mod.main <- glm(avg_spots ~ as.factor(vaccinated) + as.factor(sex),
                     family = "poisson", data = glm.data)
summary(pois.mod.main)

# fit reduced model
pois.mod.red <- glm(avg_spots ~ as.factor(vaccinated),
                family = "poisson", data = glm.data)
summary(pois.mod.red)

# check for overdispersion
dispersiontest(pois.mod.red, alternative="greater")
# test does not indicate that there is overdispersion, but we may not have
# enough samples detection.
# --> try dividing the residual deviance by the residual degress of freedom

pois.mod.red$deviance/pois.mod.red$df.residual
# 73 > 1 --> potential overdispersion --> consider negative binomial model

################################################################################
# Negative Binomial GLM
################################################################################

glm.data <- glm.data %>%
  mutate(vaccinated = case_when(vaccinated == "1" ~ "Vaccine",
                                vaccinated == "0" ~ "Control")) %>%
  rename(Treatment = vaccinated,
         Sex = sex,
         `Mean SFUs` = avg_spots)
# fit full model
nb.mod.full <- glm.nb(`Mean SFUs` ~ Treatment * Sex,
                      link = "log",
                      data = glm.data)
summary(nb.mod.full)

# fit main effects model
nb.mod.main <- glm.nb(`Mean SFUs` ~ Treatment + Sex,
                      link = "log",
                      data = glm.data)
summary(nb.mod.main)

# fit reduced model
nb.mod.red <- glm.nb(`Mean SFUs` ~ Treatment,
                 link = "log",
                 data = glm.data)
summary(nb.mod.red)

# Test whether adding the interaction term is necessary 
# (H0 is main effects model is sufficient)
anova(nb.mod.full, nb.mod.main)

# Test whether adding the sex term is necessary
# (H0 is reduced model is sufficient)
anova(nb.mod.main, nb.mod.red)

# calculate 95% CI for final reduced model
exp(confint(nb.mod.red))
