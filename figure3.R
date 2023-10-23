setwd("/Users/charlescostanzo/College/Su 2023/Chopra/Poster Code/")

library(tidyverse)
library(readxl)
library(ez)
library(MASS) # for negative binomial glm
library(AER) # for dispersion test
library(stargazer) # for generating regression tables
library(ggbeeswarm) # for beeswarm of points
library(ggpubr)
library(rstatix) 
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

# create a data frame containing negative binomial regression (model only
# containing vaccinated term) results
pval <- data.frame(.y. = "avg_spots",
                   group1 = c("Vaccinated"),
                   group2 = c("Control"),
                   p = c(8.7e-07),
                   p.adj = c(8.7e-07),
                   p.adj.signif = c("***"))

# create a data frame to add p-values to boxplot
# run t_test to get proper data frame format, but will need to change p values
stat.test <- glm.data %>%
  group_by(sex) %>%
  t_test(avg_spots ~ vaccinated) %>%
  adjust_pvalue(method = "bonferroni") %>%
  filter(!row_number() %in% 2) 

# change p values in stat.test to proper values
stat.test$p <- pval$p
stat.test$p.adj <- pval$p.adj

# add significance stars and xy coordinates for plotting
stat.test <- stat.test %>%
  add_significance() %>% 
  add_xy_position(x = "vaccinated")

# adjust y position of p value bar
stat.test$y.position = 2.9

# create plot
fig4 <- glm.data %>% 
  mutate(vaccinated = case_when(vaccinated == "0" ~ "Control",
                                vaccinated == "1" ~ "Vaccinated",
                                TRUE ~ "vaccinated"),
         vaccinated = factor(vaccinated, levels = c("Control","Vaccinated"))) %>%
  ggboxplot(x = "vaccinated",
            y = "avg_spots",
            fill = "vaccinated",
            bxp.errorbar = TRUE,
            bxp.errorbar.width = 0.15,
            label.rectangle = TRUE,
            width = 0.7) +
  stat_pvalue_manual(data = stat.test,
                     bracket.size = 1.2,
                     label.size = 10,
                     hide.ns = TRUE) +
  geom_beeswarm(aes(shape = sex, 
                    fill = vaccinated), 
                side = 0, # both directions jittering
                dodge.width = 0.7, # change width between shapes 
                size = 4.5, # adjust point size
                cex = 6, # adjust point spacing
                stroke = 0.7) +
  scale_shape_manual(values = c(21,24),
                     name = "Sex") +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n = 4),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) + 
  annotation_logticks(sides = "l") +
  theme(plot.background = element_blank())

fig4 <- ggpar(fig4,
              font.x = c(28),
              font.y = c(28),
              font.tickslab = c(24),
              font.legend = c(24),
              xlab = "Vaccination Status",
              ylab = "Normalized F1-V Specific\nSFU Count per 500,000 Cells",
              palette = c("peachpuff","darkorange"),
              legend = "right",
              legend.title = "Vaccination Status")
fig4

# save plot
ggsave(filename = "/Users/charlescostanzo/College/Su 2023/Chopra/Poster Code/Figures/figure3.jpg",
       plot = fig4, 
       width = 9.7,
       height = 6.92,
       units = "in",
       dpi = 400)

# Note: manually adjusted plot using Photoshop for final poster
# - filled in Male triangles as dark brown, Female circles as white
# - filled in boxes with orange and peach colors for vaccination status legend 