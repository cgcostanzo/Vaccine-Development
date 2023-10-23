setwd("/Users/charlescostanzo/College/Su 2023/Chopra/African Green Monkeys Plague/Data/")
library(tidyverse)
library(readxl)
library(scales)
library(ggpubr)
library(ggtext)
data <- read_excel("primate.xlsx")

data2 <- data %>%
  mutate(id_treatment_sex = unite(data = data, col = "id_treatment_sex", c(id, treatment, sex), sep = " ")$id_treatment_sex)

data2$time <- factor(data2$time, levels = c("Pre","Prime","Boost"))
data2$id_treatment_sex <- factor(data2$id_treatment_sex, 
                                 levels = c("9501 PBS F1 Male",
                                            "9286 PBS F1 Male",
                                            "9035 F1 Male",
                                            "9050 F1 Male",
                                            "9330 F1 Male",
                                            "0303 PBS F1 Female",
                                            "0116 PBS F1 Female",
                                            "9022 F1 Female",
                                            "9051 F1 Female",
                                            "9107 F1 Female",
                                            "9501 PBS LcrV Male",
                                            "9286 PBS LcrV Male",
                                            "9035 LcrV Male",
                                            "9050 LcrV Male",
                                            "9330 LcrV Male",
                                            "0303 PBS LcrV Female",
                                            "0116 PBS LcrV Female",
                                            "9022 LcrV Female",
                                            "9051 LcrV Female",
                                            "9107 LcrV Female"
                                 ))

# Map between column names and desired facet labels
facet_names <- list("9501 PBS F1 Male" = "9501 PBS Male",
                    "9286 PBS F1 Male" = "9286 PBS Male",
                    "9035 F1 Male" = "9035 ChAdOx1 Male",
                    "9050 F1 Male" = "9050 ChAdOx1 Male",
                    "9330 F1 Male" = "9330 ChAdOx1 Male",
                    "0303 PBS F1 Female" = "0303 PBS Female",
                    "0116 PBS F1 Female" = "0116 PBS Female",
                    "9022 F1 Female" = "9022 ChAdOx1 Female",
                    "9051 F1 Female" = "9051 ChAdOx1 Female",
                    "9107 F1 Female" = "9107 ChAdOx1 Female")

# Create a custom facet labeller function
facet_labeller <- function(variable, value) {
  return(facet_names[value])
}

################################################################################
# F1 Faceted Barplot
################################################################################
# Create barplot for F1 monkeys
f1_barplot <- data2 %>%
  filter(treatment == "F1" | treatment == "PBS F1") %>%
  mutate(id_treatment_sex = case_when( # change bar monkey id labels
    id_treatment_sex == "9501 PBS F1 Male" ~ "9501 M",
    id_treatment_sex == "9286 PBS F1 Male" ~ "9286 M",
    id_treatment_sex == "9035 F1 Male" ~ "9035 M",
    id_treatment_sex == "9050 F1 Male" ~ "9050 M",
    id_treatment_sex == "9330 F1 Male" ~ "9330 M",
    id_treatment_sex == "0303 PBS F1 Female" ~ "0303 F",
    id_treatment_sex == "0116 PBS F1 Female" ~ "0116 F",
    id_treatment_sex == "9022 F1 Female" ~ "9022 F",
    id_treatment_sex == "9051 F1 Female" ~ "9051 F",
    id_treatment_sex == "9107 F1 Female" ~ "9107 F",
    TRUE ~ id_treatment_sex)) %>%
  mutate(id_treatment_sex = factor(id_treatment_sex, # reorder bars
                                   levels = c("9501 M",
                                              "9286 M",
                                              "9035 M",
                                              "9050 M",
                                              "9330 M",
                                              "0303 F",
                                              "0116 F",
                                              "9022 F",
                                              "9051 F",
                                              "9107 F")),
         treatment = case_when(treatment == "PBS F1" ~ "Control",
                               treatment == "F1" ~ "Vaccinated",
                               TRUE ~ treatment)) %>%
  ggplot(aes(x = time, # each bar represents a dose/time
             y = log(titer, base = 10), # height of bar represents log10 IgG titer
             fill = treatment)) + # set bar color to correspond to each individual monkey
  geom_bar(stat = "identity",
           color = "black") +
  facet_grid(~id_treatment_sex, # create ten facets, one for each monkey, with three bars, one for each dose/time
             switch = "x") + # move the facet labels to below 
  ggtitle("\n") +
#  ggtitle("Figure 1: F1-V Titer Results", # set plot title and subtitle
#          subtitle = "IgG antibody titers of blood serum samples from ten individual African Green Monkeys exposed to\nF1-V plague antigen, quantified using enzyme-linked immunosorbent assay (ELISA).\n") +
  ylab("Total IgG Titer") + # rename y axis label
  xlab("NHP ID/Sex") + # make x axis label blank
  scale_x_discrete(labels = c("Pre", "Prime", "Boost")) + # set labels for each bar
  scale_y_continuous(limits = c(1.9,4), # set y axis limits
                     expand = c(0,0), # remove space between plot and axes
                     oob = rescale_none, # stops scale_y_continuous from dropping out of range values (and thus making bars disappear)
                     breaks = c(2,3,4), # set y axis tick mark points 
                     labels = c(expression(10^2), # relabel y axis tick marks
                                expression(10^3),
                                expression(10^4))) +
  scale_fill_manual(values = c("#FFF5C3",
                               "#DDCC77"),
                    name = "Treatment") + 
  theme(plot.title = element_text(size = 6, face = "bold"), # change title text size
        plot.subtitle = element_text(size = 20), # change subtitle text size
        legend.position = "right",
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        panel.grid.major.y = element_line(color = "black",
                                          linewidth = 0.1,
                                          linetype = 9),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 28),
        axis.text.x = element_markdown(size = 16, angle = 90, vjust = 0, hjust = 1),
        axis.text.y = element_text(size = 16, vjust = 0.3),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(size = 18))

################################################################################
# LcrV Faceted Barplot
################################################################################
lcrv_barplot <- data2 %>%
  filter(treatment == "LcrV" | treatment == "PBS LcrV") %>%
  mutate(id_treatment_sex = case_when( # change bar monkey id labels
       id_treatment_sex == "9501 PBS LcrV Male" ~ "9501 M",
       id_treatment_sex == "9286 PBS LcrV Male" ~ "9286 M",
       id_treatment_sex == "9035 LcrV Male" ~ "9035 M",
       id_treatment_sex == "9050 LcrV Male" ~ "9050 M",
       id_treatment_sex == "9330 LcrV Male" ~ "9330 M",
       id_treatment_sex == "0303 PBS LcrV Female" ~ "0303 F",
       id_treatment_sex == "0116 PBS LcrV Female" ~ "0116 F",
       id_treatment_sex == "9022 LcrV Female" ~ "9022 F",
       id_treatment_sex == "9051 LcrV Female" ~ "9051 F",
       id_treatment_sex == "9107 LcrV Female" ~ "9107 F",
       TRUE ~ id_treatment_sex)) %>%
  mutate(id_treatment_sex = factor(id_treatment_sex, # reorder bars
                                   levels = c("9501 M",
                                              "9286 M",
                                              "9035 M",
                                              "9050 M",
                                              "9330 M",
                                              "0303 F",
                                              "0116 F",
                                              "9022 F",
                                              "9051 F",
                                              "9107 F")),
         treatment = case_when(treatment == "PBS LcrV" ~ "Control",
                               treatment == "LcrV" ~ "Vaccinated",
                               TRUE ~ treatment)) %>%
  ggplot(aes(x = time, # each bar represents a dose/time
             y = log(titer, base = 10), # height of bar represents log10 IgG titer
             fill = treatment)) + # set bar color to correspond to each individual monkey
  geom_bar(stat = "identity",
           color = "black") +
  facet_grid(~id_treatment_sex, # create ten facets, one for each monkey, with three bars, one for each dose/time
             switch = "x") + # move the facet labels to below 
  ggtitle("\n") +
#  ggtitle("Figure 2: LcrV Titer Results", # set plot title and subtitle
#          subtitle = "IgG antibody titers of blood serum samples from ten individual African Green Monkeys exposed to\nLcrV plague antigen, quantified using enzyme-linked immunosorbent assay (ELISA).\n") +
  ylab("Total IgG Titer") + # rename y axis label
  xlab("NHP ID/Sex") + # make x axis label blank
  scale_x_discrete(labels = c("Pre", 
                              "Prime", 
                              "Boost")) + # set labels for each bar
  scale_y_continuous(limits = c(1.9,5.1), # set y axis limits
                     expand = c(0,0), # remove space between plot and axes
                     oob = rescale_none, # stops scale_y_continuous from dropping out of range values (and thus making bars disappear)
                     breaks = c(2,3,4,5), # set y axis tick mark points 
                     labels = c(expression(10^2), # relabel y axis tick marks
                                expression(10^3),
                                expression(10^4),
                                expression(10^5))) +
  scale_fill_manual(values = c("#D5F1FF", # change color palette
                               "#81BDDC"),
                    name = "Treatment") + 
  theme(plot.title = element_text(size = 6, face = "bold"), # change title text size
        plot.subtitle = element_text(size = 20), # change subtitle text size
        legend.position = "right",
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        panel.grid.major.y = element_line(color = "black",
                                          linewidth = 0.1,
                                          linetype = 9),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 28),
        axis.text.x = element_markdown(size = 16, angle = 90, vjust = 0, hjust = 1),
        axis.text.y = element_text(size = 16, vjust = 0.3),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(size = 18))

################################################################################
# Export Plots
################################################################################

ggsave(filename = "/Users/charlescostanzo/College/Su 2023/Chopra/Titer/Plots/figure1.jpeg",
       plot = f1_barplot,
       dpi = 1000)

ggsave(filename = "/Users/charlescostanzo/College/Su 2023/Chopra/Titer/Plots/figure2.jpeg",
       plot = lcrv_barplot,
       dpi = 1000)



################################################################################

data2 %>%
  filter(treatment == "F1" | treatment == "PBS F1") %>%
  ggplot(aes(x = id_treatment_sex, 
             y = log(titer, base=10), 
             fill = id_treatment_sex, 
             shape = time)) +
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black") +
  scale_y_continuous(limits = c(1.9,4.01), 
                     expand = c(0, 0), 
                     oob = rescale_none,
                     breaks = c(2,3,4),
                     labels = c(expression(10^2),
                                expression(10^3),
                                expression(10^4))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1))


lcrv_barplot <- data2 %>%
  filter(treatment == "LcrV" | treatment == "PBS LcrV") %>%
  mutate(id_treatment_sex = case_when( # change bar monkey id labels
    id_treatment_sex == "9501 PBS LcrV Male" ~ "9501 Unvaccinated Male",
    id_treatment_sex == "9286 PBS LcrV Male" ~ "9286 Unvaccinated Male",
    id_treatment_sex == "9035 LcrV Male" ~ "9035 Vaccinated Male",
    id_treatment_sex == "9050 LcrV Male" ~ "9050 Vaccinated Male",
    id_treatment_sex == "9330 LcrV Male" ~ "9330 Vaccinated Male",
    id_treatment_sex == "0303 PBS LcrV Female" ~ "0303 Unvaccinated Female",
    id_treatment_sex == "0116 PBS LcrV Female" ~ "0116 Unvaccinated Female",
    id_treatment_sex == "9022 LcrV Female" ~ "9022 Vaccinated Female",
    id_treatment_sex == "9051 LcrV Female" ~ "9051 Vaccinated Female",
    id_treatment_sex == "9107 LcrV Female" ~ "9107 Vaccinated Female",
    TRUE ~ id_treatment_sex)) %>%
  mutate(id_treatment_sex = factor(id_treatment_sex, # reorder bars
                                   levels = c("9501 Unvaccinated Male",
                                              "9286 Unvaccinated Male",
                                              "9035 Vaccinated Male",
                                              "9050 Vaccinated Male",
                                              "9330 Vaccinated Male",
                                              "0303 Unvaccinated Female",
                                              "0116 Unvaccinated Female",
                                              "9022 Vaccinated Female",
                                              "9051 Vaccinated Female",
                                              "9107 Vaccinated Female")))

p <- ggarrange(f1_barplot, lcrv_barplot, nrow = 2)

ggsave(filename = "/Users/charlescostanzo/College/Su 2023/Chopra/Titer/Plots/figure12.jpeg",
       p,
       width = 14.04,
       height = 7.01,
       units = "in",
       dpi = 1000)

levels = c("9501 M",
           "Male 9286",
           "Male 9035",
           "Male 9050",
           "Male 9330",
           "Female 0303",
           "Female 0116",
           "Female 9022",
           "Female 9051",
           "Female 9107")
