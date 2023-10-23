setwd("/Users/charlescostanzo/College/Su 2023/Chopra/African Green Monkeys Plague/Data/")
library(tidyverse)
library(readxl)
library(scales)
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


data2 %>%
  filter(treatment == "F1" | treatment == "PBS F1") %>%
  ggplot(aes(x = id_treatment_sex, 
             y = log(titer, base=10), 
             fill = id_treatment_sex), 
         col = "black") + 
  geom_bar(stat = "identity") + # , position = "dodge") +
  theme(panel.background = element_rect(fill = "white", # make the plot background white
                                        colour = "white"), # make the lines around each facet white
        panel.grid.major.y = element_line(colour = "black", # black horizontal dotted lines at each axis tick
                                          linewidth = 0.1, # reduce line width
                                          linetype = 9), # change line type
        axis.text.x = element_blank(), # element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.line = element_line(colour = "black"), # add x and y axes
        strip.placement = "outside", # put the time labels below axis
        strip.background = element_blank()) + # remove the gray background for the time labels
  facet_wrap(vars(time), 
             strip.position = "bottom", 
             scales = "free_x") +
  scale_y_continuous(limits = c(1.9,4.01), 
                     expand = c(0, 0), 
                     oob = rescale_none,
                     breaks = c(2,3,4)) +
  scale_fill_brewer(palette = "Paired",
                    name = "ID/Treatment/Gender",
                    labels = c("9501 PBS Male",
                               "9286 PBS Male",
                               "9035 Vaccinated Male",
                               "9050 Vaccinated Male",
                               "9330 Vaccinated Male",
                               "0303 PBS Female",
                               "0116 PBS Female",
                               "9022 Vaccinated Female",
                               "9051 Vaccinated Female",
                               "9107 Vaccinated Female")) +
  ylab(expression(paste("Total IgG Titer ", (log["10"]~scale), sep = ""))) +
  xlab("Dose") +
  ggtitle("F1-V Averaged")
