library(tidyverse)
library(ggbeeswarm)
library(ggpubr)
library(rstatix)

################################################################################
# Import and Manipulate Plot Data
################################################################################

data <- read_csv("/Users/charlescostanzo/College/Su 2023/Chopra/Poster Code/Data/primate.csv")

plot_data <- data

plot_data$time <- factor(data$time, levels = c("Pre","Prime","Boost"))

plot_data <- plot_data %>%
  filter(!grepl("PBS", treatment))
#  filter(time != "Pre") %>%

# create a data frame containing permutation test results from one-way repeated
# measures ANOVA conducted for each antigen (F1 and LcrV)
pvals <- data.frame(supp = c("F1","F1","F1","LcrV","LcrV","LcrV"),
                    .y. = rep("titer",6),
                    group1 = c("Pre","Pre","Prime","Pre","Pre","Prime"),
                    group2 = c("Prime","Boost","Boost","Prime","Boost","Boost"),
                    p = c(0.0024, 0.0022, 0.0698, 0.0024, 0.0018,0.0018),
                    p.adj = c(0.0072, 0.0066, 0.2094, 0.0072, 0.0054, 0.0054),
                    p.adj.signif = c("**","**","ns","**","**","**"))

################################################################################
# Generate Dataframe for stat_pvalue
################################################################################

# create a data frame to add p-values to boxplot
# run t_test to get proper data frame format, but will need to change p values
stat.test <- plot_data %>%
  group_by(treatment) %>%
  t_test(titer ~ time) %>%
  adjust_pvalue(method = "bonferroni")

# change p values in stat.test to proper values
stat.test$p <- pvals$p
stat.test$p.adj <- pvals$p.adj

# add significance stars and xy coordinates for plotting
stat.test <- stat.test %>%
  add_significance() %>% 
  add_xy_position(x = "time")

# adjust p value bar y positions manually
stat.test$y.position <- c(3.9, 4.3, 4.7, 5.1, 5.5, 5.9)

################################################################################
# Create Plot
################################################################################

plot <- plot_data %>%
  mutate(log10_titer = log(titer, base = 10),
         time = case_when(
           time == "Pre" ~ "Preimmune",
           TRUE ~ time
         )) %>%
  filter(!grepl("PBS", treatment)) %>%
  ggboxplot(
    x = "time", 
    y = "titer", 
    fill = "time",
    facet.by = "treatment",
    panel.labs = list(treatment = c("F1 Antigen", "LcrV Antigen")),
    bxp.errorbar = TRUE,
    bxp.errorbar.width = 0.15,
    label.rectangle = TRUE,
    repel = TRUE,
    width = 0.7,
  ) +
  stat_pvalue_manual(data = stat.test,
                     bracket.size = 1,
                     label.size = 10,
                     hide.ns = TRUE) +
  geom_beeswarm(aes(shape = sex, fill = time), 
                side = 0, # both directions jittering
                dodge.width = 0.7, # change width between shapes 
                size = 4.5, # adjust point size
                cex = 3, # adjust point spacing
                stroke = 0.7) +
  scale_shape_manual(values = c(21,24),
                     name = "Sex") +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) + 
  annotation_logticks(sides = "l") +
  theme(strip.text.x = element_text(size = 28), # change facet label text size
        strip.background = element_blank())
plot

# customize plot further
plot <- ggpar(plot, 
              # main = "Figure 3: Humoral Immune Response - Statistical Analysis",
              #  font.main = c(24,"bold"),
              #  submain = "IgG antibody titers in six African green monkeys immunized with the ChAdOx1 plague vaccine.\nPermutation tests for one-way repeated measures ANOVA were performed with a Bonferroni\nadjustment for multiple testing. **, P < 0.01",
              #  font.submain = c(24),
              font.x = c(28),
              font.y = c(28),
              font.tickslab = c(24),
              font.legend = c(24),
              xlab = "Serum",
              ylab = "Total IgG Titer",
              palette = c("darkgoldenrod1", "dodgerblue2", "#72BF7F"),
              legend = "right",
              legend.title = "Serum")
plot

################################################################################
# Export Plot
################################################################################

ggsave(filename = "/Users/charlescostanzo/College/Su 2023/Chopra/Poster Code/Figures/figure2.jpg",
       plot = plot,
       width = 14.04,
       height = 7.01,
       units = "in",
       dpi = 400)