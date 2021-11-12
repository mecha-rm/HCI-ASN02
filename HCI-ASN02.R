# Names: 
# * Adam Kahil (100655089)
# * Eric Aivaliotis (100700292)
# * Hao Tian Guan (100709845)
# * Roderick "R.J." Montague (100701758)
#
# Date: 11/11/2021
#
# Description: assignment 2 for human-computer interaction course.
#
# References:
# - https://www.rdocumentation.org/packages/openxlsx/versions/4.2.4/topics/read.xlsx 

# included libraries
library(tidyverse)
library(ggpubr)
library(rstatix)
library(dplyr)
library(ggplot2)

library(pastecs) # for levene test
library(psych) # by() function describe
library(car) # durbinWatsonTest
library(readxl) # read in XLSL
library(ez) # ezANOVA
library(stats) # post-hoc

# Exporting Information #
auto_export <- FALSE # automatically export graph
export_path <- "exports" # export path from working directory

##############
# QUESTION 1 #
##############
set.seed(5)
my_data<-data.frame(immersion = 
                      c(rnorm(12,70,10),rnorm(12,75,10),rnorm(12,100,10)),
                    group = gl(3,12, labels = c("sitting","standing","walking")))

my_data

# general information
summary(my_data)
levels(my_data$group)

group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(immersion, na.rm = TRUE),
    sd = sd(immersion, na.rm = TRUE)
  )

# Three groups of participants/ three configurations (sitting, standing, walking)
# Only one configuration per person (no shared members) - results are independent.
# Thus, One Way ANOVA is used (3 or more independent/unrelated groups)

# - GRAPH - #
# Box Plot
ggboxplot(my_data, x = "group", y = "immersion",
          color = "group", palette = c("RED", "GREEN", "BLUE"),
          order = c("sitting", "standing", "walking"),
          title = "Question 1 - User Study Box Plot (With Outliers)",
          ylab = "Immersion", xlab = "Group")

# if graph should be automatically exported.
if(auto_export) {
  ggsave(filename = "hci-asn02_q1_box_plot.png", path = export_path)
  ggsave(filename = "hci-asn02_q1_box_plot.eps", path = export_path)
  
}

# Bar Plot with Error Bars
bar <- ggplot(my_data, aes(group, immersion))
bar + stat_summary(fun = mean, geom = "bar") + 
  stat_summary(fun.data = mean_sd,  geom = "errorbar", width = 0.2) + 
  labs(title = "Question 1 - User Study Bar Chart (With Outliers)", x = "Group", y = "Immersion")

# if graph should be automatically exported.
if(auto_export) {
  ggsave(filename = "hci-asn02_q1_bar_graph_with_error_bars.png", path = export_path)
  ggsave(filename = "hci-asn02_q1_bar_graph_with_error_bars.eps", path = export_path)
}

# -ASSUMPTIONS- #

# Normality #
if (!require(rstatix)) install.packages(rstatix)

my_data %>%
  group_by(group) %>%
  shapiro_test(immersion) # get p-value for homogeneity check

# sitting returned 0.106
# standing returned 0.909
# walking returned 0.346
# all are above 0.05, meaning they likely have normality.

# making a graph to show normality
if (!require(ggplot2)) install.packages(ggplot2)

ggqqplot(my_data, x = "immersion", facet.by = "group", title = "HCI - ASN02 - Question 1 - QQPlot for Normality")

# export graph
if(TRUE) {
  ggsave(filename = "hci-asn02_q1_norm_qqplot.png", path = export_path)
  ggsave(filename = "hci-asn02_q1_norm_qqplot.eps", path = export_path)
}

by(my_data$immersion, my_data$group, describe)

###
# Sample Independence #
if (!require(car)) install.packages(car)

my_data_anova <- aov(immersion ~ group, my_data)
durbinWatsonTest(my_data_anova)

# returns p-value of 0.246

###
# Variance Equality #
if(!require(pastecs)) install.packages(pastecs)

leveneTest(immersion ~ group, my_data)
# levene test returned p-value of 0.9657, which is above 0.05

oneway.test(immersion ~ group, my_data)

###
# Outliers #

# ALL
# This was taken out because it was not needed.
# getting the differences
# my_data_no <- with(my_data,
#          immersion[group == "sitting"] - immersion[group == "standing"] - immersion[group == "walking"])

# boxplot to check for outliers in the differences
# ggboxplot(my_data_no, xlab = "data", ylab = "differences")

# INDIVIDUAL
# sitting
my_data_sit <- with(my_data, immersion[group == "sitting"])
ggboxplot(my_data_sit, title = "HCI - ASN02 - Question 1 - Box Plot - Sitting", 
          xlab = "data", ylab = "differences", color = "RED")

# export graph
if(auto_export) {
  ggsave(filename = "hci-asn02_q1_box_plot_sit.png", path = export_path)
  ggsave(filename = "hci-asn02_q1_box_plot_sit.eps", path = export_path)
}

# standing
my_data_stand <- with(my_data, immersion[group == "standing"])
ggboxplot(my_data_stand, title = "HCI - ASN02 - Question 1 - Box Plot - Standing", 
          xlab = "data", ylab = "differences", color = "GREEN")

# export graph
if(auto_export) {
  ggsave(filename = "hci-asn02_q1_box_plot_stand.png", path = export_path)
  ggsave(filename = "hci-asn02_q1_box_plot_stand.eps", path = export_path)
}

# walking
my_data_walk <- with(my_data, immersion[group == "walking"])
ggboxplot(my_data_walk, title = "HCI - ASN02 - Question 1 - Box Plot - Walking", 
          xlab = "data", ylab = "differences", color = "BLUE")

# export graph
if(auto_export) {
  ggsave(filename = "hci-asn02_q1_box_plot_walk.png", path = export_path)
  ggsave(filename = "hci-asn02_q1_box_plot_walk.eps", path = export_path)
}

##############
# QUESTION 2 #
##############

# installing the required package.
if (!require(readxl)) install.packages(readxl)

SUS <- read_xlsx("imports/SUS.xlsx")
SUS

# a) Assumptions Tests for Mixed ANOVA (2 Way)

# NOTE: (TODO: remove this for te final submission)
# - Run assumption tests for Mixed Anova
# - There should be no problem with sphericity.
# - If something is not right, you need to acknowledge it.
# - check if the results are reliable, and how reliable they are.
# - For the most part, you're going to ignore even if there's problems.
# - With one way anova you can get rid of outliers, but when measuring the same person multiple times...
# - You would get rid of the participant, which is costly, especially if there's few people.

# Outliers - Checking for Significant Outliers #
SUS %>%
  group_by(Order, Tool) %>%
  identify_outliers(Score)


# Normality #
SUS %>%
  group_by(Order, Tool) %>%
  shapiro_test(Score)

# Assumption of Sphericity #
# Order is the between subject, Tool is the within subject 
res.aov <- anova_test(
  data = SUS, dv = Score, wid = Partcipants,
  between = Order, within = Tool
)

# print values
get_anova_table(res.aov)

# Homogeneity of Variances #
# TODO: FIX THIS.
if(!require(rstatix)) install.packages("rstatix")
if(!require(pastecs)) install.packages(pastecs)

# data("ToothGrowth")

# new
# both error out, saying that the gorup is coerced to a factor.
SUS %>%
  group_by(Order) %>%
  levene_test(Score ~ Tool)

levene_test(group_by(SUS, Order), Score ~ Tool)

SUS %>%
  group_by(Tool) %>%
  levene_test(Score ~ Order)

levene_test(group_by(SUS, Tool), Score ~ Order)

#

# TODO: figure out if these are how you're supposed to do it.
# original
# SUS %>%
#   group_by(Order, Tool) %>%
#   levene_test(Score ~ Partcipants)

# levene_test(group_by(SUS, Order, Tool), Score ~ Participants)

# order doesn't matter
# SUS %>%
#   group_by(SUS, SUS$Tool) %>%
#  levene_test(SUS$Score ~ SUS$Tool*SUS$Order)

SUS %>%
  group_by(SUS, SUS$Tool) %>%
  levene_test(SUS$Score ~ SUS$Order*SUS$Tool)

# order does not matter for this it seems
# levene_test(data = SUS, formula = Score~Tool*Order)
levene_test(data = SUS, formula = Score~Order*Tool) # proper order

# order doesn't matter
# levene_test(data = group_by(SUS, SUS$Tool), formula = Score~Tool*Order)
levene_test(data = group_by(SUS, SUS$Tool), formula = Score~Order*Tool) # proper order



# Homogeneity of Covariances #
box_m(SUS["Score"], SUS$Tool)

# b) Mixed ANOVA Test with ezANOVA
if(!require(ez)) install.packages("ez")

M_AnovaModel <- ezANOVA(data = SUS, dv = .(Score), 
                        wid = .(Partcipants), within = .(Tool), between = .(Order),
                        detailed = T, type = 3)

M_AnovaModel

# C) Interaction Plot(s)
if(!require(stats)) install.packages("stats")

# pariwise tests
pairwise.t.test(SUS$Score, interaction(SUS$Tool, SUS$Order), paired=T, p.adjust.method ="bonferroni")
pairwise.t.test(SUS$Score, interaction(SUS$Order, SUS$Tool), paired=T, p.adjust.method ="bonferroni")

# Interaction Plots (X = Order)
interaction.plot(x.factor = SUS$Order, trace.factor = SUS$Tool,
                 response = SUS$Score, fun = mean, type = "b", legend = TRUE, 
                 xlab = "Order", ylab="Score", col = c("RED", "BLUE"), trace.label ="Tool")

# Interaction Plots (X = Tool)
interaction.plot(x.factor = SUS$Tool, trace.factor = SUS$Order,
                 response = SUS$Score, fun = mean, type = "b", legend = TRUE, 
                 xlab = "Tool", ylab="Score", col = c("RED", "BLUE"), trace.label ="Order")

# Line Plot/Main Plot
ggp <- ggplot(SUS, aes(x=Order, y=Score, group =
                      Tool, c("RED", "BLUE"))) +
  geom_errorbar(aes(ymin=Score-sd(Score),
                    ymax=Score+sd(Score)), width=.1,
                position=position_dodge(0.05)) +
  geom_line(aes(linetype=Tool)) +
  geom_point(aes(shape=Tool)) +
  labs(x="Order", y = "Score") +
  theme_classic()

# doesn't seem to do anything.
ggp + theme_classic() +
  scale_color_manual(values=c("RED","BLUE"))


# d) Pairwise Post-HOC Tests

# pairwise post-hoc tests
pairwise.t.test(SUS$Score, SUS$Tool, paired=T, p.adjust.method ="bonferroni")
pairwise.t.test(SUS$Score, SUS$Order, paired=T, p.adjust.method ="bonferroni")


