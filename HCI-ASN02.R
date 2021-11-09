# Names: 
# * Adam Kahil (100655089)
# * Eric Aivaliotis (100700292)
# * Hao Tian Guan (100709845)
# * Roderick "R.J." Montague (100701758)
#
# Date: 11/09/2021
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

library(pastecs) # for lavene test
library(psych) # by() function describe
library(car) # durbinWatsonTest
library(readxl)

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
  ggsave(filename = "hci-asn02_q2_box_plot.png", path = export_path)
  ggsave(filename = "hci-asn02_q2_box_plot.eps", path = export_path)
  
}

# Bar Plot with Error Bars
bar <- ggplot(my_data, aes(group, immersion))
bar + stat_summary(fun = mean, geom = "bar") + 
  stat_summary(fun.data = mean_sd,  geom = "errorbar", width = 0.2) + 
  labs(title = "Question 1 - User Study Bar Chart (With Outliers)", x = "Group", y = "Immersion")

# if graph should be automatically exported.
if(auto_export) {
  ggsave(filename = "hci-asn02_q2_bar_graph_with_error_bars.png", path = export_path)
  ggsave(filename = "hci-asn02_q2_bar_graph_with_error_bars.eps", path = export_path)
  
}

# -ASSUMPTIONS- #

# Normality #
if (!require(rstatix)) install.packages(rstatix)

my_data %>%
  group_by(group) %>%
  shapiro_test(immersion) # get p-value for homogeneity check

# TODO: draw graph to show normality

# sitting returned 0.106
# standing returned 0.909
# walking returned 0.346
# all are above 0.05, meaning they likely have normality.

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
# lavene test returned p-value of 0.9657, which is above 0.05

oneway.test(immersion ~ group, my_data)

###
# Outliers #

# getting the differences
my_data_no <- with(my_data,
          immersion[group == "sitting"] - immersion[group == "standing"] - immersion[group == "walking"])

# ("sitting","standing","walking"

# boxplot to check for outliers in the differences
ggboxplot(my_data_no, xlab = "data", ylab = "differences")

# doesn't work - fix this (look at LAB06 for help)
# identify_outliers(group_by(my_data, my_data$group), my_data$immersion)
is_extreme(my_data$immersion)

##############
# QUESTION 2 #
##############

# installing the required package.
if (!require(readxl))
  install.packages(readxl)

SUS <- read_xlsx("imports/SUS.xlsx")

# a) Assumptions Tests for ANOVA ...

# b) Mixed ANOVA Test with ezANOVA
if(!require(ez)) 
  install.packages("ez")

library(ez)

M_AnovaModel <- ezANOVA(data = SUS, dv = .(Score), 
                        wid = .(Partcipants), within = .(Tool), between = .(Order),
                        detailed = T, type = 3)

M_AnovaModel
