# Names: 
# * Adam Kahil (100655089)
# * Eric Aivaliotis (100700292)
# * Hao Tian Guan (100709845)
# * Roderick "R.J." Montague (100701758)
#
# Date: 11/03/2021
#
# Description: assignment 2 for human-computer interaction course.
#
# References:
# - https://www.rdocumentation.org/packages/openxlsx/versions/4.2.4/topics/read.xlsx 

##############
# QUESTION 1 #
##############
set.seed(5)
my_data<-data.frame(immersion = 
                      c(rnorm(12,70,10),rnorm(12,75,10),rnorm(12,100,10)),
                    group = gl(3,12, labels = c("sitting","standing","walking")))

my_data

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
