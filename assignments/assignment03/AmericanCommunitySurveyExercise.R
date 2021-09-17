# Assignment: ASSIGNMENT 3
# Name: Farrell, Amelia
# Date: 2021-09-13

## Set the working directory
setwd("C:/Users/Amelia/Documents/Bellevue/dsc520")

#install.packages("pastecs") 

## Load data file
surveydata <- read.csv(file = 'data/acs-14-1yr-s0201.csv')
str(surveydata)
nrow(surveydata)
ncol(surveydata)

## Load the ggplot2 package
library(ggplot2)
theme_set(theme_minimal())

## Frequency Histogram of 'HSDegree` variable
ggplot(surveydata, aes(HSDegree)) + geom_histogram(bins = 10, binwidth=2, fill="#69b3a2", color="#e9ecef") + 
  ggtitle("% of US County with a High Shool Degree") + xlab("Percent") + ylab("Number of US Counties")

## Density Histogram of 'HSDegree` variable with normal curve
ggplot(surveydata, aes(HSDegree)) + geom_histogram(mapping = aes(y = stat(density)), bins = 10, binwidth=2, fill="#69b3a2", color="#e9ecef") + 
  ggtitle("% of US County with a High Shool Degree") + xlab("Percent") + ylab("Number of US Counties") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(surveydata$HSDegree), 
                            sd = sd(surveydata$HSDegree)))
                                                                                                                                                                                                                        
## Probability Plot of 'HSDegree` variable
ggplot(surveydata) + geom_qq(aes(sample = HSDegree))

## quantify normality with numbers using the stat.desc() function
library(pastecs)
stat.desc(surveydata, norm = TRUE)
