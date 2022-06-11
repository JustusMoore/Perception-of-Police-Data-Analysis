#Perception of Police Study Analysis#

#The goal of this study was to use the Perception of Police Survey (POPS) to measure both African Americans and Whites perception of bias and general attitudes of the police.
#Two questionnaires were administered to participants through Google Forms; The Perception of Police Survey and a demographic questionnaire. 
#Hypothesis: African Americans would have an overall lower POPS score; with a lower score denoting greater perception of bias and less favorable perceptions of the police.

#Install necessary packages
library(tidyverse)
library(dplyr)
library(readr)

#Import Perception of Police Data
pops_data <- read_csv("Downloads/pops data.csv")
View(pops_data)

#In order to test hypothesis, need to filter responses into respective grouping variable (i.e. Race)#

#filter data to return only African American Results
African_American_Data <- pops_data %>% 
                          filter(`Please Identify Your Race`=="African American")

#filter data to return only White Results
White_data <- pops_data %>% 
                filter(`Please Identify Your Race`=="White")

#In order to compare grouping variable, need to calculate mean and standard deviation for Race# 

#calculate mean POPS scores for African American respondents and White respondents
mean_african_american<- mean(African_American_Data$`Average Score`)
mean_white <- mean(White_data$`Average Score`)
mean_difference <- (mean_white-mean_african_american)

#Calculate standard deviation for African American and White respondents
sd_african_american <- sd(African_American_Data$`Average Score`)
sd_white <-sd(White_data$`Average Score`)
mean_sd <-mean(sd_african_american/sd_white)

#Calculate the difference b/t race and score#

#Calculate Cohen's D b/t African American  and White respondents
Cohens_D <- (mean_difference/mean_sd)

#Visualization of data#

#Visualization of all Participants Perception of Police (POPS) Scores 
ggplot(pops_data, aes(x=`Participant ID`, y= `Average Score`, fill = `Please Identify Your Race`))+
  geom_col(position = "dodge")+
  labs(title = "Participants POPS Scores")

#Scatter plot of average Score by Age and Race
ggplot(data = pops_data)+
  geom_point(mapping = aes(x=`Please Identify Your Age`, y = `Average Score`))+
  facet_wrap(~`Please Identify Your Race`)

#When looking at White respondents responses, I noticed that there might be a correlation b/t age and perception of police
cor(White_data$`Please Identify Your Age`, White_data$`Average Score`)

#Conclusions#

#The data collected from this study supported my initial hypothesis, such that African American participants were more likely to have less favorable perceptions of the police.
#African American participants (M = 2.74, SD = 0.66), White participants (M = 3.19, SD = 0.94)
#This was a moderate difference (d = .65). 

  
