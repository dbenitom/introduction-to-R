
# for introduction to R course  --------------------------------------------
# B. first steps into the tidyverse and plotting ---------------------------

# load required packages ####
require(readxl)
require(ggplot2)
require(car)
require(dplyr)
require(gtools)


# data import ####

raw_growth_data<-read_excel("data/Maribacter_forsetti_OD.xlsx")

head(raw_growth_data)

# 1. introduction dplyr ####
#The [dplyr](https://dplyr.tidyverse.org/) package allows you to interact with 
#data frames and tibbles (a task most biologist will be doing). The package is  
#easy to learn because it is based around using verbs to manipulate your data 
#frames. In fact, dplyr is referred to as, "the grammar of data manipulation." 
#Here are the verbs we will learn about today:
  
# select(): return specific columns of a data frame
# filter(): extract rows of a data frame that meet specified conditions
# summarise(): return summary statistics for your data frame
# mutate(): add a new column to your data frame
# rename(): rename the title of a column


# how to pipe with %>%

# Which timepoints were measured?
raw_growth_data %>%
  select(time_h)

# when was the the OD in the glucose incubation higher than 0.2?
raw_growth_data %>%
  filter(glucose>0.2)%>%
  select(c(time_h, glucose))
  
# what was the max OD of the glucose incubation? note that "max_OD" is just a name, could be anything
raw_growth_data %>%
  summarise(max_OD= max(glucose))
  
# what was the mean OD of the glucose incubation between 60 h and 180 h incubation?
raw_growth_data %>%
  filter(time_h> 60 & time_h<180) %>%
  summarise(mean_OD_60to180h = mean(glucose), measurements = n())


# Exercises
# a. look at the time and the ODs of the arabinan and the debranched arabinan together
# b. report the maximum OD for arabinan and the maximum OD for debranched arabinan
# c. choose a time interval and calculate the mean OD of the arabinan and the debranched arabinan incubation
# BONUS: also calculate the standard deviation for the chosen time interval



# save information derived from data set using <-
times<-raw_growth_data %>%
  select(time_h)

glucose_info <- raw_growth_data %>%
  filter(time_h > 100)%>%
  summarise(mean_glucose = mean(glucose), sd_glucose = sd(glucose))

control_info <- raw_growth_data %>%
  filter(time_h > 100)%>%
  summarise(mean_control = mean(control), sd_control = sd(control))

raw_growth_data %>%
  mutate(glucose-control)

difference<- raw_growth_data %>%
  mutate(difference = glucose-control)%>%
  select(difference)

raw_growth_data$difference <- difference
raw_growth_data %>%
  rename(diff = difference)


# Excercise
# calculate the difference between arabinan and debranched arabinan and add it as a new column



# 2. explore plotting with ggplot2 ####

ggplot(data=raw_growth_data)+
  geom_point(aes(x=time_h, y=glucose))

ggplot(data=raw_growth_data)+
  geom_point(aes(x=time_h, y=glucose))+
  geom_point(aes(x=time_h, y=control), color="blue", size=2, pch=25, fill="pink")+
  theme_light()


# Exercises
# a. try out plotting different incubations like arabinan etc.
# b. try different colors
# c. try to use geom_line instead of geom_point
# BONUS: have lunch


# Comments?

# interested in microbial growth?

# need to improve digestibility!

# sigmoidal shape

ggplot(data=raw_growth_data)+
  geom_point(aes(x=time_h, y=car::logit(glucose/max(glucose))))+
  theme_light()

glucose_growth_phase<-raw_growth_data %>%
  filter(time_h<180)

ggplot(data=glucose_growth_phase)+
  geom_point(aes(x=time_h, y=car::logit(glucose/max(glucose))))+
  theme_light()

model_glucose<-lm(glucose~time_h, glucose_growth_phase)

logit_model_glucose<-lm(car::logit(glucose/max(glucose))~time_h, glucose_growth_phase)
summary(logit_model_glucose)


ggplot(data=raw_growth_data)+
  geom_point(aes(x=time_h, y=glucose))+
  geom_line(aes(x=time_h, y=time_h*model_glucose$coefficients[2]+model_glucose$coefficients[1]))+
  geom_line(aes(x=time_h, 
                y=inv.logit(time_h*logit_model_glucose$coefficients[2]+logit_model_glucose$coefficients[1])*max(glucose)))+
  theme_light()


model_time<-seq(0,350,1)
model_max<-as.numeric(raw_growth_data %>%
  summarise(max(glucose)))

ggplot()+
  geom_point(data=raw_growth_data, aes(x=time_h, y=glucose))+
  geom_line(aes(x=model_time, y=model_time*model_glucose$coefficients[2]+model_glucose$coefficients[1]))+
  geom_line(aes(x=model_time, 
                y=inv.logit(model_time*logit_model_glucose$coefficients[2]+logit_model_glucose$coefficients[1])*model_max))+
  theme_light()


# take a different substrate ####

ggplot(data=raw_growth_data)+
  geom_point(aes(x=time_h, y=arabinan))

# Comments: 
# a. plot looks like ass
# b. sigmoidal shape

ggplot(data=raw_growth_data)+
  geom_point(aes(x=time_h, y=logit(arabinan/max(arabinan))))

arabinan_growth_phase<-raw_growth_data %>%
  filter(time_h<150L)

model_arabinan<-lm(arabinan~time_h, arabinan_growth_phase)

logit_model_arabinan<-lm(car::logit(arabinan/max(arabinan))~time_h, arabinan_growth_phase)
summary(logit_model_arabinan)


ggplot(data=raw_growth_data)+
  geom_point(aes(x=time_h, y=arabinan))+
  geom_line(aes(x=time_h, y=time_h*model_arabinan$coefficients[2]+model_arabinan$coefficients[1]))+
  geom_line(aes(x=time_h, 
                y=inv.logit(time_h*logit_model_arabinan$coefficients[2]+logit_model_arabinan$coefficients[1])*max(arabinan)))+
  theme_classic()


