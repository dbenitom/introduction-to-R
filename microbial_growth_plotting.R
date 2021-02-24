
# bits of script for R course  --------------------------------------------



# B. microbial growth -----------------------------------------------------

# load required packages ####
require(readxl)
require(ggplot2)
require(car)
require(tidyr)
require(dplyr)
require(gtools)
require(nls2)

# 1. data import ####

raw_growth_data<-read_excel("data/Maribacter_forsetti_OD.xlsx")


# 2. exploratory plotting glucose ####

ggplot(data=raw_growth_data)+
  geom_point(aes(x=time_h, y=glucose))

# Comments: 
# a. plot looks like ass
# b. sigmoidal shape

ggplot(data=raw_growth_data)+
  geom_point(aes(x=time_h, y=logit(glucose/max(glucose))))

glucose_growth_phase<-raw_growth_data %>%
  filter(time_h<180L)

ggplot(data=glucose_growth_phase)+
  geom_point(aes(x=time_h, y=logit(glucose/max(glucose))))

model_glucose<-lm(glucose~time_h, glucose_growth_phase)

logit_model_glucose<-lm(car::logit(glucose/max(glucose))~time_h, glucose_growth_phase)
summary(logit_model_glucose)


ggplot(data=raw_growth_data)+
  geom_point(aes(x=time_h, y=glucose))+
  geom_line(aes(x=time_h, y=time_h*model_glucose$coefficients[2]+model_glucose$coefficients[1]))+
  geom_line(aes(x=time_h, 
                y=inv.logit(time_h*logit_model_glucose$coefficients[2]+logit_model_glucose$coefficients[1])*max(glucose)))


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


