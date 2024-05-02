#Author: Anthony Grieco
#Date: 4/2/2024
#Class: Foundations of Data Science and Analytics

library(tidyverse)
#library(dplyr)

#--------------------------------------------------
#Project 1
insurance <- read_csv("insurance_costs.csv")
summary(insurance)
glimpse(insurance)

#Checks whether age plays a factor in insurance charges (it appears as though the older one gets, the more expensive their insurance is)
ggplot(data = insurance, aes(x = insurance$age, y = insurance$charges)) +
  geom_point()

ageCharge <- lm(insurance$charges ~ insurance$age, data = insurance)
summary(ageCharge)

cor(insurance$age, insurance$charges)

#Checks whether someone is a smoker plays a factor in insurance charges (smoking plays a factor)
ggplot(data = insurance, aes(x = insurance$smoker, y = insurance$charges)) +
  geom_point()

smokerCharge <- lm(insurance$charges ~ insurance$smoker, data = insurance)
summary(smokerCharge)

cor(insurance$smoker, insurance$charges)

#---
#Checks whether age plays factor in insurance cost for nonsmokers
nSmoker <- insurance %>%
  filter(insurance$smoker == 'no')

ggplot(data = nSmoker, aes(x = nSmoker$age, y = nSmoker$charges)) +
  geom_point() + 
  geom_smooth(method="lm", se = FALSE, color = 'red') +
  labs(x="Non-Smoker's Age (Years)", y = "Insurance Charges ($)")

nSmokerLn <- lm(nSmoker$charges ~ nSmoker$age, data = nSmoker)
summary(nSmokerLn)

cor(insurance$age, insurance$charges)

#Checks whether age plays factor in insurance cost for smokers
ySmoker <- insurance %>%
  filter(insurance$smoker == 'yes')

ggplot(data = ySmoker, aes(x = ySmoker$age, y = ySmoker$charges)) +
  geom_point() + 
  geom_smooth(method="lm", se = FALSE, color = 'red') +
  labs(x="Smoker's Age (Years)", y = "Insurance Charges ($)")

ySmokerLn <- lm(ySmoker$charges ~ ySmoker$age, data = ySmoker)
summary(ySmokerLn)

cor(ySmoker$age, ySmoker$charges)

#--------------------------------------------------
#Project 2
diamonds <- read_csv("diamonds.csv")
summary(diamonds)
glimpse(diamonds)

#Checks whether the carat of a diamond impacts its price (it does)
ggplot(data = diamonds, aes(x = diamonds$carat, y = diamonds$price)) +
  geom_point()

caratPrice <- lm(diamonds$price ~ diamonds$carat, data = diamonds)
summary(caratPrice)

cor(diamonds$carat, diamonds$price)

#Checks whether the cut (quality) of a diamond impacts its price
ggplot(data = diamonds, aes(x = diamonds$cut, y = diamonds$price)) +
  geom_point()

cutPrice <- lm(diamonds$price ~ diamonds$cut, data = diamonds)
summary(cutPrice)

cor(diamonds$cut, diamonds$price)

#Checks whether the color of a diamond impacts its price
ggplot(data = diamonds, aes(x = diamonds$color, y = diamonds$price)) +
  geom_point()

colorPrice <- lm(diamonds$color ~ diamonds$color, data = diamonds)
summary(colorPrice)

cor(diamonds$color, diamonds$price)

#---
#Checks whether a Premium Cut plays a factor in diamond price
premCut <- diamonds %>%
  filter(diamonds$cut == 'Premium')

ggplot(data = premCut, aes(x = premCut$carat, y = premCut$price)) +
  geom_point(aes(color = premCut$color)) + 
  scale_color_manual(values=c("darkred", "red", "orange", "yellow", "green", "blue", "purple")) + #Manually allows you to assign colors
  geom_smooth(method="lm", se = FALSE, color = 'black') +
  coord_cartesian(xlim=c(0,4),ylim=c(0,20000)) + #Used to fix the regression line (geom_smooth) so that it would display the data more fully and reduce the extra margin where the regression line appeared but no data was actually present
  labs(x="Premium Cut Diamond's Carat (Grams)", y = "Price ($)", color = "Diamond Color")

premCutLn <- lm(premCut$price ~ premCut$carat, data = premCut)
summary(premCutLn)

cor(premCut$carat, premCut$price)

#Checks whether an Ideal Cut plays a factor in diamond price
idealCut <- diamonds %>%
  filter(diamonds$cut == 'Ideal')

ggplot(data = idealCut, aes(x = idealCut$carat, y = idealCut$price)) +
  geom_point(aes(color = idealCut$color)) + 
  scale_color_manual(values=c("darkred", "red", "orange", "yellow", "green", "blue", "purple")) + #Manually allows you to assign colors
  geom_smooth(method="lm", se = FALSE, color = 'black') +
  coord_cartesian(xlim=c(0,4),ylim=c(0,20000)) + #Used to fix the regression line (geom_smooth) so that it would display the data more fully and reduce the extra margin where the regression line appeared but no data was actually present
  labs(x="Ideal Cut Diamond's Carat (Grams)", y = "Price ($)", color = "Diamond Color")

idealCutLn <- lm(idealCut$price ~ idealCut$carat, data = idealCut)
summary(idealCutLn)

cor(idealCut$carat, idealCut$price)

#Checks whether a Very Good Cut plays a factor in diamond price
veryGoodCut <- diamonds %>%
  filter(diamonds$cut == 'Very Good')

ggplot(data = veryGoodCut, aes(x = veryGoodCut$carat, y = veryGoodCut$price)) +
  geom_point(aes(color = veryGoodCut$color)) + 
  scale_color_manual(values=c("darkred", "red", "orange", "yellow", "green", "blue", "purple")) + #Manually allows you to assign colors
  geom_smooth(method="lm", se = FALSE, color = 'black') +
  coord_cartesian(xlim=c(0,4),ylim=c(0,20000)) + #Used to fix the regression line (geom_smooth) so that it would display the data more fully and reduce the extra margin where the regression line appeared but no data was actually present
  labs(x="Very Good Cut Diamond's  Carat (Grams)", y = "Price ($)", color = "Diamond Color")

veryGoodCutLN <- lm(veryGoodCut$price ~ veryGoodCut$carat, data = veryGoodCut)
summary(veryGoodCutLN)

cor(veryGoodCut$carat, veryGoodCut$price)

#Checks whether a Good Cut plays a factor in diamond price
GoodCut <- diamonds %>%
  filter(diamonds$cut == 'Good')

ggplot(data = GoodCut, aes(x = GoodCut$carat, y = GoodCut$price)) +
  geom_point(aes(color = GoodCut$color)) + 
  scale_color_manual(values=c("darkred", "red", "orange", "yellow", "green", "blue", "purple")) + #Manually allows you to assign colors
  geom_smooth(method="lm", se = FALSE, color = 'black') +
  coord_cartesian(xlim=c(0,4),ylim=c(0,20000)) + #Used to fix the regression line (geom_smooth) so that it would display the data more fully and reduce the extra margin where the regression line appeared but no data was actually present
  labs(x="Good Cut Diamond's  Carat (Grams)", y = "Price ($)", color = "Diamond Color")

GoodCutLn <- lm(GoodCut$price ~ GoodCut$carat, data = GoodCut)
summary(GoodCutLn)

cor(GoodCut$carat, GoodCut$price)

#Checks whether a Fair Cut plays a factor in diamond price
fairCut <- diamonds %>%
  filter(diamonds$cut == 'Fair')

ggplot(data = fairCut, aes(x = fairCut$carat, y = fairCut$price)) +
  geom_point(aes(color = fairCut$color)) + 
  scale_color_manual(values=c("darkred", "red", "orange", "yellow", "green", "blue", "purple")) + #Manually allows you to assign colors
  geom_smooth(method="lm", se = FALSE, color = 'black') +
  coord_cartesian(xlim=c(0,4),ylim=c(0,20000)) + #Used to fix the regression line (geom_smooth) so that it would display the data more fully and reduce the extra margin where the regression line appeared but no data was actually present
  labs(x="Fair Cut Diamond's Carat (Grams)", y = "Price ($)", color = "Diamond Color")

fairCutLn <- lm(fairCut$price ~ fairCut$carat, data = fairCut)
summary(fairCutLn)

cor(fairCut$carat, fairCut$price)
