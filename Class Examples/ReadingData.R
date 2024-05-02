library(tidyverse)

#boolean variables - TRUE and FALSE
married <- TRUE
#Quick discussion of negation
!married

happy <- TRUE
happy

#Syntax for if else
# if (expression){
#   do something
#}

if (married){
  print("I see you are married.")
}

if (!married){
  print("I see that you are not married.")
}

if (married && happy){
  print("I see that you are happily married.")
}

if (married && !happy){
  print("I see that you are happily married.")
}

happy <- FALSE
if (married || happy){ #OR
  print("I see that you are happily married.")
}

happy <- TRUE
if (married){
  print("I see that you are married.")
}else {
  print("I see that you are single")
}

myVector <- sample(1:1000, 100, replace = TRUE)
myVector
length(myVector) #Returns length of myVector
length(unique(myVector)) #Returns number of unique items in myVector
dups <- duplicated(myVector)

#FOR LOOP
counter <- 0
for (val in myVector){
  if (val >= 500){
    counter <- counter + 1
  }
}
counter

#WHILE LOOP

# Syntax:
# 
# while (condition is TRUE){
#   do this
#   do that 
#   do other thing
#   Set other condition
# }

greater <- 0
count <- 1
while (count <= length(myVector)){
  if(myVector[count] >= 500){
    greater <- greater + 1
  }
  count <- count + 1
}
greater

#--------------
mtcars
plot(mtcars)
plot(mtcars$hp,mtcars$mpg)
simple.lm = lm(mpg ~ hp, data=mtcars)
abline(simple.lm, col="red", lwd=3)
options(scipen = 999) #Turns off scientific notation
summary(simple.lm)
#--------------

#Get some data into a table
arrests <- read.table("USArrests.csv", sep = ",")
view(arrests)
arrests
summary(arrests)
getwd()
USArrests<-read_csv("USArrests.csv")
USArrests
summary(USArrests)
colnames(USArrests) [1] <- "State"
USArrests
view(USArrests)
boxplot(USArrests$UrbanPop, main="UrbanPop")
USArrests[1, 2] <- 13.5

#Example of Discretizing/Binning the Data
USArrests$cat <- cut(USArrests$UrbanPop, 
                     breaks=c(0,49,59,69, 100), 
                     labels=c("small", "medium", "large", "x-large"))
view(USArrests)
plot(USArrests$cat, USArrests$Murder)

crimeDF <- read_csv("FBI_Crimes_Property_by_Field_Office_2020.csv")
view(crimeDF)
summary(crimeDF)
names(crimeDF)[1]<-"City"
rownames(crimeDF) <- crimeDF$City
barplot(height=crimeDF$robbery, names = crimeDF$City, horiz=TRUE, las=1)
