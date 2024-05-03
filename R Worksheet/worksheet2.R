# Anthony Grieco
# Foundations of Data Science and Analytics
# 2/7/2024

library(tidyverse)  #(#1)

over_21 <- TRUE  #(#2)

if (over_21 == TRUE){  #(#3)
  print("You are over 21")
} else{
  print("You are under 21")
}

ages <- c(20, 22, 21, 21, 22)  #(#4)
counter <- 0
for (val in ages){
  if (val >= 21){
    counter <- counter + 1
  }
}
counter

my_nums <- sample(1:100, 10)  #(#5)
index <- 1
greaterThan50 <- 0
lowerThan50 <- 0
while (index <= length(my_nums)){
  if (my_nums[index] > 50){
    greaterThan50 <- greaterThan50 + 1
  } else if (my_nums[index] < 50){
    lowerThan50 <- lowerThan50 + 1
  }
  index <- index +1
}
sprintf("Number of Items Greater Than 50: %s", greaterThan50)
sprintf("Number of Items Lower Than 50: %s", lowerThan50)

dt <- read.table("qsize.csv", sep = ",")  #(#6)

colnames(dt) <- c(dt$V1[1], dt$V2[1], dt$V3[1], dt$V4[1]) #(#7)
dt <- dt[-1,]
# view(dt)
summary(as.integer(dt$Brain))

plot(as.integer(dt$Brain), as.integer(dt$PIQ))  #(#8)

simple.lm = lm(as.integer(dt$Brain) ~ as.integer(dt$PIQ))  #(#9)
abline(simple.lm, col="red", lwd=3)

custdata <- read.table("custdata.tsv", sep = '\t')  #(#10)
colnames(custdata) <- c(custdata$V1[1], custdata$V2[1], custdata$V3[1], custdata$V4[1], custdata$V5[1], custdata$V6[1], custdata$V7[1], custdata$V8[1], custdata$V9[1], custdata$V10[1], custdata$V11[1])
custdata <- custdata[-1,]
# View(custdata)

summary(custdata)   #(#11)
sprintf("The 'health.ins' variable is of the %s type.", typeof(custdata$health.ins))
index <- 1
missingElem <- 0
while (index <= length(custdata$recent.move)){
  if(is.na(custdata$recent.move[index]) == TRUE){
    missingElem <- missingElem + 1
  }
  index <- index + 1
}
sprintf("There are %s missing data elements housed within the 'recent.move' variable.", missingElem)

library(ggplot2)  #(#12)
ggplot(custdata) +geom_histogram(aes(x=as.integer(custdata$age)), binwidth = 5, fill="blue")

ggplot(custdata) + geom_bar(aes(custdata$marital.stat), fill="blue")  #(#13)