# Anthony Grieco
# Foundations of Data Science and Analytics
# 1/30/2024

# install.packages('tidyverse')
library(tidyverse) #(#1)

getwd() #(#2)

list.files() #(#3)

my_sum <- 2024 + 21 #(#4)

# I am exactly 21 years old (#5)

typeof(my_sum) #(#6)

myVector <- 1:5L
myVector #(#7)

myVector2 <- 1.5:100
myVector2 #(#8)

myVector2 + 0.5 #(#9)

sqrt(myVector2) #(#10)

myVector3 <- myVector2[1:5] 
myVector3 #(#11)

myVector[1:5] * myVector3[1:5] #(#12)

classes <- factor(c("tiny", "small", "medium", "large", "huge"))
classes #(#13)

my_range <- 10:50L
my_range #(#14)

my_favs <- c(3, "Anthony", "Gnocchi", 4.0)
my_favs[3] #(#15)

my_matrix <- matrix(1:25, nrow = 5, ncol = 5)
my_matrix #(#16)

attributes(my_matrix) #(#17)

dimnames(my_matrix) <- list(c("R1", "R2", "R3", "R4", "R5"), c("C1", "C2", "C3", "C4", "C5"))
my_matrix #(#18)

my_matrix[21] <- NA
my_matrix #(#19)

logicVector <- is.na(my_matrix)
logicVector #(#20)

df <- data.frame(gender = c("M", "M", "F", "M"), fname = c("Rob", "Dave", "Mary", "Dean"))
df #(#21)

names(df[2]) #(#22)

summary(df) #(#23)

mtcars #(#24)

cars <- data.matrix(mtcars)
mpg <- cars[1:32]
hp <- cars[97:128]
plot (mpg, hp) #(#25)