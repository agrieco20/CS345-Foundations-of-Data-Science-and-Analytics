library(tidyverse)

getwd() #Gets the current directory and returns it to the console

list.files() #Returns all the files in the current working directory

num1 <- 9 #Assigns "num1" a value of 9
rm(num1) #Removes the variable "num1" from the environment

num1 = 9 #Another way to assign "num1" a value of 9

num1 #Prints the value of num1 to the console

class(num1) #Returns the class of the value being stored in num1
typeof(num1) #Returns the type of value (within its umbrella class) being stored in num1 (always a double by default)

num2 = 3.75
num2
typeof(num2)

num2 = 4L #Assigns num2 a specific integer value of 4
typeof(num2)

as.integer(num2) #Casts the value being stored in num2 as an explicit integer

num2 <- as.integer(num2)

num3 <- 3i #Complex Data Type (useful in things like mathematical formulas)
num3

num2 + num3

num2 * 9 + num3

married <- TRUE #Boolean Data Type
married
!married #Negates value stored in "married"
married <- FALSE
married

myString <- "Adam Albina" #Character Data Type (functionally the same as a String)
myString

typeof(myString)

length(myString) #DO NOT USE (useful only in returning the length of a matrix)
str_length(myString) #Returns actual length of a string
str_to_lower(myString) #Casts all characters in "myString" to lower case (result not stored)
str_to_upper(myString) #Casts all characters in "myString" to upper case (result not stored)

myVector <- c(1, 2, 3, 4) #Creates a vector (acts just like an array in Java); "c(...)" is the combine function
myVector

myVector <- c(1:10, c(20:40))
myVector

myVector <- c(1:4)
myVector + 10
myVector2 <- myVector + 10
rm(myVector2)

myList <- list(1:3) #Lists can store any number of different types of variables at once (vectors can only ever store 1 data type at a time) 

myList <- list(c(1:3), "A", "B", "C", "Baby you and Me") #Lists are not flattened (unlike Vectors)

attr(myList, "type_attribute") <- "This is a list" #Attributes store metadata about the data
myList

vec1<-(c(a=1, b=2, c=3))
vec1
vec1 <- vec1 + 10
vec1

vec1<-1:3; names(vec1) <- c("a", "b", "c") #"Names" assigns columns metadata in vectors
vec1

myFactor <- c("Graduate", "Undergraduate", "High School", "GED")
myFactor

myFactor <- as.factor(myFactor)

myFactor[4] <- "Middle School" #Returns Error because "Middle School" isn't a pre-established level/factor

m <- matrix(1:100, nrow = 10, ncol = 10) #Matrix (assigned as 10 x 10 block) given values 1 - 100
dim(m) #Returns the dimensions of Matrix m

myMatrix <- matrix(1:12, nrow = 4, ncol=3)
length(myMatrix)

nrow(myMatrix) #Returns number of rows that myMatrix has
ncol(myMatrix) #Returns number of columns that myMatrix has

colnames(myMatrix) <- c("One", "Two", "Three") #Assigns the names of the columns in myMatrix
rownames(myMatrix) <- c("Row1", "Row2", "Row3", "Row4") #Assigns the names of the rows in myMatrix
myMatrix

m<-matrix(rnorm(100, mean = 50, sd=10), nrow=10) #Creates a matrix containing a random assortment of numbers falling within the 1-100 range with a mean of 50 and standard deviation of 10
m

m[9,9] #Returns data element in the "m" matrix at spot (9,9)

m[9,9] <- NA #The data at point (9,9) in Matrix "m" is set equal to missing (NA)
m

missing <- is.na(m) #Checks to see if any data in the Matrix "m" is missing
missing

summary(m) #Gives the mean, median, minimum, maximum, [etc] of the given data set (also quickly tells you if there is any data missing in the data set)

df <- data.frame(x=c("red", "white", "white", "red", "red", "red"), y = c(80, 83, 94, 83, 79, 95)) #Data Frame (most common data structure in R)
view(df)
summary(df)

colnames(df)<-c("type", "rating") #Assigns column names of the data frame
df$type<-as.factor(df$type)
summary(df)

view(mtcars)
help(mtcars)
mtcars$mpg
hist(mtcars$mpg) #Creates a histogram using the mtcars mpg data
plot(mtcars$hp, mtcars$disp) #Creates Scatterplot
