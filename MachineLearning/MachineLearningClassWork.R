library(tidyverse)
#install.packages(c("party","partykit"))  
#install.packages(c("rpart", "rpart.plot"))
# install.packages("ROCR")
library(ROCR) #needed for prediction function
library(rpart) # needed for straight regression trees and data partitioning
library(rpart.plot) #needed for plotting rpart trees
library(partykit)#needed for ctrees
#install.packages("randomForest")
library(randomForest)

#turns off scientific notation
options(scipen=999)

# Read the built in data and create an iris data set in R.
data(iris)
summary(iris)
glimpse(iris)

iris %>%
  ggplot(aes(x=Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(col=Species))

iris %>%
  ggplot(aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(col = Species))

# Let's add a row number to make it easy to do the dplyr join
iris <- iris %>% mutate(id = row_number(), .before=Sepal.Length)
summary(iris)
glimpse(iris)

set.seed(1234)
trainData <- iris %>% sample_frac(.70)
testData <- anti_join(iris, trainData, by = 'id')
trainData <- trainData %>% select(-id)
testData <- testData %>% select(-id)

# Let's look at Sepal.Width in a (conditional inference tree: CTree is a
# non-parametric class of regression trees embedding tree-structured
# regression models into a well defined theory of conditional inference
# procedures.)
form_tree1 <- Species ~ Sepal.Length
tree1 <- ctree(form_tree1, data=trainData)
# check the prediction
table(predict(tree1), trainData$Species)

tree1
#plot
plot(tree1)
plot(tree1, type="simple")


#predict from the model
testPred <- predict(tree1, newdata = testData)
table(testPred, testData$Species)


form_tree2 <- Species ~ Sepal.Width
tree2 <- ctree(form_tree2, data=trainData)
# check the prediction
table(predict(tree2), trainData$Species)

#plot
plot(tree2, type="simple")

#predict from the model
testPred <- predict(tree2, newdata = testData)
table(testPred, testData$Species)

myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_tree <- ctree(myFormula, data=trainData)
# check the prediction
table(predict(iris_tree), trainData$Species)

#plot
plot(iris_tree)
plot(iris_tree, type="simple")

testPred <- predict(iris_tree, newdata = testData)
cfm <- table(testPred, testData$Species)
cfm
accuracy <- sum(diag(cfm))/sum(cfm)
print(paste('Accuracy: ', round(accuracy, 4)))


#Creating a randomForest (multiple decision trees running the same data sets for higher accuracy because it runs its own "cross validation")
iris_rf <- randomForest(Species ~ ., data=trainData, ntree=500, proximity = TRUE) #Assigns outcome variable as "Species" and uses all other data in the dataset in order to determine whether an iris falls into those species
iris_rf

#Determines which variables are the most important at determining the species of a particular iris is (greatest decrease in Gini <- meaning a better accuracy at choosing/predicting the correct species for the given iris)
importance(iris_rf)
varImpPlot(iris_rf) #Plots above line

irisPred <- predict(iris_rf, newdata = testData)
irisPred
cfm <- table(irisPred, testData$Species) #Creates a confusion matrix (allows us to see how accurately the decision tree(s) predicted the species per iris)
cfm

#####################################
#Different Data Set/Example
#####################################
titanic_data <-"https://goo.gl/At238b" %>%
  read.csv %>% select(survived, embarked, sex, age, sibsp, parch, fare) %>%
  mutate(embarked = factor(embarked),
         sex = factor(sex))

titanic_data <- slice(titanic_data, sample(1:n())) #Randomly samples the data so that it gets rid of any order that was already in the data

titanic_data <- titanic_data %>% mutate(id=row_number(), .before=survived)

titanic_data <- titanic_data[complete.cases(titanic_data), ] #if a row entry is missing data, that record is removed from the dataset

titanic_data <- titanic_data %>% mutate(survived=case_when(survived == 1 ~ "lived",
                                                           survived == 0 ~ "died")) #changes the data so that anytime a "1" was placed in the survived column for each record, it is changed to "lived" (and "0" becomes "died")

titanic_data$survived <- as.factor(titanic_data$survived)
summary(titanic_data)

rtree <- rpart(survived ~ ., titanic_data)
rtree
rpart.plot(rtree)

#Singular Decision Tree Above, Random Forest Below

titanic <-randomForest(survived ~ ., data=titanic_data, ntree = 500, proximity=TRUE) #Specifies that we will use 500 decision trees in this model
titanic
