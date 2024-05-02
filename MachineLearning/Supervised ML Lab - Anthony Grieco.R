#Author: Anthony Grieco
#Date: 4/11/2024
#Class: Foundations of Data Science and Analytics

library(tidyverse)
library(ROCR)
library(rpart)
library(rpart.plot)
library(partykit)
library(randomForest)

#--------------------------------------------------
#1. Import the wine data set into R
wine <- read_csv("WineQuality.csv")
wine <- slice(wine, sample(1:n())) #Shuffles the data
colnames(wine) <- c('type', 'fixedAcidity', 'volatileAcidity', 'citricAcid', 'residualSugar', 'chlorides', 'freeSulfurDioxide', 'totalSulfurDioxide', 'density', 'pH', 'sulphates', 'alcohol', 'quality')
wine <- wine %>% mutate(type = factor(type))
wine$type <- as.factor(wine$type)

summary(wine)
glimpse(wine)

#--------------------------------------------------
#2. Determine how many complete cases are in the data set. How many red wines? How many white wines?

wine <- wine[complete.cases(wine), ] #Rows containing empty data are removed from the dataset
red <- count(filter(wine, type == "red"))
white <- count(filter(wine, type == "white"))
print(paste('There are', red, 'red wines in the complete data set.'))
print(paste('There are', white, 'white wines in the complete data set.'))

#--------------------------------------------------
#3. What is the distribution of quality scores across the wines?
qualityDist <- ggplot(data = wine, aes(x = quality)) +
  geom_histogram() +
  labs(x= "Wine Quality Rating")
qualityDist

#--------------------------------------------------
#4. Is the data set skewed in terms of quality ratings? Convert the quality rating into a categorical variable: “low” <= 5, “high” > 5.  How many high vs low quality wines are there?
wine <- wine %>% mutate(quality=case_when(quality <= 5 ~ "Low",
                                       quality > 5 ~ "High")) %>%
  mutate(quality = factor(quality))
wine$quality <- as.factor(wine$quality)

high <- count(filter(wine, quality == "High"))
low <- count(filter(wine, quality == "Low"))
print(paste('There are a total of', high, 'high quality wines in the complete data set.'))
print(paste('There are a total of', low, 'low quality wines in the complete data set.'))

#--------------------------------------------------
#5. Split the dataset into test and train sets with a 30, 70 split respectively.
set.seed(1234)
trainData <- wine %>% sample_frac(.70)
testData <- anti_join(wine, trainData)
trainData
testData

#--------------------------------------------------
#6. It may be necessary to standardize the data. It may be prudent to try your model with both types of data – raw and standardized.
wine_norm <- wine %>% mutate_at(c('fixedAcidity', 'volatileAcidity', 'citricAcid', 'residualSugar', 'chlorides', 'freeSulfurDioxide', 'totalSulfurDioxide', 'density', 'pH', 'sulphates', 'alcohol'), ~scale(.) %>% as.vector)
  #mutate_at(fixedAcidity + volatileAcidity + citricAcid + residualSugar + chlorides + freeSulfurDioxide + density + pH + sulphates + alcohol) ~scale() vector()
wine_norm

trainData_norm <- wine_norm %>% sample_frac(.70)
testData_norm <- anti_join(wine_norm, trainData_norm)
trainData_norm
testData_norm
#--------------------------------------------------
#7. Create a classification model for wine based only the physicochemical features (not type). Develop a model that accurately predicts whether a wine is high or low quality.

#Decision Tree (Raw Data)
trainData <- trainData %>% select(-type)
testData <- testData %>% select(-type)

physiochem <- quality ~ alcohol + volatileAcidity + density
wine_tree <- ctree(physiochem, data=trainData)
plot(wine_tree, type='simple')
wine_tree

#Decision Tree (Normalized Data)
trainData_norm <- trainData_norm %>% select(-type)
testData_norm <- testData_norm %>% select(-type)

wine_tree_norm <- ctree(physiochem, data=trainData_norm)
plot(wine_tree_norm, type='simple')
wine_tree_norm

#--------------------------------------------------
#8. Demonstrate the accuracy of your model. You may try several models, Decision Trees, Random Forests, Logistic Regression and/or KNN.

#Testing Decision Tree Accuracy (Raw Data)
table(predict(wine_tree), trainData$quality)

testPred <- predict(wine_tree, newdata = testData)
cfm_dt <- table(testPred, testData$quality)
cfm_dt
accuracy <- sum(diag(cfm_dt))/sum(cfm_dt)
print(paste('Raw Data Decision Tree Accuracy: ', round(accuracy, 4)))

#RandomForest (Raw Data)
wine_rf <- randomForest(physiochem, data=trainData, ntree=500, proximity = TRUE)
wine_rf

winePred <- predict(wine_rf, newdata = testData)
winePred
cfm_rf <- table(winePred, testData$quality)
cfm_rf
accuracy <- sum(diag(cfm_rf))/sum(cfm_rf)
print(paste('Raw Data Random Forest Accuracy: ', round(accuracy, 4)))

#Testing Decision Tree Accuracy (Normalized Data)
table(predict(wine_tree_norm), trainData_norm$quality)

testPred_norm <- predict(wine_tree_norm, newdata = testData_norm)
cfm_dt_norm <- table(testPred_norm, testData_norm$quality)
cfm_dt_norm
accuracy <- sum(diag(cfm_dt_norm))/sum(cfm_dt_norm)
print(paste('Normalized Data Decision Tree Accuracy: ', round(accuracy, 4)))

#RandomForest (Normalized Data)
wine_rf_norm <- randomForest(physiochem, data=trainData_norm, ntree=500, proximity = TRUE)
wine_rf_norm

winePred_norm <- predict(wine_rf_norm, newdata = testData_norm)
winePred_norm
cfm_rf_norm <- table(winePred_norm, testData_norm$quality)
cfm_rf_norm
accuracy <- sum(diag(cfm_rf_norm))/sum(cfm_rf_norm)
print(paste('Normalized Data Random Forest Accuracy: ', round(accuracy, 4)))

#--------------------------------------------------
#9. Are there differences in red vs white wines in terms of classification efficacy? You may need to subset the data into red and white wines.

#Random Forest for Red Wines (Raw Data)
trainData_R <- wine %>% 
  filter(type == "red") %>% sample_frac(.70)
testData_R <- anti_join(wine, trainData_R)
trainData_R
testData_R

r_wine_rf <- randomForest(physiochem, data=trainData, ntree=500, proximity = TRUE)
r_wine_rf

r_winePred <- predict(r_wine_rf, newdata = testData_R)
r_winePred
r_cfm_rf <- table(r_winePred, testData_R$quality)
r_cfm_rf
accuracy <- sum(diag(r_cfm_rf))/sum(r_cfm_rf)
print(paste('Raw Data - Red Wine Random Forest Accuracy: ', round(accuracy, 4)))

#Random Forest for White Wines (Raw Data)
trainData_W <- wine %>% 
  filter(type == "white") %>% sample_frac(.70)
testData_W <- anti_join(wine, trainData_W)
trainData_W
testData_W

w_wine_rf <- randomForest(physiochem, data=trainData, ntree=500, proximity = TRUE)
w_wine_rf

w_winePred <- predict(w_wine_rf, newdata = testData_W)
w_winePred
w_cfm_rf <- table(w_winePred, testData_W$quality)
w_cfm_rf
accuracy <- sum(diag(w_cfm_rf))/sum(w_cfm_rf)
print(paste('Raw Data - White Wine Random Forest Accuracy: ', round(accuracy, 4)))

#Random Forest for Red Wines (Normalized Data)
trainData_R_norm <- wine_norm %>% 
  filter(type == "red") %>% sample_frac(.70)
testData_R_norm <- anti_join(wine_norm, trainData_R_norm)
trainData_R_norm
testData_R_norm

r_wine_rf_norm <- randomForest(physiochem, data=trainData_norm, ntree=500, proximity = TRUE)
r_wine_rf_norm

r_winePred_norm <- predict(r_wine_rf_norm, newdata = testData_R_norm)
r_winePred_norm
r_cfm_rf_norm <- table(r_winePred_norm, testData_R_norm$quality)
r_cfm_rf_norm
accuracy <- sum(diag(r_cfm_rf_norm))/sum(r_cfm_rf_norm)
print(paste('Normalized Data - Red Wine Random Forest Accuracy: ', round(accuracy, 4)))

#Random Forest for White Wines (Normalized Data)
trainData_W_norm <- wine_norm %>% 
  filter(type == "white") %>% sample_frac(.70)
testData_W_norm <- anti_join(wine_norm, trainData_W_norm)
trainData_W_norm
testData_W_norm

w_wine_rf_norm <- randomForest(physiochem, data=trainData_norm, ntree=500, proximity = TRUE)
w_wine_rf_norm

w_winePred_norm <- predict(w_wine_rf_norm, newdata = testData_W_norm)
w_winePred_norm
w_cfm_rf_norm <- table(w_winePred_norm, testData_W_norm$quality)
w_cfm_rf_norm
accuracy <- sum(diag(w_cfm_rf_norm))/sum(w_cfm_rf_norm)
print(paste('Normalized Data - White Wine Random Forest Accuracy: ', round(accuracy, 4)))
