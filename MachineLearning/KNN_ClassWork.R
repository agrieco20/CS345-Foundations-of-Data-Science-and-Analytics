library(tidyverse)
library(class)
library(caret)
#install.packages("gmodels")
library(gmodels)

#turns off scientific notation
options(scipen=999)

data(iris)
summary(iris)

iris %>%
  ggplot(aes(x=Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point()

iris %>%
  ggplot(aes(x = Petal.Length, y = Petal.Width, col = Species)) +
  geom_point()

iris <- iris %>% mutate(id = row_number(), .before=Sepal.Length)
summary(iris)

set.seed(3544)
iris_train <- iris %>% sample_frac(.70)
iris_test <- anti_join(iris, iris_train, by = 'id')
target_cat <- pull(iris_train,"Species")
target_cat
test_cat <- pull(iris_test, "Species")
test_cat

# rule of thumb k = Sqrt(number of obs)
sqrt(nrow(iris_train))

# we can also try some runs
error<-c()
for (i in 1:20){
  knn.fit <- knn(train = iris_train[,2:4], test = iris_test[,2:4], cl = as.factor(target_cat), k = i)
  error[i] = 1- mean(knn.fit == iris_test$Species)
  print(paste("Run:", i, ": Error: ", round(error[i],4)))
}
print(min(error))

ggplot(data = data.frame(error), aes(x = 1:20, y = error)) +
  geom_line(color = "Blue")

#KNN trains and then runs itself on the test data.
knn.fit <- knn(train = iris_train[,2:4], 
               test = iris_test[,2:4], 
               cl = as.factor(target_cat), 
               k = 10)

cm <- confusionMatrix(data=knn.fit, reference = as.factor(test_cat))
cm

#Running multiple times may produce different results because
#ties are broken at random...

#wCrossTable(x = test_cat, y = knn.fit)

