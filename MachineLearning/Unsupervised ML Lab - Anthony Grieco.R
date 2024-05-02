#Author: Anthony Grieco
#Date: 4/18/2024
#Class: Foundations of Data Science and Analytics

library(tidyverse)
library(cluster)
library(factoextra)
library(dendextend)

#--------------------------------------------------
#1. Import the seeds data set into R
seeds <- read.csv("seeds_dataset.csv")

#--------------------------------------------------
#2. Determine how many complete cases are in the data set and remove incomplete cases
seeds <- na.omit(seeds)

#--------------------------------------------------
#3. It is usually necessary to standardize the data. Standardize the data set
seeds_std <- seeds %>% 
  mutate_at(vars(-("type.of.seed")), ~(scale(.) %>% as.vector))

#--------------------------------------------------
#4. Set a seed value for reproducibility
set.seed(1234)

#--------------------------------------------------
#5. Save the type.of.seed column to a vector and remove the type.of.seed column from the dataset
seedType <- seeds_std$type.of.seed
seeds_std <- seeds_std %>% select(-type.of.seed)
seedType

#--------------------------------------------------
#6. Determine the optimal value for the number of clusters in the data (assume you don’t already know the actual clusters are equivalent to 3). You may want to try several methods
fviz_nbclust(seeds_std, kmeans, method = "wss")
fviz_nbclust(seeds_std, kmeans, method = "silhouette")

gap_stat <- clusGap(seeds_std, FUN = kmeans, nstart=25, K.max=10, B=50)
fviz_gap_stat(gap_stat)

#--------------------------------------------------
#7. Create a kmeans model for clustering based on the optimal number of groups determined above
km.seeds <- kmeans(seeds_std, 3, nstart=25)
km.seeds

#--------------------------------------------------
#8. Generate an appropriate visualization of the groups
fviz_cluster(km.seeds, data=seeds_std)

#--------------------------------------------------
#9. Add the cluster number back into the original data set
seeds_std <- seeds_std %>% mutate(cluster = km.seeds$cluster)
seeds_std

#--------------------------------------------------
#10. Create a confusion matrix or a simple table to determine how well the algorithm performed
cfm <- table(seeds_std$cluster, seedType)
acc <- sum(diag(cfm) / sum(rowSums(cfm)) * 100)
paste("Accuarcy: ",round(acc, 2))
