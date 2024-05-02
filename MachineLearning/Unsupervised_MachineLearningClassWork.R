library(tidyverse)
#install.packages("cluster")
library(cluster)
#install.packages("factoextra")
library(factoextra)
#install.packages("dendextend")
library(dendextend)

#Unsupervied ML

#KMeans Algorithm - walks through entire dataset and randomly assigns data to a cluster, sorting then happens from there based on trends/patterns in the data and a Sum of Squares calculation determining how distant things are from the central point of the data
df <- iris

df %>% 
  ggplot(aes(Petal.Length, Petal.Width)) + 
  geom_point(aes(col=Species), size = 3)

df %>% 
  ggplot(aes(Sepal.Length, Sepal.Width)) + 
  geom_point(aes(col=Species), size = 3)

n_df <- df %>% mutate_at(c("Sepal.Length", "Sepal.Width", 
                           "Petal.Length"), 
                         ~(scale(.) %>% as.vector))

irisCluster <- kmeans(n_df[,1:4], center=3, nstart = 25) #Tells the kmeans model to build clusters of the data with the first 4 columns of the n_df dataset only | Tells kmeans how many clusters to make | Tells kmeans how many models to run
irisCluster

table(irisCluster$cluster, n_df$Species) #Creates confusion matrix of the above kmeans model

#Determining the optimal number of clusters for kmeans to create (3 Options for Same basic results)
fviz_nbclust(n_df[, 1:4], kmeans, method = "wss") # 2 Clusters would be appropriate for this data set assuming that we didn't already know that there are 3 species in the data, but 3 clusters would also be possible
fviz_nbclust(n_df[, 1:4], kmeans, method = "silhouette") #Another example

gap_stat <- clusGap(n_df[,1:4], FUN = kmeans, nstart=25, K.max=10, B=50)
fviz_gap_stat(gap_stat)

#--------
#Determining which states were the most similar to one another (with respect to crime) in 1973
data("USArrests")
help("USArrests")
df2 <- na.omit(USArrests) #Removes all rows with missing data

scaled_df <- scale(df2)

#Determining Number of Clusters
fviz_nbclust(scaled_df, kmeans, method = "wss") # 2 Clusters would be appropriate for this data set assuming that we didn't already know that there are 3 species in the data, but 3 clusters would also be possible
fviz_nbclust(scaled_df, kmeans, method = "silhouette") #Another example
gap_stat <- clusGap(scaled_df[,1:4], FUN = kmeans, nstart=25, K.max=10, B=50)
fviz_gap_stat(gap_stat)

#Determining/Displaying Actual Clusters
km.res <- kmeans(scaled_df, 4, nstart=25)
km.res
fviz_cluster(km.res, data=scaled_df)

df2 <- df2 %>% mutate(cluster=km.res$cluster)

############################################################
#Hierarchical Clustering Example


seeds_df <- read.csv("seeds_dataset.csv")
summary(seeds_df)
glimpse(seeds_df)
seeds_df <- na.omit(seeds_df) #Removes records with missing data

seeds_label <- seeds_df$type.of.seed #Copies all values in type.of.seed column and transfers them to sees_label
seeds_label

seeds_df <- seeds_df %>% select(-type.of.seed) #returns all columns except for "type.of.seed" (would have been the "outcome variable" in Supervied ML)

#Data Normalized because we done know the units used for each individual column
seeds_df_sc <- scale(seeds_df)

#Creates Distance Matrix
dist_mat <- dist(seeds_df_sc, method = "euclidean")
dist_mat

#Creates model that determines average distance between data points
hclust_avg <- hclust(dist_mat, method = "average")
plot(hclust_avg, hang=-1, cex = 0.7) #"cex" = size of font used in model

#Creates approximate groups based on how similar the data is (NOT A PREDICTION MODEL AS WOULD BE THE CASE IN SUPERVISED ML)
cut_avg <- cutree(hclust_avg, k=3)
table(cut_avg)

rect.hclust(hclust_avg, k=3, border=2:6) #Creates a visual representation of where each identified cluser is in the model
abline(h=3, color="red") #Shows dilineation of where clusters form in the data

#Colors branches in model to show specificially which branches are a part of which clusters 
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 3)
plot(avg_col_dend)

seeds_df <- seeds_df %>% mutate(cluster = cut_avg)
seeds_df$cluster <- as.factor(seeds_df$cluster)
summary(seeds_df)
count(seeds_df, cluster)

seeds_df %>%
  ggplot(aes(x=area, y=perimeter, color = cluster)) + 
  geom_point()

seeds_df %>%
  ggplot(aes(x=length.of.kernel, y=width.of.kernal, color = cluster)) + 
  geom_point()

#Creates Confusion Matrix
cfm <- table(seeds_df$cluster, seeds_label)
acc <- sum(diag(cfm) / sum(rowSums(cfm)) * 100)
paste("Accuarcy: ",round(acc, 2))
