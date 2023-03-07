setwd("C://Users/prantosh/Documents/Intro_to_Grad_Research/Intro_to_Graduate_Research")
library("readxl")
library(tidyverse)

library("readxl")

####
## This is the new dataset combination of Top 5 league with MLS ##
#This dataset contains (France,Italy, Germany, England, Spain and US squads)#
###

# Read xlsx files
soccer_collection = read_excel("C://Users/prantosh/Documents/Intro_to_Grad_Research/Intro_to_Graduate_Research/all_datasets.xlsx")
View(soccer_collection)
test <- soccer_collection

##Scaling for the quantitative datas##
#scale(select(soccer_collection, -Gender, -Squad))
game.s <- scale(select(test, -Club,-League))
View(game.s)

game.d <- dist(game.s)

Set_after_scaling <- data.frame(test$Club, test$League,game.s)
#View(Set_after_scaling)

######### Using of NbClust package to find the best number of clusters#######

install.packages("NbClust")
library("NbClust")

##Using index == "ball" , we get 3 clusters.####

res <- NbClust(game.s, diss = NULL, distance = 'euclidean', min.nc = 2, max.nc = 20, 
               method = 'kmeans', index = 'ball', alphaBeale = 0.1)
res


##Using index == "frey" , we get 2 clusters.####

res1 <- NbClust(data=game.s, diss=NULL , distance = "euclidean", min.nc=2, max.nc = 20,method ="kmeans", 
                index= "frey", alphaBeale = 0.1)
res1

##Using index == "gamma" , we get 20 clusters.####

res2 <- NbClust(data=game.s, diss=NULL , distance = "euclidean", min.nc=2, max.nc = 20,method ="kmeans", 
                index= "gamma", alphaBeale = 0.1)
res2

##Using index == "dunn" , we get 11 clusters.####

res3 <- NbClust(data=game.s, diss=NULL , distance = "euclidean", min.nc=2, max.nc = 15,method ="kmeans", 
                index= "dunn", alphaBeale = 0.1)
res3

##Using index == "kl" , we get 6 clusters.####

res4 <- NbClust(data=game.s, diss=NULL , distance = "euclidean", min.nc=2, max.nc = 15,method ="kmeans", 
                index= "kl", alphaBeale = 0.1)
res4

##Using index == "ratkowsky" , we get 3 clusters.####

res5 <- NbClust(data=game.s, diss=NULL , distance = "euclidean", min.nc=2, max.nc = 15,method ="kmeans", 
                index= "ratkowsky", alphaBeale = 0.1)
res5

##Using index == "cindex" , we get 3 clusters.####

res6 <- NbClust(data=game.s, diss=NULL , distance = "euclidean", min.nc=2, max.nc = 15,method ="kmeans", 
                index= "cindex", alphaBeale = 0.1)
res6


##Using index == "hubert" , we get 5 clusters.####

res6 <- NbClust(data=game.s, diss=NULL , distance = "euclidean", min.nc=2, max.nc = 15,method ="kmeans", 
                index= "hubert", alphaBeale = 0.1)
res6




##########################################

install.packages('factoextra')
library(ggplot2)
library(factoextra)

fviz_nbclust(game.s,kmeans, method = "wss") + labs(subtitle = "Elbow Method")





####kmeans clustering##### 

##Selecting k =3 
km.out <- kmeans(game.s, centers = 3, nstart = 100)
print(km.out)


####Visualizing the clustering algorithm results based on CLub
km.clusters <- km.out$cluster
rownames(game.s) <- soccer_collection$Club
rownames(game.s) <- paste(soccer_collection$Club, 1: dim(soccer_collection)[1],sep = "_")
fviz_cluster(list(data = game.s, cluster= km.clusters))



####Visualizing the clustering algorithm results based on League
km.clusters <- km.out$cluster
rownames(game.s) <- soccer_collection$League
rownames(game.s) <- paste(soccer_collection$League, 1: dim(soccer_collection)[1],sep = "_")
fviz_cluster(list(data = game.s, cluster= km.clusters))


####K means Clustering

##Selecting k =4
####k means#####
km.out <- kmeans(game.s, centers = 4, nstart = 100)
print(km.out)


####Visualizing the clustering algorithm results based on CLub
km.clusters <- km.out$cluster
rownames(game.s) <- soccer_collection$Club
rownames(game.s) <- paste(soccer_collection$Club, 1: dim(soccer_collection)[1],sep = "_")
fviz_cluster(list(data = game.s, cluster= km.clusters))



####Visualizing the clustering algorithm results based on League
km.clusters <- km.out$cluster
rownames(game.s) <- soccer_collection$League
rownames(game.s) <- paste(soccer_collection$League, 1: dim(soccer_collection)[1],sep = "_")
fviz_cluster(list(data = game.s, cluster= km.clusters))



####Using k == 4 seems diverse in the data as we can see more clearly how the cluster plot scatters###


##From the graph I think the blue color seems to be best club of all 5 top league as in the blue cluster 
## The clubs are seems like top from their league.

