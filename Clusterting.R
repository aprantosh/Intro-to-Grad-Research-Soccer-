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

str(soccer_collection)
summary(soccer_collection)

##Scaling for the quantitative datas##
#scale(select(soccer_collection, -Gender, -Squad))
game.s <- scale(select(test, -Club,-League))
View(game.s)
game.s <- data.frame(game.s)
game.d <- dist(game.s)

Set_after_scaling <- data.frame(test$Club, test$League,game.s)
#View(Set_after_scaling)

#########Using of the fviz_nbclust to find out the best number of cluster
install.packages('factoextra')
library(ggplot2)
library(factoextra)

fviz_nbclust(game.s,kmeans, method = "wss") + labs(subtitle = "Elbow Method")
fviz_nbclust(game.s,kmeans, method = "silhouette")
fviz_nbclust(game.s,kmeans, method = "gap_stat")

NbClust(data = game.s, diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 993, method = "kmeans")

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




################****************************################
##############kmeans clustering##### 

##Selecting k =3 
km.out1 <- kmeans(game.s, centers = 3, nstart = 100)
print(km.out1)


####Visualizing the clustering algorithm results based on CLub
km1.clusters <- km.out1$cluster
km1.clusters
rownames(game.s) <- soccer_collection$Club
rownames(game.s) <- paste(soccer_collection$Club, 1: dim(soccer_collection)[1],sep = "_")
fviz_cluster(list(data = game.s, cluster= km1.clusters))





####Visualizing the clustering algorithm results based on League
km1.clusters <- km.out1$cluster
km1.clusters
rownames(game.s) <- soccer_collection$League
rownames(game.s) <- paste(soccer_collection$League, 1: dim(soccer_collection)[1],sep = "_")
fviz_cluster(list(data = game.s, cluster= km1.clusters))

############################################
#########***********************############
####K means Clustering

##Selecting k =4
####k means#####

km.out2 <- kmeans(game.s, centers = 4, nstart = 100)
print(km.out2)


####Visualizing the clustering algorithm results based on CLub
km2.clusters <- km.out2$cluster
rownames(game.s) <- soccer_collection$Club
rownames(game.s) <- paste(soccer_collection$Club, 1: dim(soccer_collection)[1],sep = "_")
fviz_cluster(list(data = game.s, cluster= km2.clusters))



####Visualizing the clustering algorithm results based on League
km2.clusters <- km.out2$cluster
rownames(game.s) <- soccer_collection$League
rownames(game.s) <- paste(soccer_collection$League, 1: dim(soccer_collection)[1],sep = "_")
fviz_cluster(list(data = game.s, cluster= km2.clusters))



####Using k == 4 seems diverse in the data as we can see more clearly how the cluster plot scatters###


##From the graph I think the blue color seems to be best club of all 5 top league as in the blue cluster 
## The clubs are seems like top from their league.

###%%%% for k =3 %%%%%######
km.out1$size
table(soccer_collection$League, km.out1$cluster)
table(soccer_collection$Club, km.out1$cluster)

#####%%%%% for k =4 %%%%%%%#####
km.out2$size
table(soccer_collection$League, km.out2$cluster)
table(soccer_collection$Club, km.out2$cluster)

plot(soccer_collection[c("Gls", "Sh")], col = km.out1$cluster)

plot(soccer_collection[c("Gls", "Sh")], col = soccer_collection$League)






##@@@@@@@@@@@@ Finding number of clusters in unique way @@@@@@@@@@@@@@@@@@@@@#

lista.methods = c("kl", "ch", "hartigan","mcclain", "gamma", "gplus",
                  "tau", "dunn", "sdindex", "sdbw", "cindex", "silhouette",
                  "ball","ptbiserial", "gap","frey")
lista.distance = c("metodo","euclidean", "maximum", "manhattan", "canberra")

tabla = as.data.frame(matrix(ncol = length(lista.distance), nrow = length(lista.methods)))
names(tabla) = lista.distance

for (j in 2:length(lista.distance)){
  for(i in 1:length(lista.methods)){
    
    nb = NbClust(game.s, distance = lista.distance[j],
                 min.nc = 2, max.nc = 8, 
                 method = "kmeans", index =lista.methods[i])
    tabla[i,j] = nb$Best.nc[1]
    tabla[i,1] = lista.methods[i]
    
  }}

tabla
View(tabla)


