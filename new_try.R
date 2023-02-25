setwd("C://Users/prantosh/Documents/Intro_to_Grad_Research/Intro_to_Graduate_Research")
library("readxl")
library(tidyverse)

library(readxl)
Top5_league = read_excel("C://Users/prantosh/Documents/Intro_to_Grad_Research/Intro_to_Graduate_Research/Book1.xlsx")

View(Top5_league)

#######Scaling################

data.s <- scale(Top5_league[,-1:-2])
data.s
data.d <- dist(data.s)
data.d
#################factoextra#################

install.packages('factoextra')
library(ggplot2)
library(factoextra)

fviz_nbclust(data.s,kmeans, method = "wss") + labs(subtitle = "Elbow Method")
fviz_nbclust(data.s,kmeans, method = "silhouette")
fviz_nbclust(data.s,kmeans, method = "gap_stat")

####We will be using 4 clusters as seen in the Elbow Method##############


####kmeans#####
km.out <- kmeans(data.s, centers = 4, nstart = 100)
print(km.out)


####Visualizing the clustering algorithm results based on Squad
km.clusters <- km.out$cluster
rownames(data.s) <- Top5_league$Squad
rownames(data.s) <- paste(Top5_league$Squad, 1: dim(Top5_league)[1],sep = "_")
fviz_cluster(list(data = data.s, cluster= km.clusters))
##From the graph I think the blue color seems to be best club of all 5 top league as in the blue cluster 
## The clubs are seems like top from their league.



####Visualizing the clustering algorithm results based on Leauge
km.out <- kmeans(data.s, centers = 4, nstart = 100)
print(km.out)

km.clusters <- km.out$cluster
rownames(data.s) <- Top5_league$League
rownames(data.s) <- paste(Top5_league$League, 1: dim(Top5_league)[1],sep = "_")
fviz_cluster(list(data = data.s, cluster= km.clusters))

##From the graph I think the blue color seems to be best club of all 5 top league as in the blue cluster 
## The clubs are seems like top from their league.


#############NbClust Package###################

library(NbClust)
temp1 <- NbClust(data = tab, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, 
        
        method = "kmeans", index = "gamma", alphaBeale = 0.1)
temp1
####(we got 10 clusters using index = gamma)

temp1 <- NbClust(data = tab, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, 
                 
                 method = "kmeans", index = "ball", alphaBeale = 0.1)
temp1

###we gor 3 clusters usning index  = ball

temp1 <- NbClust(data = tab, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, 
                 
                 method = "kmeans", index = "hubert", alphaBeale = 0.1)
temp1
### we got 4 clusters using index = hubert

###We are getting different nummber of clusters from different algorithm. I assume 4 cluster seems good.
##when I tried 5 cluster 

############################################



