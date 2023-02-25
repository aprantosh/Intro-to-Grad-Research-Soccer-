setwd("C://Users/prantosh/Documents/Intro_to_Grad_Research/Intro_to_Graduate_Research")
library("readxl")
library(tidyverse)

library("readxl")

# Read xlsx files
soccer_collection = read_excel("C://Users/prantosh/Documents/Intro_to_Grad_Research/Intro_to_Graduate_Research/Final_data_research.xlsx")

View(soccer_collection)
test <- soccer_collection
#scale(select(soccer_collection, -Gender, -Squad))
game.s <- scale(select(test, -Gender,-Squad))

game.d <- dist(game.s)
#View(abc)
Set_after_scaling <- data.frame(test$Gender, test$Squad,a)

##View(Set_after_scaling)

install.packages("NbClust")
library("NbClust")

##fviz_nbclust(abc,kmeans, method = "gap_stat")

res <- NbClust(game.s, diss = NULL, distance = 'euclidean', min.nc = 2, max.nc = 20, 
               method = 'kmeans', index = 'ball')
res


###we get 3 cluster using index  = ball

res1 <- NbClust(data=game.s, diss=NULL , distance = "euclidean", min.nc=2, max.nc = 15,method ='complete', 
                index= "frey", alphaBeale = 0.1)
res1

####we get 3 cluster using index = frey
table (res$Best.partition)

table(soccer_collection$Gender)

##########################################

install.packages('factoextra')
library(ggplot2)
library(factoextra)

fviz_nbclust(game.s,kmeans, method = "wss") + labs(subtitle = "Elbow Method")



####kmeans#####
km.out <- kmeans(game.s, centers = 3, nstart = 100)
print(km.out)


####Visualizing the clustering algorithm results based on Squad
km.clusters <- km.out$cluster
rownames(game.s) <- soccer_collection$Squad
rownames(game.s) <- paste(soccer_collection$Squad, 1: dim(soccer_collection)[1],sep = "_")
fviz_cluster(list(data = game.s, cluster= km.clusters))



####Visualizing the clustering algorithm results based on Gender
km.clusters <- km.out$cluster
rownames(game.s) <- soccer_collection$Gender
rownames(game.s) <- paste(soccer_collection$Gender, 1: dim(soccer_collection)[1],sep = "_")
fviz_cluster(list(data = game.s, cluster= km.clusters))

