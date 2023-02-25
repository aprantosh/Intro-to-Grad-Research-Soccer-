setwd("C://Users/prantosh/Documents/Intro_to_Grad_Research/Intro_to_Graduate_Research")
library("readxl")
library(tidyverse)

library(readxl)
Top5_league = read_excel("C://Users/prantosh/Documents/Intro_to_Grad_Research/Intro_to_Graduate_Research/Book1.xlsx")

View(Top5_league)

#######Scaling################

data.s <- scale(Top5_league[,-1:-2])
data.s
d <- dist(data.s)
options(max.print = 1000000000)
d
tab <- data.frame(data.s)
tab
###################### K means ################
k2 <- kmeans(data.s,2)
k2
k2$cluster
#################factoextra#################

install.packages('factoextra')
library(ggplot2)
library(factoextra)

fviz_nbclust(data.s,kmeans, method = "wss")
fviz_nbclust(data.s,kmeans, method = "silhouette")
fviz_nbclust(data.s,kmeans, method = "gap_stat")




#############NbClust Package###################

library(NbClust)
temp1 <- NbClust(data = tab, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, 
        
        method = "kmeans", index = "all", alphaBeale = 0.1)
temp1
table (temp$Best.partition)

table(Top5_league$Squad)
table(Top5_league$League)
table()



############################################

library(tidyverse)

test1 <- Top5_league
a <- scale(select(test1, -Squad, -League))

abc <-data.frame(a)
View(abc)
Set_after_scaling <- data.frame(test$Gender, test$Squad,a)

View(Set_after_scaling)

install.packages("NbClust")
library("NbClust")


res <- NbClust(abc, diss = NULL, distance = 'euclidean', min.nc = 2, max.nc = 20, 
               method = 'kmeans', index = 'all')
res
table (res$Best.partition)

table(soccer_collection$Gender)



###################
library(gmodels)
library(NbClust)
library(tidyverse)
library(readxl)
library(knitr)

# Find best number of clusterw
set.seed(1990)
# list of potential indices (note: I removed some which took longer to run)
indices <- c("kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "dunn", "sdindex", "sdbw")
# initialize var to collect clustering results
results <- list()
# loop over indices with try function (which continues running even with errors)
for (i in 1:length(indices)) {
  print(paste0("Trying ", indices[i], " index..."))
  results[[i]] <- try(NbClust(data=abc,min.nc=2,max.nc=15, index=indices[i], method="kmeans")) 
}
num_clust <- list()
for (i in 1:length(results)){
  num_clust[[i]] <- try(as.numeric(results[[i]]$Best.nc[1]))
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

paste0("Based on a number of criteria, we will select ", getmode(num_clust), " clusters.")

