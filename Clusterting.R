setwd("C://Users/prantosh/Documents/Intro_to_Grad_Research/Intro_to_Graduate_Research")
install.packages('factoextra')
library("readxl")
library(tidyverse)
library(tidyr)
library(gridExtra)
library(ggplot2)
library(purrr)
library(factoextra)
library(FactoMineR)

####
## This is the new dataset combination of Top 5 league with MLS ##
#This dataset contains (France,Italy, Germany, England, Spain and US squads)#
###

# Read xlsx files
soccer_collection = read_excel("C://Users/prantosh/Documents/Intro_to_Grad_Research/Intro_to_Graduate_Research/all_datasets.xlsx")
#View(soccer_collection)
test <- soccer_collection

summary(test)



#######Graph and plot visualization of the dataset#################

test[1:3]
str(test)
names(test)


ggplot(test, aes(League,  pass_completed))+ 
  geom_point(size = 3)+
  geom_line(colour = "red")


ggplot(test, aes(League,  pass_completed))+ 
  geom_point(size = 3 , alpha =0.5)+
  geom_smooth(method= lm,se =F) +
  #facet_wrap(~League)+
  labs(title= "Completed Pass among different Countries Club")+
  theme_bw()

 ####################################################################

glimpse(test)
test$TotDist

str(soccer_collection)
summary(soccer_collection)

##Scaling for the quantitative datas##
#scale(select(soccer_collection, -Gender, -Squad))
game.s <- scale(select(test, -Club,-League))
#View(game.s)
game.s <- data.frame(game.s)
game.d <- dist(game.s)

Set_after_scaling <- data.frame(test$Club, test$League,game.s)
#View(Set_after_scaling)

#########Using of the fviz_nbclust to find out the best number of cluster



################****************************################
##############kmeans clustering##### 

##Selecting k =3 
km.out1 <- kmeans(game.s, centers = 3, nstart = 100)
print(km.out1)

o=order(km.out1$cluster)
loopp <-data.frame(test$League[o],km.out1$cluster[o])

loopp

table(data.frame(test$League[o],km.out1$cluster[o]))


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

##From the graph I think the blue color seems to be best club of all 5 top league as in the blue cluster 
## The clubs are seems like top from their league.

###%%%% for k =3 %%%%%######
km.out1$size
table(soccer_collection$League, km.out1$cluster)
table(soccer_collection$Club, km.out1$cluster)


###################################new try##################




ggplot(data = mnop, aes(x =test.League.o. ,y= Freq,fill = km.out1.cluster.o.  ))+geom_bar(stat = "identity") + 
  labs(x = "\n Country", y = "Total Numbers in Respective Cluster \n", 
       title = "Visualization of partition of League in respective to Cluster\n",
       fill = "km.out1.cluster.o.") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12),
        legend.position = "bottom")


#############################
##Selecting k =3 
km.out1 <- kmeans(game.s, centers = 3, nstart = 100)
print(km.out1)

o=order(km.out1$cluster)
o
loopp <-data.frame(test$League[o],km.out1$cluster[o])

loopp

lops <- data.frame(test$League, km.out1$cluster)
lops



ggplot(test, aes(League,  pass_completed))+ 
  geom_point(size = 3)+
  geom_line(colour = "red")


ggplot(test, aes(League,  pass_completed))+ 
  geom_point(size = 3 , alpha =0.5)+
  geom_smooth(method= lm,se =F) +
  facet_wrap(~Club)+
  labs(title= "Completed Pass among different Countries Club")+
  theme_bw()


ggplot(test, aes(League,  Interceptions))+ 
  geom_point(size = 3 , alpha =0.5)+
  geom_line(colour = "red")+
  geom_smooth(method= lm,se =F) +
  #facet_wrap(~League)+
  labs(title= "Interceptions made  among different Countries Club")+
  theme_bw()

ggplot(test, aes(League,total_Shots))+ 
  geom_point(size = 3 , alpha =0.5)+
  geom_line(colour = "red")+
  geom_smooth(method= lm,se =F) +
  #facet_wrap(~League)+
  labs(title= "Total shots made in a Game of different Countries Club")+
  theme_bw()


soccer_collection %>%
  ggplot(aes(x = pass_completed)) +
  #geom_boxplot() +
  geom_point(stat = "count") +
  geom_line(stat = "count") +
  facet_wrap(vars(League)) +
  theme_bw()  


lops
table(lops)
all <- lops
colnames(all)[1]  <- "League" 
colnames(all)[2]  <- "Cluster" 
lops
all



soccer_collection$Cluster <- all$Cluster

#soccer_collection <- cbind.data.frame(soccer_collection,CLuster)
soccer_collection$Cluster

#all <- cbind.data.frame(soccer_collection, loopp$Cluster)

a<- all$League 
b <- soccer_collection$Errors
Clusters<-all$Cluster
new_tab <- cbind.data.frame(a,b,Clusters)
new_tab

new_tab %>%
  ggplot(aes(x = soccer_collection$Club, y = b,fill =factor(Clusters),color= factor(Clusters), shape = factor(Clusters)
)) +
  geom_point(size = 3 , alpha =4)+
  geom_smooth(method= lm,se =F) +
  facet_wrap(vars(a)) +
  
  labs(title= "Mistakes made among different Countries Club")+
  theme_bw()


soccer_collection %>%
  ggplot(aes(x = Club, y = total_Shots)) +
  geom_point() +
  facet_wrap(vars(League)) +
  theme_bw()


#color = hp, shape = factor(cyl)



new_table <- cbind.data.frame(soccer_collection$League, soccer_collection$total_Shots, loopp$Cluster)
new_table
table(new_table)

game.s
km <- kmeans(game.s, centers = 3, nstart = 100)
print(km)

table(km$cluster)

o=order(km.out1$cluster)
loopp <-data.frame(test$League[o],km.out1$cluster[o])

loopp


