setwd("C://Users/prantosh/Documents/Intro_to_Grad_Research/Intro_to_Graduate_Research")
library("readxl")
library(tidyverse)

library("readxl")

# Read xlsx files
soccer_collection = read_excel("C://Users/prantosh/Documents/Intro_to_Grad_Research/Intro_to_Graduate_Research/Final_data_research.xlsx")

View(soccer_collection)
test <- soccer_collection
#scale(select(soccer_collection, -Gender, -Squad))
a <- scale(select(test, -Gender,-Squad))

abc <-data.frame(a)
#View(abc)
Set_after_scaling <- data.frame(test$Gender, test$Squad,a)

View(Set_after_scaling)

install.packages("NbClust")
library("NbClust")

fviz_nbclust(abc,kmeans, method = "gap_stat")

res <- NbClust(abc, diss = NULL, distance = 'euclidean', min.nc = 2, max.nc = 20, 
               method = 'kmeans', index = 'mcclain')
res

res1 <- NbClust(data=abc, diss=NULL , distance = "euclidean", min.nc=2, max.nc = 15,method ='complete', 
                index= "all", alphaBeale = 0.1)
table (res$Best.partition)

table(soccer_collection$Gender)



