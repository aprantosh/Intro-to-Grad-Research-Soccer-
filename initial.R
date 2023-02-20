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
Set_after_scaling <- data.frame(test$Gender, test$Squad,a)
                
View(Set_after_scaling)

install.packages("NbClust")
library("NbClust")

NbClust(a)


