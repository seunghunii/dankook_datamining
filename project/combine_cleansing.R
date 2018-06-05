pkgs <- c('dplyr','stringr','rjson','progress')
sapply(pkgs,require,character.only = TRUE)

setwd('C:/Users/Seung Hun/Desktop/dataming_project/data')
 
new <- read.csv('new.csv')
old <- read.csv('old.csv')

