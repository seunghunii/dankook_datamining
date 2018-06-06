pkgs <- c('dplyr','stringr','progress')
sapply(pkgs,require,character.only = TRUE)

setwd('C:/Users/Seung Hun/Desktop/dataming_project/data')
 
new <- read.csv('new.csv')
old <- read.csv('old.csv')

names(new)
names(old)
length(unique(new$영화명))
