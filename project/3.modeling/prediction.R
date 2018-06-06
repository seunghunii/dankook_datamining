pkgs <- c('dplyr','progress')
sapply(pkgs,require,character.only = TRUE)

# data ready
movie <- read.csv('C:/Users/Seung Hun/Desktop/dataming_project/data/movie.csv',stringsAsFactors = FALSE)[,-1]
movie$top_sup[is.na(movie$top_sup)] <- 0
movie <- movie[!is.na(movie$people),] 
movie[,c(6:11)] <- lapply(movie[,c(6:11)],as.factor)

# function
rmse <- function(predict,real){
  sqrt(mean((real - predict)^2))
}

rsquare <- function(pred,real){
  1 - (sum((real-pred)^2)/sum((real-mean(real))^2))
}

# data partition
set.seed(4)
idx <- sample(1:nrow(movie),0.7 * nrow(movie))
train <- movie[idx,-2]
test <- movie[-idx,-2]

# formula
# 9,10이 중복되는 변수임
formu <- as.formula(paste0('people ~ ',paste0(names(movie)[c(3:9,11)],collapse = '+')))
formu

# 1. knn
#  - 1. knn regression
library(FNN)
movie2 <- movie[,-2]
movie2[,5:10] <- apply(movie2[,5:10],2,as.numeric)

for(i in 2:4){
  movie2[,i] <- as.numeric(as.character(movie2[,i]))
}
for(i in 2:ncol(movie2)){
  movie2[,i] <- scale(movie2[,i])
}

train2 <- movie2[idx,-1]
test2 <- movie2[-idx,-1]
y <- movie2[,1]

knn_test <- data.frame(matrix(nrow = 10,ncol = 4))
names(knn_test) <- c('train_rmse','train_acc',
                     'test_rmse','test_acc')
pb <- progress_bar$new(total = 15)

for(i in 1:15){
  pb$tick()
  mda <- knn.reg(train2,train2,y[idx],k = i)$pred
  mdb <- knn.reg(train2,test2,y[idx],k = i)$pred
  knn_test[i,1] <- rmse(y[idx],mda)
  knn_test[i,2] <- rsquare(y[idx],mda)
  knn_test[i,3] <- rmse(y[-idx],mdb)
  knn_test[i,4] <- rsquare(y[-idx],mdb)
}
knn_test

# best k = 4 in knn regression
md_knn_pre <- knn.reg(train2,test2,y[idx],k = 4)$pred
md_knn_rms <- rmse(md_knn_pre,y[-idx])

# 2. naive bayes는 분류문제만 가능함.

# 3. tree model

# 4. nueralnet

# 5. randomforest

# 
