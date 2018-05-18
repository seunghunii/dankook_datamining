pkgs <- c('dplyr','MASS','readxl',
          'class','FNN','caret')
sapply(pkgs,require,character.only = TRUE)

wd <- 'C:/Users/Seung Hun/Desktop'
setwd(wd)
boston <- read_xlsx('BostonHousing.xlsx',sheet = 2)

# 데이터 분할
set.seed(4)
train_idx <- sample(1:nrow(boston), 0.6 * nrow(boston))
test_idx <- setdiff(1:nrow(boston),train_idx)
train <- boston[train_idx,]
test <- boston[test_idx,]

# sensitivity & specificity
sens <- function(real,pre){
  sum(real == '1' & pre == '1') / sum(real == '1')
}

spec <- function(real,pre){
  sum(real == '0' & pre == '0') / sum(real == '0')
}

# a. 
a_train <- data.frame(scale(train[,1:13]))
a_test <- data.frame(scale(test[,1:13]))
a_train_y <- unlist(train[,14])
a_test_y <- unlist(test[,14])

# result & error with test data
k_test <- data.frame(matrix(nrow = 203,ncol = 5))

for(i in 1:5){
  k_test[,i] <- knn(a_train,a_test,a_train_y,k = i)
}

# error & accuracy
(test_err <- apply(k_test,2,function(x)mean(x != a_test_y)))
(test_acc <- apply(k_test,2,function(x)1 - mean(x != a_test_y)))

# error + accuracy = 1?
test_err + test_acc

# sensitiity & specificity
(test_sen <- apply(k_test,2,function(x)sens(a_test_y,x)))
(test_spe <- apply(k_test,2,function(x)spec(a_test_y,x)))

# result & error with train error
k_train <- data.frame(matrix(nrow = 303,ncol = 5))

for(i in 1:ncol(k_train)){
  k_train[,i] <- knn(a_train,a_train,a_train_y,k = i)
}

# error & accuracy
(train_err <- apply(k_train,2,function(x)mean(x != train[,14])))
(train_acc <- apply(k_train,2,function(x)1-mean(x != train[,14])))

# err + acc = 1?
train_err + train_acc

# sensitiity & specificity
(train_sen <- apply(k_train,2,function(x)sens(a_train_y,x)))
(train_spe <- apply(k_train,2,function(x)spec(a_train_y,x)))

# 최적의 k값?
# test accuracy는 k가 3일때와 5일때 가장 좋다.
# 둘 중 선택을 위해 민감도와 특이도를 살펴보면
# 민감도는 똑같지만 특이도에서 k = 3이 좋은 수치를 보였으므로
# 최적의 k는 3이다.

# b.
new <- data.frame(matrix(c(0.2,0,7,0,0.538,6,62,4.7,4,307,21,10),
                         ncol = 12,nrow = 1))
names(new) <- names(boston[,1:12])

b_train <- a_train[,1:12]
b_test_a <- rbind(a_test[,1:12])
b_test_b <- rbind(a_test[,1:12],new)
b_test_y <- unlist(test[,13])
b_train_y <- unlist(train[,13])

## knn regression
# model
reg_old <- knn.reg(b_train,b_test_a,b_train_y,k = 3)
rmse(reg_old$pred,b_test_y)

# knn regression predict
reg_new <- knn.reg(b_train,b_test_b,b_train_y,k = 3)
reg_new$pred[204]

## knn
# model
knn_old <- knn(b_train,b_test_a,b_train_y,k = 3)
rmse(as.numeric(knn_old),b_test_y)

# knn predict
knn_new <- knn(b_train,b_test_b,b_train_y,k = 3)

# what is difference

# c.학습세트의 오류
# a번의 오류율은
(train_err <- apply(k_train,2,function(x)mean(x != train[,14])))

# b번의 오류율? regression이라 오류율을 구할수는 없고
# rmse
sqrt(mean((b_test_y - reg$pred[1:203])^2))