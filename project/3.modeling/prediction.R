pkgs <- c('dplyr','progress','rlist',
          'pbapply','data.table')
sapply(pkgs,require,character.only = TRUE)

# 1. data ready ####
movie <- read.csv('D:/바탕/dataming_project/data/movie.csv',stringsAsFactors = FALSE)[,-1]
movie[,c(6,7,9:11)] <- lapply(movie[,c(6,7,9:11)],as.factor)

# function ####
rmse <- function(predict,real){
  sqrt(mean((real - predict)^2))
}

rsquare <- function(pred,real){
  1 - (sum((real-pred)^2)/sum((real-mean(real))^2))
}

dum <- function(x,name = 'dummy'){
  uni <- length(unique(x))
  a <- matrix(nrow = length(x),ncol = uni)
  for(i in 1:uni){
    a[,i] <- ifelse(x == unique(x)[i],1,0)
  }
  assign(paste0('dummy_',name),a,envir = .GlobalEnv)
}

normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

re_norm <- function(x,stand){
  (x * (max(stand) - min(stand))) + min(stand)
}

# 2. data partition ####
set.seed(4)
idx <- sample(x = c("train", "valid", "test"),
              size = nrow(movie),
              replace = TRUE,
              prob = c(6, 2, 2))

# formula
# 9,10이 중복되는 변수임
formu_gen <- as.formula(paste0('people ~ ',paste0(names(movie)[c(4:8,10,11)],collapse = '+')))
formu_gen 

# 3. modeling ####
# *** knn ####
library(FNN)
movie2 <- movie[,-c(2,9)]
movie2 <- apply(movie2,2,function(x)
  normalize(as.numeric(x))) %>% data.frame()

train2 <- movie2[idx == 'train',]
valid2 <- movie2[idx == 'valid',]
test2 <- movie2[idx == 'test',]
y <- movie$people

knn_reg_test <- data.frame(matrix(nrow = 15,ncol = 4))
names(knn_reg_test) <- 
  c('train_rmse','train_rsquare',
    'valid_rmse','valid_rsquare')
pb <- progress_bar$new(total = 15,format = 'in progress... [:bar] :elapsed')

for(i in 1:15){
  cat('knn regression with k : ',i,'\n')
  mda <- knn.reg(train2,train2,train2$people,k = i)$pred
  mdb <- knn.reg(train2,valid2,train2$people,k = i)$pred
  prea <- re_norm(mda,y)
  preb <- re_norm(mdb,y)
  knn_reg_test[i,1] <- rmse(prea,y[idx == 'train'])
  knn_reg_test[i,2] <- rsquare(prea,y[idx == 'train'])
  knn_reg_test[i,3] <- rmse(preb,y[idx == 'valid'])
  knn_reg_test[i,4] <- rsquare(preb,y[idx == 'valid'])
}
knn_reg_test

# best k = 3 in knn regression
md_knnreg_pre <- knn.reg(train2,test2,train2$people,k = 3)$pred %>%
  re_norm(.,y)
(md_knnreg_rms <- rmse(md_knnreg_pre,y[idx == 'test']))
(md_knnreg_rsquare <- rsquare(md_knnreg_pre,y[idx == 'test']))

# *** naive bayes ####
# 사용불가. 분류문제에서만 활용가능.

# *** tree model ####
train <- movie[idx == 'train',-2]
valid <- movie[idx == 'valid',-2]
test <- movie[idx == 'test',-2]

library(tree)
md_tree <- tree(formu_gen,data = train)

plot(md_tree)
text(md_tree)

prune.tree(md_tree) %>% plot()
# best = 6일때
# train valid 데이터로 예측성능을 본다면?

md_tree_test <- matrix(nrow = 15,ncol = 4) %>% data.frame()
names(md_tree_test) <- c('train_rmse','train_rsquare',
                         'valid_rmse','valid_rsquare')

for(i in 2:15){
  pb$tick()
  md <- tree(formu_gen,data = train) %>%
    prune.tree(.,best = i)
  pre_a <- predict(md,newdata = train)
  pre_b <- predict(md,newdata = valid)
  md_tree_test[i,1] <- rmse(pre_a,y[idx == 'train'])
  md_tree_test[i,2] <- rsquare(pre_a,y[idx == 'train'])
  md_tree_test[i,3] <- rmse(pre_b,y[idx == 'valid'])
  md_tree_test[i,4] <- rsquare(pre_b,y[idx == 'valid'])
}

# 성능이 가장 좋은 몇 개의 tree들 중에
# 7이 가장 작은 트리 (= 동성능 대비 효율적 계산)
md_tree <- prune.tree(tree(formu_gen,data = train),best = 7)
md_tree_pre <- predict(md_tree,test)
(md_tree_rms <- rmse(md_tree_pre,y[idx == 'test']))
(md_tree_rsquare <- rsquare(md_tree_pre,y[idx == 'test']))

# *** neuralnet ####
library(neuralnet)
movie_dum <- list()
for(i in c(6:8,10,11)){
  movie_dum[[i]] <- dum(movie[,i])  
}
dumm <- list.cbind(movie_dum) %>% data.frame()

dumm_name <- list()
for(i in c(6:8,10,11)){
  aa <- movie[,i]
  bb <- length(unique(aa))
  dumm_name[[i]] <- paste0('dumm_',names(movie)[i],unique(aa)[1:bb])
}
names(dumm) <- unlist(dumm_name)
movie_neu <- cbind(movie[,c(1,3:5)],dumm)
movie_neu <- apply(movie_neu,2,as.integer) %>% data.frame()
movie_neu <- apply(movie_neu,2,normalize) %>% data.frame()
train_neu <- movie_neu[idx == 'train',]
valid_neu <- movie_neu[idx == 'valid',]
test_neu <- movie_neu[idx == 'test',]
form <- as.formula(paste0('people ~ ',paste0(names(train_neu)[-c(1,2)],collapse = '+')))

pb <- progress_bar$new(total = 15)

tem <- list()
for(x in 1:15){
  pb$tick()
  tem[[x]] <- neuralnet(form,data = train_neu,stepmax = 200000,
                        threshold = 0.01,hidden = x)
  }

pre <- list()
for(x in 2:15){
  pb$tick()
  pre[[x]] <- re_norm(compute(tem[[x]],valid_neu[,-1])$net.result,
                  movie$people)
}

res <- lapply(pre,function(x)
  data.frame(rmse(x,movie$people[idx == 'valid']),
             rsquare(x,movie$people[idx == 'valid'])))

res <- rbindlist(res)
names(res) <- c('valid_rmse','valid_rsquare')
res

# hidden = 2일때 rsquare가 제일 높고, rmse가 제일 낮다.
md_neur <- neuralnet(form,data = train_neu,hidden = 2,
                     threshold = 0.01,stepmax = 200000)
md_neur_pre <- compute(md_neur,test_neu[,-c(1,2)])$net.result
md_neur_pre <- re_norm(md_neur_pre,movie$people)

(md_neur_rms <- rmse(md_neur_pre,y[idx == 'test']))
(md_neur_rsquare <- rsquare(md_neur_pre,y[idx == 'test']))

# *** randomforest ####
library(randomForest)
set.seed(4)
md_rf <- randomForest(formu_gen,data = train,
                   ntree = 1000,importance = TRUE)
md_rf_pre <- predict(md_rf,test)
(md_rf_rms <- rmse(md_rf_pre,test$people))
(md_rf_rsquare <- rsquare(md_rf_pre,test$people))

# 4. result ####
result <- data.frame(test_rmse = c(knn.reg = md_knnreg_rms,
                                   tree = md_tree_rms,
                                   neur = md_neur_rms,
                                   rf = md_rf_rms),
                  test_rsquare = c(knn.reg = md_knnreg_rsquare,
                                   tree= md_tree_rsquare,
                                   neur = md_neur_rsquare,
                                   rf = md_rf_rsquare)) %>%
                  arrange(test_rmse)
result
# write.csv(result,'result.csv')
