pkgs <- c('dplyr','neuralnet','ggplot2',
          'rlist','tree')
sapply(pkgs,require,character.only = TRUE)

acc <- read.csv('C:/Users/Seung Hun/Desktop/Accidents.csv')
acc <- acc[acc$SUR_COND != 9,]

# 사용자함수 설계
# 더미화
dum <- function(x,name = 'dummy'){
  uni <- length(unique(x))
  a <- matrix(nrow = length(x),ncol = uni)
  for(i in 1:uni){
    a[,i] <- ifelse(x == unique(x)[i],1,0)
  }
  assign(paste0('dummy_',name),a,envir = .GlobalEnv)
}

# 정규화
normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

# 성능평가
perf <- function(pred,real){
  if(length(pred) != length(real)){
    warning('No match in length',
            '\n',paste0('pred_length : ',length(pred)),
            '\n',paste0('real_length : ',length(real)))
    break()}
  sensitivity <- sum(pred == 1 & real == 1) / sum(real == 1)
  specificity <- sum(pred == 0 & real == 0) / sum(real == 0)
  accuracy    <- mean(pred == real)
  error <- mean(pred != real)
  (perform <- data.frame(sensitivity,specificity,accuracy,error))
}

# neuralnet model
# 변수 더미화
acc_dum <- list()

# dummy
for(i in setdiff(1:24,c(2,14,18,21))){
  acc_dum[[i]] <- dum(acc[,i])
}

# scale
for(i in c(2,14,18,21)){
  acc_dum[[i]] <- scale(acc[,i])
}

acc_neu <- list.cbind(acc_dum) %>% data.frame()

namm <- list()
for(i in setdiff(1:24,c(2,14,18,21))){
  bb <- length(unique(acc[,i]))
  namm[[i]] <- paste0(names(acc)[i],'_',unique(acc[,i]))
}
for(i in c(2,14,18,21)){
  namm[[i]] <- names(acc)[i]
}

names(acc_neu) <- unlist(namm)

formu <- as.formula(paste0(paste0(names(acc_neu)[c(51,50,52)],collapse = "+"),' ~ ',
                           paste0(names(acc_neu)[1:49],collapse = "+")))

set.seed(4)
idx <- sample(1:nrow(acc_neu),0.7 * nrow(acc_neu))
train_neu <- acc_neu[idx,]
test_neu <- acc_neu[-idx,]

md_neu <- neuralnet(formu,threshold = 0.01,data = train_neu)
pre_neu <- compute(md_neu,test_neu[,-c(51,50,52)])$net.result
pre_neu2 <- compute(md_neu,train_neu[,-c(51,50,52)])$net.result

pre_neu <- apply(pre_neu,1,which.max)-1
pre_neu2 <- apply(pre_neu2,1,which.max)-1

pre_neu <- unname(pre_neu) %>% as.numeric()
pre_neu2 <- unname(pre_neu2) %>% as.numeric()

neu_perf <- perf(pre_neu,acc[-idx,24])
neu_perf2 <- perf(pre_neu2,acc[idx,24])
neuralnet_perf <- cbind(neu_perf2,neu_perf)

# tree model
# tree
for(i in setdiff(1:24,c(2,14,18,21))){
  acc[,i] <- as.factor(acc[,i])
}

train <- acc[idx,]
test <- acc[-idx,]

md_tree <- tree(MAX_SEV_IR ~.,data = train)
prune.tree(md_tree) %>% plot()

for(i in 2:3){
  md <- prune.tree(md_tree,best = i)
  pre1 <- predict(md,train,type = 'class')
  pre2 <- predict(md,test,type = 'class')
  print(rbind(perf(pre1,train$MAX_SEV_IR),
              perf(pre2,test$MAX_SEV_IR)))
  }

md_tree <- prune.tree(md_tree,best = 3)

pre_tre <- predict(md_tree,test,type = 'class')
pre_tre2 <- predict(md_tree,train,type = 'class')

tre_perf <- perf(pre_tre,test$MAX_SEV_IR)
tre_perf2 <- perf(pre_tre2,train$MAX_SEV_IR)
tree_perf <- cbind(tre_perf2,tre_perf)

result <- (rbind(neuralnet_perf,tree_perf))
