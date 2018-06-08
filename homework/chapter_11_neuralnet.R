pkgs <- c('dplyr','neuralnet','MASS',
          'tree','readxl','data.table')
sapply(pkgs,require,character.only = TRUE)

# 데이터
setwd('C:/Users/Seung Hun/Desktop')
bos <- read_xlsx('BostonHousing.xlsx',sheet = 2) %>%
  data.frame()
colnames(bos) <- tolower(colnames(bos))

# 명목형 변수 더미변수화
# chas와 rad, cat..medv
# 더미변수 함수 만들기
dum <- function(x,name = ' dummy'){
  idx <- length(unique(x))
  a <- matrix(nrow = length(x),ncol = idx)
  for(i in 1:idx){
    a[,i] <- ifelse(x == unique(x)[i],1,0)
    assign(paste0('dummy_',name),a,envir = .GlobalEnv)
  }
}

dum(bos$chas,'chas')
table(apply(dummy_chas,1,sum) == 1)  # rowsum == 1 (true 506)

dum(bos$rad,'rad')
table(apply(dummy_rad,1,sum) == 1) # rowsum == 1 (true 506)

dum(bos$cat..medv,'cat.medv')
table(apply(dummy_cat.medv,1,sum) == 1) # rowsum == 1 (true 506)

dummy <- cbind(dummy_chas,dummy_rad,dummy_cat.medv) %>% data.frame()
names(dummy) <- c(paste0('chas',1:ncol(dummy_chas)),
                  paste0('rad',1:ncol(dummy_rad)),
                  paste0('cat.medv',1:ncol(dummy_cat.medv)))
bos <- cbind(bos,dummy)
rm(dummy,dummy_chas,dummy_rad,dummy_cat.medv)

bos$chas <- factor(bos$chas)
bos$rad <- factor(bos$rad)
bos$cat..medv <- factor(bos$cat..medv)

# 분할
set.seed(4)
idx <- sample(1:nrow(bos),nrow(bos) * 0.7)
train <- bos[idx,1:13]
test <- bos[-idx,1:13]
names(train)

# medv를 종속변수로 하는 예측문제
# 회귀분류나무
md_tree <- tree(medv ~.,data = train)
plot(md_tree)
text(md_tree)

# 최적의 나무
prune.tree(md_tree) %>% plot()
md_prune <- prune.tree(md_tree,k = 8)

# 성능측정
# train
train_reg_tr <- predict(md_tree,train)
train_reg_pu <- predict(md_prune,train)

sqrt(mean((train$medv - train_reg_tr)^2)) # 3.877554761
sqrt(mean((train$medv - train_reg_pu)^2))

# test
test_reg_tr <- predict(md_tree,test)
test_reg_pu <- predict(md_prune,test)

sqrt(mean((test$medv-test_reg_tr)^2)) # 5.288670701
sqrt(mean((test$medv-test_reg_pu)^2))

# k를 7로 했을때와 그냥 tree model을 적합했을때
# 두 모델이 동일하다
identical(md_tree,md_prune) # true
remove(test_reg_pu,train_reg_pu,md_prune)

# 분할
train <- bos[idx,c(1:3,5:8,10:13,15:25)]
test <- bos[-idx,c(1:3,5:8,10:13,15:25)]
names(train)

# neuralnet
form <- as.formula(paste('medv ~ ',
                   paste(names(train)[-11], collapse = ' + ')))

# 최고의 은닉층 노드 찾기
# hidden = 6에서 stop.
node_reg <- list()
for(i in 1:10){
  md <- neuralnet(form,data = train,hidden = i,lifesign = 'full',
                  threshold = 0.01,linear.output = T)
  test_y <- bos[-idx,'medv']
  pre_tes <- compute(md,test[,-11])
  node_reg[[i]] <- sqrt(mean((test_y - pre_tes$net.result)^2))
  }

md_reg_neur <- neuralnet(form,data=train,hidden = 6,
                         threshold = 0.01,linear.output = T)
plot(md_reg_neur)

# 가중치
md_reg_neur$weights

# 성능평가
# train
train_reg_nu <- compute(md_reg_neur,train[-11]) 
sqrt(mean((train$medv-train_reg_nu$net.result)^2)) # 9.144682996

# test
test_reg_nu <- compute(md_reg_neur,test[-11]) 
sqrt(mean((test$medv-test_reg_nu$net.result)^2)) # 9.289712519

# medv regression 문제에서의 성능비교(rmse)
(regression <- data.frame(train_tree = sqrt(mean((train$medv - train_reg_tr)^2)),
                          test_tree = sqrt(mean((test$medv - test_reg_tr)^2)),
                          train_neur = sqrt(mean((train$medv-train_reg_nu$net.result)^2)),
                          test_neur = sqrt(mean((test$medv-test_reg_nu$net.result)^2))))
# 예측문제에서는 neuralnet의 rmse가 더 크다.

# 종속변수를 cat.medv로 하는 분류문제.
train <- bos[idx,c(1:12,14)]
test <- bos[-idx,c(1:12,14)]

# 성능평가함수
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

# 나무 모형
md_tree2 <- tree(cat..medv ~.,data = train)

prune.tree(md_tree2) %>% plot()
md_prune <- prune.tree(md_tree2,k = 7)

# 성능측정
# train
train_cls_tr <- predict(md_tree2,train,type = 'class')
train_cls_pu <- predict(md_prune,train,type = 'class')

perf(train_cls_tr,train$cat..medv)
perf(train_cls_pu,train$cat..medv)

# test
test_cls_tr <- predict(md_tree2,test,type = 'class')
test_cls_pu <- predict(md_prune,test,type = 'class')  

perf(test_cls_tr,test$cat..medv)
perf(test_cls_pu,test$cat..medv)

# 여기서도 두 tree가 동일하다
identical(md_tree2,md_prune)
remove(md_prune,train_cls_pu,test_cls_pu)

# neuralnet
train <- bos[idx,c(1:3,5:8,10:12,15:27)]
test <- bos[-idx,c(1:3,5:8,10:12,15:27)]

form <- as.formula(paste('cat.medv1 + cat.medv2 ~',
                         paste(colnames(train[,-c(22,23)]), collapse = ' + ')))

# 은닉층 노드 수 정하기
node_cls <- list()
for(i in 1:10){
  md <- neuralnet(form,data = train,hidden = i,
                  threshold = 0.01,linear.output = F,
                  lifesign = 'full')
   test_y <- bos[-idx,'cat..medv']
  pre_tes <- compute(md,test[,-c(22,23)])
  pre_tes_class <- apply(pre_tes$net.result,1,which.max)-1
  node_cls[[i]] <- c(perf(pre_tes_class,test_y))
}
(node_cls <- rbindlist(node_cls))

md_cls_neur <- neuralnet(form,data = train,hidden = 6,
                         threshold = 0.01,linear.output = F)

# 가중치
md_cls_neur$weights

# plot
plot(md_cls_neur)

# 성능평가
# train
train_cls_nu <- compute(md_cls_neur,train[,-c(22,23)])
train_cls_nu_class <- apply(train_cls_nu$net.result,1,which.max)-1
(train_perf <- perf(train_cls_nu_class,bos[idx,'cat..medv']))

# test
test_cls_nu <- compute(md_cls_neur,test[,-c(22,23)])
test_cls_nu_class <- apply(test_cls_nu$net.result,1,which.max)-1
(test_perf <- perf(test_cls_nu_class,bos[-idx,'cat..medv']))

(classification <- rbind(train_tree = perf(train_cls_tr,bos[idx,'cat..medv']),
                          test_tree = perf(test_cls_tr,bos[-idx,'cat..medv']),
                         train_neur = perf(train_cls_nu_class,bos[idx,'cat..medv']),
                          test_neur = perf(test_cls_nu_class,bos[-idx,'cat..medv'])))

classification
regression
