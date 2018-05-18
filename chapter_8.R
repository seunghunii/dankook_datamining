# chapter8_naive bayes
# 데이터마이닝 4조 과제.
pkgs <- c('readxl','dplyr','ggplot2',
          'stringr','e1071','caret')
sapply(pkgs,require,character.only = TRUE)

wd <- 'C:/Users/Seung Hun/Desktop'
setwd(wd)

# 데이터 불러오기 & 열이름 정리
universal <- read_xlsx('UniversalBank.xlsx',sheet = 2)
colnames(universal) <- make.names(colnames(universal))
universal$Personal.Loan <- as.factor(universal$Personal.Loan)

# 데이터 분할
set.seed(4)
indx <- sample(1:nrow(universal),nrow(universal) * 0.6)
train <- universal[indx,c('Online','CreditCard','Personal.Loan')]
test <- universal[-indx,c('Online','CreditCard','Personal.Loan')]

# b. pivot table
with(train,table(Online,CreditCard,Personal.Loan))
ftable(train[,c('Online','CreditCard','Personal.Loan')])

# 신용카드를 소유하고 있고, 
# 온라인 뱅킹 서비스의 적극적 사용자
with(train,sum(Online == 1 & CreditCard == 1)) # 544

# 신용카드를 소유하고 있고, 
# 온라인 뱅킹 서비스의 적극적 사용자들 주
# 대부를 수용받은 사람
with(train,sum(Online == 1 & CreditCard == 1
               & Personal.Loan == 1)) # 50

# 완전한 베이즈 확률
50 / 544 # 0.092(9.2%)

# g. 변수 추출, 펙터화
(model <- naiveBayes(Personal.Loan ~ .,data = train))

# 학습데이터 예측
train_pre <- predict(model,train)
table(train_pre,train$Personal.Loan)
confusionMatrix(train_pre,train$Personal.Loan,
                positive = '1')

# 검증데이터 예측
test_pre <- predict(model,test)
table(test_pre,test$Personal.Loan)
confusionMatrix(test_pre,test$Personal.Loan,
                positive = '1')

# Credit Card = 1 & Online = 1 일때
# 대부를 수용받을지에 대한 예측
newdata <- data.frame(Online = 1, CreditCard = 1)
predict(model,newdata,type = 'raw')
