pkgs <- c('dplyr','readxl','stringr','lubridate',
          'data.table','rlist','tree')
sapply(pkgs,require,character.only = TRUE)

wd <- 'C:/Users/Seung Hun/Desktop'
setwd(wd)

flight <- read_xlsx('FlightDelays.xlsx',sheet = 2)
colnames(flight) <- tolower(colnames(flight)) %>%
  str_replace_all(.,' ','_')

# 전처리
# - 요일
# day_week가 있으나, 숫자로 되어있어
# 일요일부터인지 월요일부터인지가 불분명해 날짜를 활용
wdd <- ymd(flight$fl_date) %>%
  lubridate::wday(label = TRUE)

date <- list()
for(i in 1:6){
  date[[i]] <- 
    ifelse(wdd == levels(wdd)[i],1,0)
  }
date <- list.cbind(date)

# 가변수 만들기 전 펙터화
flight[,c('carrier','origin','dest')] <- 
  apply(flight[,c('carrier','origin','dest')],2,as.factor) %>% data.frame

# - 항공기
plane <- list()
for(i in 1:7){
  plane[[i]] <- ifelse(flight$carrier == levels(flight$carrier)[i],1,0)
}
plane <- list.cbind(plane)

# - 출발공항
depart <- list()
for(i in 1:2){
  depart[[i]] <- ifelse(flight$origin == levels(flight$origin)[i],1,0)
}
depart <- list.cbind(depart)

# - 도착공항
arrive <- list()
for(i in 1:2){
  arrive[[i]] <- ifelse(flight$dest == levels(flight$dest)[i],1,0)
}
arrive <- list.cbind(arrive)

# 출발시간, 예정출발시간
time <- cut(flight$dep_time,8)
levels(time) <- paste0('dept_',letters[1:8])

crs_time <-cut(flight$crs_dep_time,
           breaks = c(seq(0,2400,200)),
           include.lowest = TRUE)
levels(crs_time) <- letters[1:8]

# 데이터와 합치기
dummy <- data.frame(cbind(date,plane,arrive,depart,time,crs_time))
names(dummy) <- c(levels(wdd)[1:6],
                  paste0('plane_',levels(flight$carrier)[1:7]),
                  paste0('from_',levels(flight$origin)[1:2]),
                  paste0('to_',levels(flight$dest)[1:2]),
                  'time','crs_time')
flight <- cbind(flight,dummy)
remove(date,plane,arrive,depart,time,crs_time,dummy)

# 적절한 예측변수
flight_par <- flight[,c(13,5,9,11,14:30,32)]
flight_par <- data.frame(apply(flight_par,2,as.factor))
flight_par$day_of_month <- as.numeric(flight_par$day_of_month)
flight_par$distance <- as.numeric(flight_par$distance)

# 데이터 분할
set.seed(4)
idx <- sample(1:nrow(flight_par),0.7 * nrow(flight_par))
train <- flight_par[idx,]
test <- flight_par[-idx,]

# a. 분류나무모델
md <- tree(flight_status ~., data = train,mincut = 0)
summary(md)

pre <- predict(md,test,type = 'class')
table(test$flight_status,pre)

plot(md)
text(md)

# a의 결과를 가지치기
prune.tree(md) %>% plot() # k = 2
(a_prune <- prune.tree(md,k = 2))
summary(a_prune)

plot(a_prune)
text(a_prune)

pre <- predict(a_prune,test,type = 'class')
table(test$flight_status,pre)

# b. 오전 7시, DCA -> EWR
# 우선 데이터가 있다.
# 출발지와 목적지, 비행기,거리가 중복된 정보이다.
# 추가로 날씨와 몇일의 비행기인지에 대한 정보가 필요하다.
# 모델을 실제로 예측에 활용할 수 있을 것으로 보인다.
with(flight_par,flight_par[from_DCA == 1 & to_EWR == 1,])

# c. 예정날짜 제외한 모델
train2 <- train[,-4]
test2 <- test[,-4]
md_noday <- tree(flight_status ~., data = train2)
summary(md_noday)
plot(md_noday)
text(md_noday)

pre <- predict(md_noday,test2,type = 'class')
table(test$flight_status,pre)

# 완전히 성장한 나무모델
md_full <- tree(flight_status ~., data = train2,
                   mindev = 0, minsize = 2)
summary(md_full)
plot(md_full)
text(md_full)

pre <- predict(md_full,test2,type = 'class')
table(test$flight_status,pre)

# 가지치기 된 나무모델
# detect k?
prune.tree(md_noday) %>% plot() # k = 2
(md_prune <- prune.tree(md_noday,k = 2))
plot(md_prune)
text(md_prune)

pre <- predict(md_noday3,test2,type = 'class')
table(test$flight_status,pre)
