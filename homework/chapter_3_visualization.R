# set
pkgs <- c('dplyr','ggplot2','readxl',
          'stringr','lubridate','ggmap','Hmisc','ellipse',
          'lattice','GGally','spatstat')
sapply(pkgs,require,character.only = TRUE)

# 3.3
lap_2008 <- read_xlsx('C:/Users/Seung Hun/Desktop/데이터마이닝/LaptopSalesJanuary2008.xlsx',
                      sheet = 2)
lap_2008 <- as.data.frame(lap_2008)
glimpse(lap_2008)

# a
# 가장 낮은 매장 = W4 3PH 481
# 가장 높은 매장 = N17 6QA 495
lap_2008 %>% group_by(`Store Postcode`) %>%
  summarise(a = mean(`Retail Price`)) %>%
  ggplot(aes(reorder(`Store Postcode`,a),a,group = a)) +
  geom_bar(stat = 'identity') +
  coord_cartesian(ylim = c(450,500)) + ylab("Mean Price")

# b
lap_2008 %>% filter(`Store Postcode` %in% c('W4 3PH','N17 6QA')) %>%
  ggplot(aes(`Store Postcode`,`Retail Price`,group = `Store Postcode`)) +
  stat_boxplot(geom = 'errorbar',width = 0.3) +
  geom_boxplot(notch = TRUE)

# 3.4
lap_sale <- read.table('C:/Users/Seung Hun/Desktop/데이터마이닝/LaptopSales.txt',sep = "\t",header = TRUE)
lap_sale$Date_my <- dmy_hms(lap_sale$Date)
glimpse(lap_sale)

# a.i
boxplot(lap_sale$Retail.Price)

# a.ii
# by day.
lap_sale %>% mutate(da = day(Date_my)) %>%
  group_by(da) %>% 
  summarise(a = mean(Retail.Price,na.rm = TRUE)) %>%
  ggplot(aes(da,a,group = 1)) + geom_bar(stat = 'identity') +
  coord_cartesian(ylim = c(500,530))

# by month
lap_sale %>% mutate(da = month(Date_my)) %>%
  mutate(da = factor(str_replace_na(da),levels = c(seq(1,12,1),'NA'))) %>%
  group_by(da) %>%  summarise(a = mean(Retail.Price,na.rm = TRUE)) %>%
  ggplot(aes(da,a,group = 1)) + geom_bar(stat = 'identity') +
  coord_cartesian(ylim = c(440,545))

# by time
lap_sale %>% mutate(da = time(Date_my)) %>%
  mutate(da = factor(str_replace_na(da),levels = c(seq(1,12,1),'NA'))) %>%
  group_by(da) %>%  summarise(a = mean(Retail.Price,na.rm = TRUE)) %>%
  ggplot(aes(da,a,group = 1)) + geom_bar(stat = 'identity') +
  coord_cartesian(ylim = c(350,670))

# a.iii
# boxplot
# original
lap_sale %>% ggplot(aes(Store.Postcode,Retail.Price,
                        group = Store.Postcode)) +
  stat_boxplot(geom = 'errorbar',width = 0.5) +
  geom_boxplot(outlier.alpha = 0.3)

# only box
lap_sale %>% ggplot(aes(Store.Postcode,Retail.Price,
                        group = Store.Postcode)) +
  geom_boxplot(notch = TRUE) + coord_cartesian(ylim = c(300,630))

# a.iv
# by screen size?
lap_sale %>% group_by(Screen.Size..Inches.) %>%
  summarise(a = mean(Retail.Price,na.rm = TRUE)) %>%
  ggplot(aes(factor(Screen.Size..Inches.),a,group = Screen.Size..Inches.)) +
  geom_bar(stat = 'identity') + coord_cartesian(ylim = c(400,560))

# by battery life?
lap_sale %>% group_by(Battery.Life..Hours.) %>%
  summarise(a = mean(Retail.Price,na.rm = TRUE)) %>%
  ggplot(aes(Battery.Life..Hours.,a,group = Battery.Life..Hours.)) +
  geom_bar(stat = 'identity') + coord_cartesian(ylim = c(450,550))

# by ram?
lap_sale %>% group_by(RAM..GB.) %>%
  summarise(a = mean(Retail.Price,na.rm = TRUE)) %>%
  ggplot(aes(factor(RAM..GB.),a,group = RAM..GB.)) +
  geom_bar(stat = 'identity')

# by processor speeds ghz.
lap_sale %>% group_by(Processor.Speeds..GHz.) %>%
  summarise(a = mean(Retail.Price,na.rm = TRUE)) %>%
  ggplot(aes(factor(Processor.Speeds..GHz.),a,group = Processor.Speeds..GHz.)) +
  geom_bar(stat = 'identity') + coord_cartesian(ylim = c(450,540))

# by integrated wireless
lap_sale %>% group_by(Integrated.Wireless.) %>%
  summarise(a = mean(Retail.Price,na.rm = TRUE)) %>%
  ggplot(aes(Integrated.Wireless.,a,group = Integrated.Wireless.)) +
  geom_bar(stat = 'identity') + coord_cartesian(ylim = c(500,520))

# by hd.size.gb
lap_sale %>% group_by(HD.Size..GB.) %>%
  summarise(a = mean(Retail.Price,na.rm = TRUE)) %>%
  ggplot(aes(factor(HD.Size..GB.),a,group = HD.Size..GB.)) +
  geom_bar(stat = 'identity')

# bundled applications.
lap_sale %>% group_by(Bundled.Applications.) %>%
  summarise(a = mean(Retail.Price,na.rm = TRUE)) %>%
  ggplot(aes(Bundled.Applications.,a,group = Bundled.Applications.)) +
  geom_bar(stat = 'identity') + coord_cartesian(ylim = c(470,530))

# b
# b.i
# code <- geocode(as.character(unique(lap_sale$Store.Postcode)))
# code <- cbind(code,as.character(unique(lap_sale$Store.Postcode)))
# code2 <- geocode(as.character(unique(lap_sale$Customer.Postcode)))
# code2 <- cbind(code2,as.character(unique(lap_sale$Customer.Postcode)))

store <- read.csv('C:/Users/Seung Hun/Desktop/rstudio-export/store.csv')
customer <- read.csv('C:/Users/Seung Hun/Desktop/rstudio-export/customer.csv')

loon <- rbind(store[,2:3],customer[,2:3])
loon <- cbind(loon,wh = c(rep('store',nrow(store)),rep('customer',nrow(customer))))

# lon <- get_map(location="london", zoom=11, maptype='terrain', source='google', color='color')
place <- ggmap(lon) +
         geom_point(data = loon,aes(lon,lat,col = wh)) +
         scale_color_manual(values = c(alpha('red',0.4),'black'))
place

# b.ii
# freq?
lap_sale %>% group_by(Store.Postcode) %>%
  summarise(a = n()) %>% ggplot(aes(Store.Postcode,a,group = Store.Postcode)) +
  geom_bar(stat = 'identity')

# average price?
lap_sale %>% group_by(Store.Postcode) %>%
  summarise(a = mean(Retail.Price,na.rm = TRUE)) %>%
  ggplot(aes(Store.Postcode,a,group = Store.Postcode)) +
  geom_bar(stat = 'identity') + coord_cartesian(ylim = c(400,530))

# b.iii
place + geom_point(data = store,aes(lon,lat),alpha = 0.4,size = 35)

# b.iv
# 변수만들기
cust <- customer[complete.cases(customer),]
st <- store[complete.cases(store),]
a <- vector(length = nrow(cust))
dist <- vector(length = nrow(cust))

for(i in 1:nrow(cust)){
  tem <- rbind(st[,2:3],cust[i,2:3])
  a[i] <- nnwhich(tem)[nrow(tem)]
  dist[i] <- nndist(tem)[nrow(tem)]
}

# 가장 가까운 매장 통계
summ <- data.frame(table(a)) 
summ %>% ggplot(aes(a,Freq,group = a)) +
  geom_bar(stat = 'identity')

# 거리통계
# 거리의 분포
boxplot(dist,ylim = c(0,0.08))

# 비슷한 곳에 사는 손님별?
# 런던을 크게 8개 정도의 구역으로 나눔.
# customer postcode의 첫번째 글자별 거리 평균
aaa <- data.frame(dist = dist,
                  code = cust$as.character.unique.lap_sale.Customer.Postcode..,
                  near = st$as.character.unique.lap_sale.Store.Postcode..[a])
aaa$new_code <- ifelse(str_detect(str_sub(aaa$code,1,2),"[:digit:]") == TRUE,
                       str_sub(aaa$code,1,1),str_sub(aaa$code,1,2))
aaa %>% group_by(new_code) %>% summarise(a = mean(dist)) %>%
  ggplot() + geom_bar(aes(new_code,a),stat = 'identity')

# 가장 가까운 매장별 통계
# b.iii에서 그린 원 밖의 점이 있는 매장이 outlier를 가짐.
aaa %>% group_by(near) %>% summarise(a = mean(dist)) %>%
  ggplot(aes(near,a)) + geom_bar(stat = 'identity') +
  coord_cartesian(ylim = c(0,1))

# c
# c.i
lap_sale$time <- hour(lap_sale$Date_my)
lap_sale$Store.Postcode <- as.character(lap_sale$Store.Postcode)
sale <- lap_sale %>% group_by(time,Store.Postcode) %>%
              summarise(a = mean(Retail.Price,na.rm = TRUE))

sale2 <- lap_sale %>% group_by(time) %>% summarise(a = mean(Retail.Price,na.rm = TRUE))

par(mfrow = c(2,4))
for(i in 1:8){
  b <- which(sale$Store.Postcode == unique(sale$Store.Postcode)[i])
  plot(sale$time[b],sale$a[b],col = "red",ylim = c(400,560),
       xlab = "time",ylab = unique(sale$Store.Postcode)[i])
  points(sale2$time,sale2$a)
}

par(mfrow = c(2,4))
for(i in 9:16){
  b <- which(sale$Store.Postcode == unique(sale$Store.Postcode)[i])
  plot(sale$time[b],sale$a[b],col = "red",ylim = c(400,560),
       xlab = "time",ylab = unique(sale$Store.Postcode)[i])
  points(sale2$time,sale2$a)
}

# c.ii & d.i
lap_sale[,c(10,12)] <- as.character(lap_sale[,c(10,12)])
lap_sale[,c(10,12)] <- ifelse(lap_sale[,c(10,12)] == "Yes",1,0)

plotcorr(cor(lap_sale[,c(5,6:12)]),main = 'Retail Price by components')
heatmap(cor(lap_sale[,c(5,6:12)]),Rowv = NA,Colv = NA)

# d.ii
perfor <- apply(lap_sale[6:12],2,
                function(x)data.frame(table(lap_sale$Store.Postcode,x)))

render('datamine.R','pdf_document')
