pkgs <- c('dplyr','stringr','lubridate',
          'tidyr')
sapply(pkgs,require,character.only = TRUE)

setwd('C:/Users/Seung Hun/Desktop/dataming_project/data')
 
new <- read.csv('new.csv',stringsAsFactors = FALSE)
old <- read.csv('old.csv',stringsAsFactors = FALSE)

nam <- c('rank','mv_name','screen','play_t',
         'people','record','play_wk','open','country','supplier')
names(new) <- nam
names(old) <- nam

movie <- rbind(new,old)
movie <- movie[!is.na(movie$open),] 
remove(new,old)

# 재개봉한 영화인가?
# na인 영화는 영화제나 출품작으로 재개봉이 아님.
movie$open <- as.Date(movie$open,format = '%Y-%m-%d')
movie$record <- as.Date(movie$record,format = '%Y-%m-%d')
movie$re_open <- ifelse(movie$open <= as.Date('2014-01-01',
                        format = '%Y-%m-%d'),1,0)
movie$re_open[is.na(movie$re_open)] <- 0 

# 한국 사람들이 가장 많이 보는 영화는
# 한국, 미국, 일본
movie$top_country <- ifelse(movie$country %in% 
                          c('한국','미국','일본'),1,0)

# record
movie$record_month <- month(movie$record)
movie$record_date <- day(movie$record)
movie$month_week <- ifelse(movie$record_date %in% 1:7,1,
                    ifelse(movie$record_date %in% 8:14,2,
                    ifelse(movie$record_date %in% 15:21,3,
                    ifelse(movie$record_date %in% 22:28,4,
                    ifelse(movie$record_date %in% 29:36,5,'error')))))

# supplier
movie <- separate(movie,supplier,
                  into = c('supplier'),sep = ',')
movie$supplier[str_detect(movie$supplier,'씨제이|CJ')] <- '씨제이이엔엠'
movie$supplier[str_detect(movie$supplier,'이십세기폭스')] <- '이십세기폭스코리아'
movie$top_sup <- ifelse(str_detect(movie$supplier,c('씨제이|쇼박스|롯데|이십세기|넥스트')),1,0)

names(movie)
movie <- movie[,c(5,2,3,4,7,11:16)]

movie$top_sup[is.na(movie$top_sup)] <- 0
movie <- movie[!is.na(movie$people),]
#write.csv(movie,'C:/Users/Seung Hun/Desktop/dataming_project/data/movie_vis.csv')
movie$record_month <- ifelse(movie$record_month %in% c(12,1,2),4,
                      ifelse(movie$record_month %in% c(3,4,5),1,
                      ifelse(movie$record_month %in% c(6,7,8),2,
                      ifelse(movie$record_month %in% c(9,10,11),3,'error')))) %>% as.factor()
#write.csv(movie,'C:/Users/Seung Hun/Desktop/dataming_project/data/movie.csv')
