pkgs <- c('dplyr','ggplot2','ellipse')
sapply(pkgs,require,character.only = TRUE)

movie <- read.csv('C:/Users/Seung Hun/Desktop/dataming_project/data/movie.csv')
movie <- movie[!is.na(movie$people),]

# 기본 시각화
# 상관관계 그림
heatmap(cor(movie,use = 'complete.obs'),Rowv = NA,Colv = NA)
plotcorr(cor(movie,use = 'complete.obs'))

# 몇주차에 개봉한 영화를 사람들이 많이 볼까?
# 3주차에 많이 본다. 
movie %>% group_by(month_week) %>% summarise(a = sum(people,na.rm = TRUE)) %>%
  ggplot(aes(month_week,a)) + geom_bar(stat = 'identity') +
  xlab('주차') + ylab('총 관객수')

# 날짜가 지날수록 많이 볼까?
# 몇몇 이상치를 제외하고는 큰 의미없다.
movie %>% group_by(record_date) %>% summarise(a = sum(people)) %>%
  ggplot(aes(record_date,a)) + geom_bar(stat = 'identity') +
  scale_x_continuous(breaks = 1:31) + xlab('개봉날짜') +
  ylab('총 관객수')

# 월별은?
# 7,12,1에 개봉한 영화들을 많이 봤다.
# 7월 베테랑,암살,택시운전사,군함도 등이 개봉.
# 이상치가 되는 영화가 가장 많은 것.
# 1월에는 검사외전, 국제시장, 공조 등.
# 12월에는 신과함께, 히말라야, 1987.
movie %>% group_by(record_month) %>% summarise(a = sum(people)) %>%
  ggplot(aes(record_month,a)) + geom_bar(stat = 'identity') +
  scale_x_continuous(breaks = 1:12) + xlab('월') +
  ylab('총 관객수')

# 스크린을 많이 쓰면 사람들이 많이 볼까?
# 많이 본다.
ggplot(movie,aes(screen,people)) + geom_line() +
  xlab('스크린 수') + ylab('총 관객수')

# 많이 틀어도?
# 많이 본다.
ggplot(movie,aes(play_t,people)) + geom_line() +
  xlab('상영횟수') + ylab('총 관객수')

# 재개봉 영화와 그렇지 않은 영화의 관객 boxplot
# 차이가 있다고 볼 수 있다.
# 상대적으로 재개봉하지 않은 영화에서 관객수의 이상치가 훨씬 많다.
with(movie,boxplot(log(people+1)~re_open,notch = TRUE))

# 영화산업결산 자료에서 가장 규모가 큰 배급사 여부인 top_sup
# 해당하는 영화들의 관객수가 훨씬 많다.
with(movie,boxplot(log(people+1)~top_sup,notch = TRUE))

