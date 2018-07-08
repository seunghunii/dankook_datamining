pkgs <- c('dplyr','ggplot2','ellipse',
          'lattice')
sapply(pkgs,require,character.only = TRUE)

movie <- read.csv('C:/Users/Seung Hun/Desktop/dataming_project/data/movie_vis.csv')
movie <- movie[!is.na(movie$people),-c(1,3)]

# 기본 시각화
# 상관관계 그림(수치형만)
# 스크린 수와 상영횟수 간의 상관관계가 너무 높다.
# 두 변수 중 하나는 제외하고 하는 것이 좋을 것.
# splom으로 산점도도 보려고 했으나, 성능의 문제로 실패함.
heatmap(cor(movie[c(1:4)]),Rowv = NA,Colv = NA)
plotcorr(cor(movie[c(1,3,4)]))
splom(movie[c(1,3,4)])

# 몇주차에 개봉한 영화를 사람들이 많이 볼까?
# 3주차에 많이 본다. 
movie %>% group_by(month_week) %>%
  summarise(a = sum(people,na.rm = TRUE)) %>% 
  mutate(coll = ifelse(a == max(a),1,0)) %>%
  ggplot(aes(factor(month_week),a,fill = coll)) +
  geom_bar(stat = 'identity') +
  theme(legend.position = 'none') +
  xlab('주차') + ylab('총 관객수')

# 월별은?
# 7,12,1에 개봉한 영화들을 많이 봤다.
# 7월 베테랑,암살,택시운전사,군함도 등이 개봉.
# 이상치가 되는 영화가 가장 많은 것.
# 1월에는 검사외전, 국제시장, 공조 등.
# 12월에는 신과함께, 히말라야, 1987.
movie %>% group_by(record_month) %>% summarise(a = sum(people)) %>%
  arrange(-a) %>% mutate(coll = c(rep(1,3),rep(0,9))) %>%
  ggplot(aes(factor(record_month),a,fill = coll)) +
  geom_bar(stat = 'identity') +
  xlab('월') + ylab('총 관객수') +
  theme(legend.position = 'none')

# 겨울에 개봉한 영화를 가장 많이 본다.
# 여름과 큰 차이가 없다.
movie$season <- ifelse(movie$record_month %in% c(12,1,2),1,
                ifelse(movie$record_month %in% c(3,4,5),2,
                ifelse(movie$record_month %in% c(6,7,8),3,
                ifelse(movie$record_month %in% c(9,10,11),4,'error'))))

movie %>% group_by(season) %>% summarise(a = sum(people)) %>%
  arrange(-a) %>% mutate(coll = c(1,0,0,0)) %>%
  ggplot(aes(factor(season),a,fill = coll)) +
  geom_bar(stat = 'identity') + xlab('분기') + ylab('관객수') +
  theme(legend.position = 'none')

# 스크린을 많이 쓰면 사람들이 많이 볼까?
# 많이 본다.
ggplot(movie,aes(screen,people)) + geom_line() +
  xlab('스크린 수') + ylab('총 관객수')

# 많이 틀어도?
# 많이 본다.
ggplot(movie,aes(play_t,people)) + geom_line() +
  xlab('상영횟수') + ylab('총 관객수')

# 그런데 heatmap에서 봤듯 상관관계가 너무 높다.
# 둘 중에 한 변수만 쓰는 것이 좋을것 같다.
ggplot(movie,aes(play_t,screen)) + geom_line() +
  xlab('상영횟수') + ylab('스크린 수')

# 재개봉 영화와 그렇지 않은 영화의 관객 boxplot
# 차이가 있다고 볼 수 있다.
# 상대적으로 재개봉하지 않은 영화에서 관객수의 이상치가 훨씬 많다.
with(movie,boxplot(log(people+1)~re_open,notch = TRUE))

# 영화산업결산 자료에서 가장 규모가 큰 배급사 여부인 top_sup
# 해당하는 영화들의 관객수가 훨씬 많다.
with(movie,boxplot(log(people+1)~top_sup,notch = TRUE))

