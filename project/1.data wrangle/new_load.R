pkgs <- c('dplyr','data.table','progress',
          'stringr','readxl')
sapply(pkgs,require,character.only = TRUE)

setwd('D:/크롬 다운로드')
file_name <- list.files()[str_detect(list.files(),'2018-05-25 ')]

df <- lapply(file_name,read_xlsx,col_names = FALSE)

movie <- rep(list(
  rep(list(list),4)),11)

for(i in 1:11){
  a <- df[[i]] %>% data.frame()
  week <- which(str_detect(a[,1],'주차'))
  vec <- vector(length = 8)
  vec[seq(1,8,2)] <- week
  vec[-seq(1,8,2)] <- c(week[-1]-1,nrow(a))
  from <- vec[seq(1,8,2)]
  tooo <- vec[-seq(1,8,2)]
  for(k in 1:4){
    movie[[i]][[k]] <- a[from[k]:tooo[k],]
  }
}

idx <- list()
for(i in 1:11){
  idx[[i]] <- lapply(movie[[i]],nrow)
}
idx <- unlist(idx)

df <- rbindlist(df) %>% data.frame()
temp <- df[,1] %>% unlist() %>% unname()
temp <- temp[str_detect(temp,'주차')]
temp <- temp[complete.cases(temp)]
st<- str_sub(temp,7,18)
st <- str_replace_all(st,'[^[:digit:]] ','-') %>% as.Date()

play <- rep(st,idx)

df <- cbind(df,play)
df <- df[!is.na(df[,2]),]
colnames(df) <- df[1,]
df <- df[df[,1] != '순위',]
df <- df[,c(2,3,9,13,14,15,17,18)]
df[,c(3,4,5)] <- lapply(df[,c(3,4,5)],as.numeric)

df_num <- df %>% group_by(영화명) %>%
  summarise(screen = sum(스크린수),
            playtt = sum(상영횟수),
            people = sum(관객수),
            wekkk = min('17371'))

df_txt <- df[,c(1,2,6,7)]
df_txt <- df_txt[!duplicated(df_txt[,1]),]

df <- left_join(df_num,df_txt,by = '영화명')

# write.csv(df,'C:/Users/Seung Hun/Desktop/dataming_project/data/new.csv')

