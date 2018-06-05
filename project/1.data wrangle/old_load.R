pkgs <- c('data.table','dplyr','stringr')
sapply(pkgs,require,character.only = TRUE)

setwd('D:/크롬 다운로드/기러기')
lf <- list.files()

datas <- lapply(lf,read.csv,stringsAsFactors = FALSE)
idx <- sapply(datas,nrow)
week <- paste0('20',str_sub(lf,1,8)) %>%
  str_replace_all(.,'[^[:digit:]]','-') %>%
  as.Date()

df <- rbindlist(datas) 
week <- rep(week,idx) %>% as.Date()
old <- cbind(df,week) %>% data.frame()
old <- old[old$영화명 != '',]
old <- old[,c(1,2,3,7,9,10,11,14,19)]
old[,c(4,5,6)] <- lapply(old[,c(4,5,6)],as.numeric)
  
old_num <- old %>% group_by(영화명) %>%
  summarise(screen = sum(스크린수),
            playtt = sum(상영횟수),
            people = sum(관객수),
            weekkk = min(week))

old_txt <- old[,c(2,3,7,8)]
old_txt <- old_txt[!duplicated(old_txt[,1]),]

old <- left_join(old_num,old_txt)
# write.csv(old,'C:/Users/Seung Hun/Desktop/dataming_project/data/old.csv')
