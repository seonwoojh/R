# 사전 준비

rm(list = ls())

library(dplyr)
library(readr)        
library(stringr)
library(rJava)
library(memoise)
library(KoNLP)
library(wordcloud)
library(RColorBrewer)
library(MASS)
library(plyr)
library(twitteR)
library(base64enc)
library(NIADic)
library(RmecabKo)
library(rtweet)
library(tidyverse)
library(igraph)
library(reshape)
source("./my_twitter_oauth.R")
useNIADic()

# "아이폰12 관련 한글 트윗 1000개를 가져오기"
keyword_ko <- enc2utf8("아이폰12")
iPhone12_kor = searchTwitter(keyword_ko, n=1000, lang = "ko")

# 데이터 미리보기
head(iPhone12_kor)


# 데이터 프레임으로 만들기
#df_iphone_ko = twListToDF(iPhone12_kor)

# 추출한 데이터 중 텍스트 파일만 추출
df_ko_word <- sapply(iPhone12_kor,function(t) t$getText()) 

# 한글 - 품사별 처리

iphone12_kors <- df_ko_word %>% SimplePos09()

# 데이터 셋 만들기
iphone12_kors <- iphone12_kors %>%   
  melt() %>%  
  as_tibble() %>% 
  select(3,1)  

# 명사 추출
iphone12_ko_명사 <- iphone12_kors %>%  
  mutate(명사=str_match(value,'([가-힣]+)/N')[,2]) %>%
  
  na.omit() %>%
  
  mutate(글자수=str_length(명사)) %>%
  
  filter(str_length(명사)>=2)

# 용언 추출
iphone12_ko_용언 <- iphone12_kors %>%  
  mutate(용언=str_match(value,'([가-힣]+)/P')[,2]) %>%
  na.omit() %>%                        
  
  mutate(글자수=str_length(용언)) %>%       
  filter(str_length(용언)>=2)

# 수식언 추출
iphone12_ko_수식언 <- iphone12_kors %>%  
  mutate(수식언=str_match(value,'([가-힣]+)/M')[,2]) %>%
  na.omit() %>%                                
  mutate(글자수=str_length(수식언)) %>%
  filter(str_length(수식언)>=2)             

# 명사 전처리
iphone12_ko.명사 <- iphone12_ko_명사$명사 
iphone12_ko.명사 <- iphone12_ko.명사 %>% unlist() 
iphone12_ko.명사 <- iphone12_ko.명사 %>% as.vector() 
iphone12_ko.명사 <- str_replace_all(iphone12_ko.명사, "[^[:alnum:][:blank:]+?&/\\-]","")
iphone12_ko.명사 <- str_replace_all(iphone12_ko.명사, "^.{1}$","")
iphone12_ko.명사 <- str_replace_all(iphone12_ko.명사, "\\d+","") 
iphone12_ko.명사 <- iphone12_ko.명사 %>% as.list() 
iphone12_ko.명사[iphone12_ko.명사 ==""] <- NULL 
iphone12_ko.명사 <- iphone12_ko.명사 %>% unlist()
iphone12_ko.명사 <- iphone12_ko.명사 %>% as.data.frame()

# 용언 전처리
iphone12_ko.용언 <- iphone12_ko_용언$용언 
iphone12_ko.용언 <- iphone12_ko.용언 %>% unlist() 
iphone12_ko.용언 <- iphone12_ko.용언 %>% as.vector() 
iphone12_ko.용언 <- str_replace_all(iphone12_ko.용언, "[^[:alnum:][:blank:]+?&/\\-]","")
iphone12_ko.용언 <- str_replace_all(iphone12_ko.용언, "^.{1}$","")
iphone12_ko.용언 <- iphone12_ko.용언 %>% as.list() 
iphone12_ko.용언[iphone12_ko.용언 ==""] <- NULL 
iphone12_ko.용언 <- iphone12_ko.용언 %>% unlist()               
iphone12_ko.용언 <- iphone12_ko.용언 %>% as.data.frame() 

# 수식언 전처리
iphone12_ko.수식언 <- iphone12_ko_수식언$수식언 
iphone12_ko.수식언 <- iphone12_ko.수식언 %>% unlist() 
iphone12_ko.수식언 <- iphone12_ko.수식언 %>% as.vector() 
iphone12_ko.수식언 <- str_replace_all(iphone12_ko.수식언, "[^[:alnum:][:blank:]+?&/\\-]","")
iphone12_ko.수식언 <- str_replace_all(iphone12_ko.수식언, "^.{1}$","")
iphone12_ko.수식언 <- iphone12_ko.수식언 %>% as.list() 
iphone12_ko.수식언[iphone12_ko.수식언 ==""] <- NULL 
iphone12_ko.수식언 <- iphone12_ko.수식언 %>% unlist()               
iphone12_ko.수식언 <- iphone12_ko.수식언 %>% as.data.frame()

# 명사 용언 수식언을 묶어서 하나로 만듭니다
iphone12_ko <- bind_rows(iphone12_ko.명사,iphone12_ko.용언,iphone12_ko.수식언)

#여기까지 정리
iphone12_ko_count <- table(iphone12_ko)                     
iphone12_ko_count <- sort(iphone12_ko_count, decreasing = TRUE) 

iphone12_ko_count30 <- iphone12_ko_count[1:10]           ## Top 30까지 추립니다


iphone12_ko_count30df <- iphone12_ko_count30 %>% as.data.frame()             ## data frame변환하고 그래프 작성 
ggplot(iphone12_ko_count30df, aes(x=iphone12_ko, y=Freq),) + geom_bar(stat="identity")

iphone12_ko_count %>% wordcloud()
iphone12_ko_count[2:length(iphone12_ko_count)] %>% wordcloud2()  
?ggplot
iphone12_ko

blue <- brewer.pal(6,"Blues")
wordcloud(
  
  names(iphone12_ko_count),
  
  freq=iphone12_ko_count,
  
  scale=c(5,1),
  
  rot.per=0.5,
  
  min.freq=7,
  
  random.order=F,
  
  colors = blue
)
iphone12_ko_count[1:60]
