#스타벅스 감성분석

library(twitteR)

twitter_API_key = "PLSwbTD2uH299JqlYJ33gP6LD"

twitter_API_secret_key = "8w6tnzmK6zyOvkSlRaMgu0RXzsTHJ5X7hmFlaMlfqf8DhdCNQS"

twitter_access_token = "1323122806733762560-7MO3tDQ1HmwHAM1e1jqXIjCpWTtEJU"


twitter_access_secret_token = "Cd4dwHq1UONLlmZPb4DxAyZcA0LdacdlZFVknsARaBXAW"

setup_twitter_oauth(consumer_key = twitter_API_key,
                    consumer_secret = twitter_API_secret_key,
                    access_token = twitter_access_token,
                    access_secret = twitter_access_secret_token)

library(twitteR)
library(plyr)
library(stringr)
#source("../../../my_twitter_oauth.R")

tweets = searchTwitter("starbucks", n=1000, lang="en")
class(tweets)

head(tweets)                  

# get the text
tweets_txt = sapply(tweets, function(x) x$getText())
starbucks.score = score.sentiment(tweets_txt, pos.words,
                                  neg.words)

#score.sentiment 함수 만들기

table(starbucks.score$score)
mean(starbucks.score$score)
#install.packages("ggplot2")
library(ggplot2)
qplot(starbucks.score$score)