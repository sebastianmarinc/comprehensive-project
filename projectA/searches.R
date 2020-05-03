library(rtweet)

# get secret keys
source("secrets.R")

# api access 
api = create_token(
  app = app,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret
)

# search tweets for COVID
tweets_covid = search_tweets(
  "#COVID", n = 500, include_rts = F, lang = "en"
)
tweets_covid$group = rep("covid", nrow(tweets_covid))

# search tweets for COVID19
tweets_covid19 = search_tweets(
  "#COVID19", n = 500, include_rts = F, lang = "en"
)
tweets_covid19$group = rep("covid19", nrow(tweets_covid19))

# search tweets for COVID-19
tweets_covid.19 = search_tweets(
  "#COVIDー19", n = 500, include_rts = F, lang = "en"
)
tweets_covid.19$group = rep("covid.19", nrow(tweets_covid.19))

# search tweets for COVID_19
tweets_covid_19 = search_tweets(
  "#COVID_19", n = 500, include_rts = F, lang = "en"
)
tweets_covid_19$group = rep("covid_19", nrow(tweets_covid_19))

# combine tibbles 
covid_tbl = rbind(tweets_covid, tweets_covid19, tweets_covid.19, tweets_covid_19)
