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

# search tweets for COVID19
tweets_covid19 = search_tweets(
  "#COVID19", n = 500, include_rts = F, lang = "en"
)

# search tweets for COVID-19
tweets_covid.19 = search_tweets(
  "#COVIDー19", n = 500, include_rts = F, lang = "en"
)

# search tweets for COVID_19
tweets_covid_19 = search_tweets(
  "#COVID_19", n = 500, include_rts = F, lang = "en"
)
