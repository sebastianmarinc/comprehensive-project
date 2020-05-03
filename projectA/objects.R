source("searches.R")
source("functions.R")

tweets_covid$group = rep("covid", nrow(tweets_covid))
tweets_covid19$group = rep("covid19", nrow(tweets_covid19))
tweets_covid.19$group = rep("covid.19", nrow(tweets_covid.19))
tweets_covid_19$group = rep("covid_19", nrow(tweets_covid_19))
covid_tbl = rbind(tweets_covid, tweets_covid19, tweets_covid.19, tweets_covid_19)
covid_cp = process(covid_tbl$text)
top_tbl = get_tbl(covid_cp)
top_20 = top_tbl[1:20,]
tweets_cp1 = process(tweets_covid$text)
tweets_tbl1 = get_tbl(tweets_cp1)
tweets_cp2 = process(tweets_covid19$text)
tweets_tbl2 = get_tbl(tweets_cp2)
tweets_cp4 = process(tweets_covid_19$text)
tweets_tbl4 = get_tbl(tweets_cp4)
tweets_cp3 = process(tweets_covid.19$text)
tweets_tbl3 = get_tbl(tweets_cp3)
all_covid = paste(tweets_covid$text, collapse = "")
all_covid19 = paste(tweets_covid19$text, collapse = "")
all_covid.19 = paste(tweets_covid.19$text, collapse = "")
all_covid_19 = paste(tweets_covid_19$text, collapse = "")
all_tweets = c(all_covid, all_covid19, 
               all_covid.19, all_covid_19)
all_cp = process(all_tweets)
all_tdm = tdm(all_cp)
colnames(all_tdm) = c("COVID","COVID19", 
                      "COVIDー19", "COVID_19")
all_m = as.matrix(all_tdm)

shared = tibble(
  hashtag_comparison = c("COVID - COVID19",
                         "COVID - COVIDー19",
                         "COVID - COVID_19",
                         "COVID19 - COVIDー19",
                         "COVID19 - COVID_19",
                         "COVIDー19 - COVID_19"),
  shared_terms = c(
    nrow(subset(all_m, all_m[, 1] > 5 & all_m[, 2] > 5)),
    nrow(subset(all_m, all_m[, 1] > 5 & all_m[, 3] > 5)),
    nrow(subset(all_m, all_m[, 1] > 5 & all_m[, 4] > 5)),
    nrow(subset(all_m, all_m[, 2] > 5 & all_m[, 3] > 5)),
    nrow(subset(all_m, all_m[, 2] > 5 & all_m[, 4] > 5)),
    nrow(subset(all_m, all_m[, 3] > 5 & all_m[, 4] > 5))
  )
)

