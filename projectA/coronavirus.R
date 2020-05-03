library(shiny)
library(shinybusy)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(tibble)

# get variables for shiny app
source("searches.R")
source("functions.R")

# define ui
ui <- fluidPage(
  titlePanel("Tweets about Coronavirus"),
  sidebarLayout(
    sidebarPanel(
      selectInput("hashtag", "Pick a hashtag",
                  c("#COVID",
                    "#COVID19",
                    "#COVIDー19",
                    "#COVID_19"),
                  selected = "COVID"),
      
      
      actionButton("pull", "New tweets"),
      p("(Click button to pull new tweets!)"),
      
      p('.'),
      p(' The most recent 500 tweets were pulled for each hashtag. '), 
      p(' Tweets were processed in the following order:'),
      p(' - Unicode was removed'),
      p(' - Created corpus'),
      p(' - URLs were removed'),
      p(' - HTML converted into text'),
      p(' - Tags were removed'),
      p(' - Abbreviations and contractions were replaced'),
      p(' - All words were converted to lowercase'),
      p(' - All numbers were removed'),
      p(' - All punctuation was removed'),
      p(' - English stop words were removed, along with the following common terms 
        in virtually all tweets: "covid", "covid19", "covid-19", "covid_19", 
        "covid__19", "also", "can", "like", "coronavirus", "corona virus" '),
      p(' - Excessive whitespace was removed'),
      p(' - Words were lemmatized'),
      p(' - DTM was created with unigrams and bigrams'),
      p(' - Sparse terms were removed at .97')
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Wordclouds", plotOutput("wordcloud")),
        tabPanel("Top 20 Bargraph", plotOutput("bargraph")),
        tabPanel("Shared Terms", tableOutput("shared"))
      )
    )
  )
)

# define server
server = function(input, output) {
  output$wordcloud = renderPlot({
    if(input$hashtag == "#COVID") {
      tweets_cp = process(tweets_covid$text)
      tweets_tbl = get_tbl(tweets_cp)
      wordcloud(tweets_tbl$words, tweets_tbl$freq, 
                max.words = 50, colors = "tomato",
                main = "50 most popular words",
                scale = c(4,.25),
                random.order = F,
                rot.per = 0)
    } else if(input$hashtag == "#COVID19") {
      tweets_cp = process(tweets_covid19$text)
      tweets_tbl = get_tbl(tweets_cp)
      wordcloud(tweets_tbl$words, tweets_tbl$freq, 
                max.words = 50, colors = "slateblue",
                main = "50 most popular words",
                scale = c(4,.25),
                random.order = F,
                rot.per = 0)
    } else if(input$hashtag == "#COVIDー19") {
      tweets_cp = process(tweets_covid.19$text)
      tweets_tbl = get_tbl(tweets_cp)
      wordcloud(tweets_tbl$words, tweets_tbl$freq, 
                max.words = 50, colors = "royalblue",
                main = "50 most popular words",
                scale = c(4,.25),
                random.order = F,
                rot.per = 0)
    } else if(input$hashtag == "#COVID_19") {
      tweets_cp = process(tweets_covid_19$text)
      tweets_tbl = get_tbl(tweets_cp)
      wordcloud(tweets_tbl$words, tweets_tbl$freq, 
                max.words = 50, colors = "purple3",
                main = "50 most popular words",
                scale = c(4,.25),
                random.order = F,
                rot.per = 0)
    }
  })
  
  observeEvent(input$pull, {
    show_modal_spinner()
    source("searches.R")
    remove_modal_spinner()
  })
  
  output$bargraph = renderPlot({
    # top 20 terms across hastags
    covid_cp = process(covid_tbl$text)
    top_tbl = get_tbl(covid_cp)
    top_20 = top_tbl[1:20,]
    ggplot(top_20, aes(x = reorder(words, freq), y = freq)) + 
      geom_col() + 
      coord_flip() +
      labs(title = "Top 20 terms across hashtags",
           x = "terms",
           y = "raw count")
  })
  
  output$shared = renderTable({
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
    shared
  })
  
}

# run app
shinyApp(ui = ui, server = server)
#seabass.shinyapps.io/projectA