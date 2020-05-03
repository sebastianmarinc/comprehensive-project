library(shiny)
library(shinybusy)
library(wordcloud)
library(ggplot2)

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
      
      
      actionButton("pull", "Pull new tweets"),
    
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
      wordcloud(tweets_tbl1$words, tweets_tbl1$freq, 
                max.words = 50, colors = "tomato",
                main = "50 most popular words",
                scale = c(4,.25),
                random.order = F,
                rot.per = 0)
    } else if(input$hashtag == "#COVID19") {
      wordcloud(tweets_tbl2$words, tweets_tbl2$freq, 
                max.words = 50, colors = "slateblue",
                main = "50 most popular words",
                scale = c(4,.25),
                random.order = F,
                rot.per = 0)
    } else if(input$hashtag == "#COVIDー19") {
      wordcloud(tweets_tbl3$words, tweets_tbl3$freq, 
                max.words = 50, colors = "royalblue",
                main = "50 most popular words",
                scale = c(4,.25),
                random.order = F,
                rot.per = 0)
    } else if(input$hashtag == "#COVID_19") {
      wordcloud(tweets_tbl4$words, tweets_tbl4$freq, 
                max.words = 50, colors = "purple3",
                main = "50 most popular words",
                scale = c(4,.25),
                random.order = F,
                rot.per = 0)
    }
  })
  
  observeEvent(input$pull, {
    show_modal_spinner()
    source("objects.R")
    remove_modal_spinner()
  })
  
  output$bargraph = renderPlot({
    # top 20 terms across hastags
    ggplot(top_20, aes(x = reorder(words, freq), y = freq)) + 
      geom_col() + 
      coord_flip() +
      labs(title = "Top 20 terms across hashtags",
           x = "terms",
           y = "raw count")
  })
  
  output$shared = renderTable({
    shared
  })
  
}

# run app
shinyApp(ui = ui, server = server)
#seabass.shinyapps.io/projectA