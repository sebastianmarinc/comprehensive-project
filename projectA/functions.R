library(RWeka)
library(textclean)
library(tm)
library(qdap)
library(textstem)
library(stringr)
library(tibble)

# preprocessing function 
process = function(x) { 
  if(class(x) != "character") {
    print("input should be a vector of tweets")
  } else {
    # process tweets
    x = iconv(x, "UTF-8","ASCII",sub="") #remove unicode, ascii 
    tweets_cp = VCorpus(VectorSource(x)) #creating corpus
    tweets_cp = tm_map(tweets_cp, content_transformer(rm_url)) #removing any url's
    tweets_cp = tm_map(tweets_cp, content_transformer(replace_html)) #converting html text into punctuation
    tweets_cp = tm_map(tweets_cp, content_transformer(function(x) #removing @ tags
      str_remove_all(x, pattern = "@\\w*")))
    tweets_cp = tm_map(tweets_cp, content_transformer(replace_abbreviation)) #expanding abbreviations
    tweets_cp = tm_map(tweets_cp, content_transformer(replace_contraction)) #expanding contractions
    tweets_cp = tm_map(tweets_cp, content_transformer(str_to_lower)) #all lower case
    tweets_cp = tm_map(tweets_cp, removeNumbers) #removing numbers
    tweets_cp = tm_map(tweets_cp, removePunctuation) #removing punctuation
    tweets_cp = tm_map(tweets_cp, removeWords, c(stopwords("en"), #removing non-unique terms, uninteresting terms
                                                 "covid","covid19","covid-19",
                                                 "covid_19","covid__19",
                                                 "also","can","like","coronavirus",
                                                 "corona virus")) 
    tweets_cp = tm_map(tweets_cp, stripWhitespace) #removing execessive space
    tweets_cp = tm_map(tweets_cp, content_transformer(lemmatize_strings)) #creating lemmas
  }
}

# create tokenizer function from lemmas for word cloud
tokenizer = function(x){ #create tokenizer
  NGramTokenizer(x, Weka_control(min = 1, max = 2))
} 

# function to produce DTM, then matrix, then tibble
get_tbl = function(x, sparsity = .97) {
  if(class(x)[1] != "VCorpus"){
    print("input should be a corpus of tweets")
  } else {
  tweets_dtm = DocumentTermMatrix(x, control = list(tokenize = tokenizer)) #get unigrams and bigrams
  tweets_dtm = removeSparseTerms(tweets_dtm, sparsity) #removing sparse terms from DTM
  tweets_m = as.matrix(tweets_dtm) #creating data matrix from DTM after sparse terms were removed
  tweets_tbl = as_tibble(tweets_m)
  term_freq = sort(colSums(tweets_tbl), decreasing = T) #create term frequency vector
  tweets_freq = tibble(words = names(term_freq), freq = term_freq) #create tibble with words and frequencies for wordcloud
  } 
}

tdm = function(x, sparsity = .97) {
  if(class(x)[1] != "VCorpus"){
    print("input should be a corpus of tweets")
  } else {
    tweets_tdm = TermDocumentMatrix(x, control = list(tokenize = tokenizer)) #get unigrams and bigrams
    tweets_tdm = removeSparseTerms(tweets_tdm, sparsity) #removing sparse terms from DTM
  }
}

