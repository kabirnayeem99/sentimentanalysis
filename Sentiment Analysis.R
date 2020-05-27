# Load the necessary libraries
library('tm')  # Package for Text Mining
library('lubridate')  # Makes it easier to work with dates and times
library('dplyr')  # Grammar of data manipulation
library('ggplot2')  # System for declaratively creating graphics
library('scales')  # Converting from data values to perceptual properties
library('reshape2')  # Flexibly restructure and aggregate data
library('syuzhet')  # Extracts sentiment and sentiment-derived plot arcs from text
library('wordcloud')  # Keywords as Word Cloud Viz
library('wordcloud2')  # Enhanced Wordcloud using Data Frame

# Functions

## Read data
ReadData <- function() {
  csv.file = read.csv(file.choose(), header = TRUE)
  tweets <- iconv(csv.file$text, to = "utf-8")
}

## Build Corpus
BuildCorpus <- function(file) {
  corpus <- iconv(file$text, to = "utf-8") #converting encoding
  corpus <- Corpus(VectorSource(corpus))
}

# Remove URL using Regex
RemoveURL <- function(x) {
  gsub(" http.*", "", x)
}

## Clean Corpus
CleanCorpus <- function(corpus) {
  corpus <- tm_map(corpus, tolower) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  cleanset <- tm_map(corpus, removeWords, stopwords(kind = "en"))
  cleanset <- tm_map(cleanset, content_transformer(RemoveURL))
  cleanset <-
    tm_map(cleanset, removeWords, c('aapl', 'apple', 'apples'))
  cleanset <- tm_map(cleanset, gsub,
                     pattern = 'iphones',
                     replacement = 'iphone')
  cleanset <- tm_map(cleanset, stripWhitespace)
}

## Term-document Matrix
CreateTDMatrix <- function(cleanset) {
  tdm <- TermDocumentMatrix(cleanset)
  tdm <- as.matrix(tdm)
}

## Trending Words Bar
CreateTrendingBar <- function(tdm) {
  w <- rowSums(tdm)
  w <- subset(w, w >= 25)
  png(file = 'Output/trending_words_bar.png')
  barplot (w,
           las = 2,
           col = rainbow(50))
  dev.off()
}

## Word Cloud 1
CreateWordCloud <- function(tdm) {
  w <- sort(rowSums(tdm), decreasing = TRUE)
  set.seed(100)
  png(file = 'Output/trending_word1.png')
  wordcloud(
    words = names(w),
    freq = w,
    max.words = 150,
    random.order = FALSE,
    min.freq = 10,
    colors = brewer.pal(8, name = 'Dark2'),
    scale = c(5, 0.5)
  )
  dev.off()
}

## Word Cloud 2
CreateWordCloud2 <- function(tdm) {
  w <- data.frame(names(w), w)
  colnames(w) <- c('word', 'freq')
  head(w)
  
  png(file = 'Output/trending_word2.png')
  wordcloud2(w,
             size = 0.8,
             shape = 'circle',
             minSize = 0.1)
  letterCloud(w,
              word = 'a',
              size = 1)
  dev.off()
}

## Sentiment Scoring Bar Plot
CreateSentimentBar <- function(tweets) {
  # Obtain sentiment scores
  sentiment <- get_nrc_sentiment(tweets)
  # Viz
  png(file = "Output/sentiment_bar.png")
  barplot (colSums(sentiment),
           col = rainbow(10))
  dev.off()
}

# The main function
ReadData()
BuildCorpus(csv.file)
CleanCorpus(corpus)
CreateTDMatrix(cleanset)
CreateTrendingBar(tdm)
CreateWordCloud(tdm)
CreateSentimentBar(tweets)
