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

# Read the data
csv.file = read.csv(file.choose(), header=TRUE)
str(csv.file)
tweets <- iconv(csv.file$text, to="utf-8")
tweets

# Build corpus
corpus <- iconv(csv.file$text, to = "utf-8") #converting encoding
corpus <- Corpus(VectorSource(corpus))

# Clean Data
corpus <- tm_map(corpus, tolower) #transform into lower case
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
cleanset <- tm_map(corpus, removeWords, stopwords(kind = "en"))
removeURL <- function(x) gsub(" http.*","",x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple', 'apples'))
cleanset <- tm_map(cleanset, gsub, 
                   pattern = 'iphones',
                   replacement = 'iphone'
                   )
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:10])

# Term-document Matrix
tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)

# Bar Plot
w <- rowSums(tdm)
w <- subset(w, w>=25)
png(file='Output/trending_words_bar.png')
barplot (w,
         las = 2, 
         col = rainbow(50))
dev.off()

# Word Cloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(100)
png(file='Output/trending_word1.png')
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

w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
head(w)

png(file='Output/trending_word2.png')
wordcloud2(
  w,
  size = 0.8,
  shape = 'circle',
  minSize = 0.1)
letterCloud(w,
            word = 'a',
            size = 1)
dev.off()

# Obtain sentiment scores
sentiment <- get_nrc_sentiment(tweets)
head(sentiment)

# Viz
png(file="Output/saving_plot2.png")
barplot (
  colSums(sentiment),
  col = rainbow(10)
)
dev.off()
