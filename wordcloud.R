install.packages("qdapRegex")
## load rtweet package
library(rtweet)
library(qdapRegex)
library(plyr) 
library(tidyverse) 
library(tm) 
library(ggplot2)
library(wordcloud)
#Remove characters functions
tweet.removeEmoji = function(x) gsub("\\p{So}|\\p{Cn}", "", x, perl = TRUE)
tweet.removeSpecialChar = function(x) gsub("[^[:alnum:]///' ]", "", x)
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

tweets$text <- rm_url(tweets$text)
tweets_cm$text <- rm_twitter_url(tweets_cm$text)
docs = Corpus(VectorSource(tweets_cm$text))

docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "`")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, removePunctuation)
docs = tm_map(docs, content_transformer(tweet.removeEmoji))
docs = tm_map(docs, content_transformer(tweet.removeSpecialChar))
docs <- tm_map(docs,content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeWords, "captainmarvel")
docs <- tm_map(docs, removeWords, "rt")

# Extra stop words from analysing frequencies after doing stem and culling DTM
myStopwords <- c('movie', 'amp', 'saw', 'time', 'film', 'really', 'watch', 'today', 'dont',
                 'got', 'didnt', 'cant', 'can', 'will', 'finally', 'going',
                 'new', 'wait', 'think', 'just', 'see', 'one', 'movies', 'still')
docs <- tm_map(docs, removeWords, myStopwords)

# convert corpus to Document Term Matrix
dtm <-DocumentTermMatrix(docs)

# collapse matrix by summing over columns and then ordering by frequency
freq <- colSums(as.matrix(dtm))

# setting the same seed each time ensures consistent look across clouds
set.seed(10)
# wordCloud with colour for 50 words
par(mar = rep(0, 4))
wordcloud(names(freq),freq,max.words=50,colors=brewer.pal(6,"Dark2"), 
          scale = c(4, 0.2),random.order=FALSE, rot.per=.15)