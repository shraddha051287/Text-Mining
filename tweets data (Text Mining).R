library("twitteR")
#install.packages("ROAuth")
library("ROAuth")

cred <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', # Consumer Key (API Key)
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

#install.packages("base64enc")
library(base64enc)

#install.packages("httpuv")
library(httpuv)

setup_twitter_oauth("FXTquJNbgDG2dH81XYVqNZFAb", # Consumer Key (API Key)
                    "3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO", #Consumer Secret (API Secret)
                    "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh",  # Access Token
                    "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('akshaykumar', n = 500,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)

write.csv(TweetsDF, "Tweets.csv",row.names = F)

getwd()


# Importing Amazon reviews data
txt <- read.csv(file.choose(), header = TRUE)

str(txt)
View(txt)
length(txt)

x <- as.character(txt$text)
str(x)
length(x)

# Corpus
#install.packages("tm")
library(tm)

# x = stemDocument(x, language = "english")
# ?stemDocument

x <- Corpus(VectorSource(x))

inspect(x[1])
inspect(x[500])

?tm_map

# Data Cleansing
x1 <- tm_map(x, tolower)
inspect(x1[1])

x1 <- tm_map(x1, removePunctuation)
inspect(x1[1])
inspect(x1[500])

x1 <- tm_map(x1, removeNumbers)
inspect(x1[500])

x1 <- tm_map(x1, removeWords, stopwords('english'))
inspect(x1[1])

#striping white spaces
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])

# Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)
tdm[["nrow"]]
tdm[["ncol"]]
dtm <- t(tdm)

tdm <- as.matrix(tdm)
tdm[100:109, 1:10]

tdm[1:20, 1:20]

# Read the third review
inspect(x[3])

# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 30)
w_sub

barplot(w_sub, las=3, col = rainbow(20))

# Term phone and oneplus repeats in all most all documents
x1 <- tm_map(x1, removeWords, 'uffuffb')
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)
tdm

tdm <- as.matrix(tdm)
tdm[100:109, 1:20]

# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 30)
w_sub

barplot(w_sub, las=3, col = rainbow(20))

# Word cloud
#install.packages("wordcloud")
library(wordcloud)
windows()
wordcloud(words = names(w_sub), freq = w_sub) # wordcloud with only subset of words

w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
windows()
wordcloud(words = names(w_sub1), freq = w_sub1) # all words are considered

windows()
wordcloud(words = names(w_sub1), freq = w_sub1, random.order = F, colors = rainbow(20), scale=c(3,1), rot.per = 0.3)
?wordcloud

# lOADING +VE AND -VE dictonaries
pos.words = scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words = scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
pos.words = c(pos.words,"wow", "kudos", "hurray") # including our own positive words to the existing list

# Positive wordcloud
pos.matches = match(names(w_sub1), c(pos.words))
pos.matches = !is.na(pos.matches)
freq_pos <- w_sub1[pos.matches]
p_names <- names(freq_pos)
windows()
wordcloud(p_names,freq_pos,scale=c(4,1),colors = rainbow(20))

# Negative wordcloud
neg.matches = match(names(w_sub1), c(neg.words))
neg.matches = !is.na(neg.matches)
freq_neg <- w_sub1[neg.matches]
n_names <- names(freq_neg)
windows()
wordcloud(n_names,freq_neg,scale=c(5,1),colors = brewer.pal(8,"Dark2"))


#######################################
# Advance options

# wordcloud2 - shapes for word cloud
w
w_small <- subset(w, w >= 10)
w_small

barplot(w_small, las=2, col = rainbow(30))

#install.packages("wordcloud2")
library(wordcloud2)

w1 <- data.frame(names(w_small), w_small)
colnames(w1) <- c('word', 'freq')

wordcloud2(w1, size=0.5, shape='circle')
?wordcloud2

wordcloud2(w1, size=0.5, shape = 'triangle')

######

# Letter cloud

letterCloud(w1, word = "akshaykumar", wordSize = 0.5)
?letterCloud

#######

# install.packages("quanteda")
library(quanteda)
?quanteda
# n-gram document term frequency 
?dfm

dtm0_2 <- dfm(unlist(x1),ngrams=2,verbose = F)
tdm0_2 <- t(dtm0_2)

a0 = NULL
for (i1 in 1:ncol(tdm0_2)){ if (sum(tdm0_2[, i1]) == 0) {a0 = c(a0, i1)} }
length(a0)		# no. of empty docs in the corpus
if (length(a0) >0) { tdm0_2 = tdm0_2[, -a0]} else {tdm0_2 = tdm0_2};	dim(tdm0_2)	# under TF weighing
a0 <- NULL;i1 <- NULL

dtm0_2 <- t(tdm0_2)

##### Custom function ####
makewordc = function(x){	
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  windows()
  wordcloud(freq.df$word[1:120], freq.df$freq[1:120],scale = c(6,1),random.order = F, colors=1:10)
} 

# Bi gram word cloud
makewordc(tdm0_2) # We have too see warnings to edit few words
wordcloud(words = names(w_sub1), freq = w_sub1, random.order = F, colors = rainbow(30), scale=c(3,1), rot.per = 0.3)

?makeword
title(sub = "BIGRAM - Wordcloud using TF")


# Bar plot of words and their frequency
words_bar_plot <- function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  head(freq.df, 20)
  library(ggplot2)
  windows()
  ggplot(head(freq.df,50), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Words") + ylab("Frequency") +
    ggtitle("Most frequent words")
}

# Bi gram barplot on TF
words_bar_plot(tdm0_2)

## Bi gram on TFIDF

dtm1_2 <- dfm_tfidf(dtm0_2) #Warning message:'tfidf' is deprecated.Use 'dfm_tfidf' instead.See help("Deprecated") 
tdm1_2 <- t(dtm1_2)
a0 = NULL
for (i1 in 1:ncol(tdm1_2)){ if (sum(tdm1_2[, i1]) == 0) {a0 = c(a0, i1)} }
length(a0)		# no. of empty docs in the corpus
if (length(a0) >0) { tdm1_2 = tdm1_2[, -a0]} else {tdm1_2 = tdm1_2};	dim(tdm1_2)	# under TF weighing
a0 <- NULL;i1 <- NULL
dtm1_2 <- t(tdm1_2)

# Bi gram word cloud for TFIDF
makewordc(tdm1_2) # We have too see warnings to edit few words
title(sub = "BIGRAM - Wordcloud using dfm_tfidf")

# Bigram barplot on TFIDF
words_bar_plot(tdm1_2)

#########################################################################
