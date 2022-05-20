library(rvest)
library(XML)
library(magrittr)

# IMDB "FORREST GUMP" Movie Reviews #############################
url <- "https://www.imdb.com/title/tt0109830/reviews?ref_=tt_ql_3"
IMDB_reviews <- NULL
for (i in 1:30){
  murl <- read_html(as.character(paste(url,i,sep="")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
write.table(IMDB_reviews,"FORREST GUMP.txt",row.names=FALSE)
getwd()

# Sentiment Analysis using word colud

txt <- IMDB_reviews

str(txt)
length(txt)
View(txt)

install.packages("tm")
library(tm)

# Converting the character data corpus type
x <- Corpus(VectorSource(txt))

inspect(x[1])

x <- tm_map(x, function(x) iconv(enc2utf8(x), sub='byte'))

# Data cleansing
x1 <- tm_map(x, tolower)
inspect(x1[1])

x1 <- tm_map(x1, removePunctuation)
inspect(x1[1])

x1 <- tm_map(x1, removeNumbers)
inspect(x1[1])

x1 <- tm_map(x1, removeWords, stopwords('english'))
inspect(x1[1])

# Striping White Spaces
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])

# Term Document Matrix
# Converting Unstructed data to structed format using DTM

#tdm <- TermDocumentMatrix(x1)
#tdm
#dtm <- t(tdm) # transpose

dtm <- DocumentTermMatrix(x1)
tdm <- t(dtm)

# To Remove spares entries upon a specific value
#corpus.dtm.frequent <- removeSparseTerms(tdm,0.99)

tdm <- as.matrix(tdm)
dim(tdm)

tdm[1:20 , 1:20]
inspect(x[1])

# Bar Plot

w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 60)
w_sub

barplot(w_sub, las=2, col=rainbow(30))

# Term like didnt, isnt etc repeats more than or equal to 60 times which doesnt add any value to analysis
x1 <- tm_map(x1, removeWords,c('didnt','isnt','many', 'says', 'others', 'along', 'problem', 'ways', 'slow' ))
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)
tdm

tdm <- as.matrix(tdm)
tdm[100:120 , 1:20]

# Bar Plot after removing terms like didnt, isnt etc.,

w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 150)
w_sub

barplot(w_sub, las=2, col=rainbow(30))

# Term like let,little etc repeats more than or equal to 150 times which doesnt add any value to analysis
x1 <- tm_map(x1, removeWords,c('get','let','little', 'films', 'will', 'think', 'man', 'like', 'give', 'thinking', 'much', 'first', 'lives', 'make', 'saw', 'feel' ))
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)
tdm

tdm <- as.matrix(tdm)
tdm[100:120 , 1:20]

# Bar Plot after removing terms like get, let etc., 

w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 80)
w_sub

barplot(w_sub, las=2, col=rainbow(30))

## Word Could ##
install.packages("wordcloud")
library(wordcloud)

wordcloud(words = names(w_sub), freq = w_sub)

w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
head(w_sub1)


wordcloud(words = names(w_sub1), freq = w_sub1) # all words are considered

# Better Visulization
wordcloud(words = names(w_sub1), freq = w_sub1, random.order = F, colors = rainbow(30), scale = c(2,0.5), rot.per = 0.4)
windows()
wordcloud(words = names(w_sub1), freq = w_sub1, random.order = F, colors = rainbow(30), scale = c(3,0.5), rot.per = 0.3)


### Word Cloud2 ###
install.packages("wordcloud2")
library(wordcloud2)

w1 <- data.frame(names(w_sub),w_sub)
colnames(w1) <- c('word' , 'freq')

wordcloud2(w1, size=0.5, shape = 'circle')
wordcloud2(w1, size=0.4, shape = 'triangle')
wordcloud2(w1, size=0.4, shape = 'star')

## Bigram ##
install.packages("RWeka")
library(RWeka)
library(wordcloud)

minfreq_bigram <- 2
bitoken <- NGramTokenizer(x1,Weka_control(min = 2 , max = 2))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq, decreasing = TRUE),]

wordcloud(sort_two$bitoken, sort_two$Freq, random.order = F, scale = c(2, 0.35), min.freq = minfreq_bigram , colors = brewer.pal(8, "Dark2"), max.words = 150)

## Loading Positive and Negative words

pos.words <- readLines(file.choose())
neg.words <- readLines(file.choose())
stopwords <- readLines(file.choose())

## Positive word cloud
pos.matches <- match(names(w_sub1), pos.words)
pos.matches <- !is.na(pos.matches)
freq_pos <- w_sub1[pos.matches]
names <- names(freq_pos)
windows()
wordcloud(names, freq_pos, scale=c(4, 0.5), colors = brewer.pal(8, "Dark2"))

## Negative word cloud
neg.matches <- match(names(w_sub1), neg.words)
neg.matches <- !is.na(neg.matches)
freq_neg <- w_sub1[neg.matches]
names <- names(freq_neg)
windows()
wordcloud(names, freq_neg, scale=c(4, 0.5), colors = brewer.pal(8, "Dark2"))
