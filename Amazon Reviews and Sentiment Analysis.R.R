library(rvest)
library(XML)
library(magrittr)

# Amazon Reviews for Apple-iPhone-11-Pro-512GB

aurl <- "https://www.amazon.in/Apple-iPhone-11-Pro-512GB/product-reviews/B00MITLPX2/ref=cm_cr_arp_d_paging_btm_next_2?showViewpoints=1&pageNumber="
amazon_reviews <- NULL
for (i in 1:30){
  murl <- read_html(as.character(paste(aurl,i,sep="")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"AppleiPhone11Pro512GB.txt",row.names=FALSE)
getwd()

# Sentiment Analysis using word cloud

txt <- amazon_reviews

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
x1 <- tm_map(x, tolower) # Converts all letters into lower case
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

# Term one repeats more than or equal to 60 times which doesnt add any value to analysis and 
# its not a familiar word compare the product which will not have impact on accuracy of the model
x1 <- tm_map(x1, removeWords,c('one'))
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)
tdm

tdm <- as.matrix(tdm)
tdm[100:109 , 1:20]

# Bar Plot after removing "one" word

w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 50)
w_sub

barplot(w_sub, las=2, col=rainbow(30))

# Term "using" and "also" repeats more than or equal to 50 times which doesnt add any value to analysis and 
# its not a familiar word compare the product which will not have impact on accuracy of the model
x1 <- tm_map(x1, removeWords,c('using','also'))
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)
tdm

tdm <- as.matrix(tdm)
tdm[100:109 , 1:20]

# Bar Plot after removing "using" & "also" word

w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 50)
w_sub

barplot(w_sub, las=2, col=rainbow(30))


## Word Could ##
install.packages("wordcloud")
library(wordcloud)

wordcloud(words = names(w_sub), freq = w_sub)

w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
head(w_sub1)
wordcloud(words = names(w_sub1), freq = w_sub1) # all words are considered


# Better Visualization
wordcloud(words = names(w_sub1), freq = w_sub1, random.order = F, colors = rainbow(30), scale = c(2,0.5), rot.per = 0.4)

wordcloud(words = names(w_sub1), freq = w_sub1, random.order = F, colors = rainbow(30), scale = c(3,0.5), rot.per = 0.3)


### Word Cloud2 ###
install.packages("wordcloud2")
library(wordcloud2)

w1 <- data.frame(names(w_sub),w_sub)
colnames(w1) <- c('word' , 'freq')

wordcloud2(w1, size=0.5, shape = 'circle')
wordcloud2(w1, size=0.5, shape = 'triangle')
wordcloud2(w1, size=0.5, shape = 'star')


## Bigram ##
install.packages("rJava")
install.packages("RWeka")
library(RWeka)
library(wordcloud)

minfreq_bigram <- 2
bitoken <- NGramTokenizer(x1,Weka_control(min = 2 , max = 2))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq, decreasing = TRUE),]

wordcloud(sort_two$bitoken, sort_two$Freq, random.order = F, scale = c(2, 0.35), min.freq = minfreq_bigram , colors = brewer.pal(8, "Dark2"), max.words = 150)
windows()

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
