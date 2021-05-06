setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
library(tm)
library(stringr)
library(quanteda)
library(tosca)
library("wordcloud")

url <- "https://osbcontent.s3-eu-west-1.amazonaws.com/PC+Appendix+2021-001-FB-FBR.pdf"

download.file(url, 'public_comments.pdf', mode="wb")

pdf <- pdftools::pdf_text(pdf = "public_comments.pdf")

# get "full comments"
full_comments <- str_match_all(pdf, "(?s)Full Comment\\s*(.*?)\\s*Link to Attachment")

full_comments_vec <- c()

for (i in 1:length(full_comments)){
  full_comments_vec[i] <- full_comments[[i]][2]
}

# delet NAs
full_comments_vec <- full_comments_vec[!is.na(full_comments_vec)]

# Preprocessing
full_comments_vec <- tolower(full_comments_vec)
full_comments_vec <- removePunctuation(full_comments_vec)

# identify important bi and trigrams
corpus <- corpus(full_comments_vec)
tokenized_corpus <- tokens(corpus) 
tokenized_corpus_bigram <-tokens_ngrams(tokenized_corpus, n=2)
tokenized_corpus_trigram <-tokens_ngrams(tokenized_corpus, n=3)

topfeatures(dfm(tokenized_corpus_bigram), 50) 
topfeatures(dfm(tokenized_corpus_trigram), 50) 

# imporatant collocations 2 token
full_comments_vec <- gsub("donald trump", "donald_trump", full_comments_vec)
full_comments_vec <- gsub("social media", "social_media", full_comments_vec)
full_comments_vec <- gsub("united states", "united_states", full_comments_vec)
full_comments_vec <- gsub("free speech", "free_speech", full_comments_vec)
full_comments_vec <- gsub("first amendment", "first_amendment", full_comments_vec)
full_comments_vec <- gsub("freedom of speech", "freedom_of_speech", full_comments_vec)
full_comments_vec <- gsub("donald j trump ", "donald_j_trump", full_comments_vec)
full_comments_vec <- gsub("january 6", "january_6 ", full_comments_vec)
full_comments_vec <- gsub("maxine waters", "maxine_waters ", full_comments_vec)

df <- data.frame("id"=1:length(full_comments_vec), "text"=full_comments_vec)

write.csv(df, file="comments.csv")

cols=c("id", "text")

# textmeta from csv
textmeta <- readTextmeta(path=".", file="comments.csv", cols= cols, dateFormat =
                           "%Y-%m-%d",idCol = "id", textCol = "text", encoding = "") 

# preprocessing with tosca

corpusClean <- cleanTexts(object = textmeta, sw = c(tm::stopwords("en"),"said", "will", "us", "new", "also", "see"), rmNumbers= F, checkUTF8=F)

# create wordtable
wordtable <- makeWordlist(corpusClean)

# freq analysis

head(sort(wordtable$wordtable, decreasing = TRUE), 100)

# eliminate rare words

words5 <- wordtable$words[wordtable$wordtable > 5]

# LDA prep and gen

pagesLDA <- LDAprep(text = corpusClean$text, vocab = words5)

result <- LDAgen(documents = pagesLDA, K = 5L, vocab = words5, seed = 123, folder=".") # K 10, alpha=eta qua default 1/k
result <- LDAgen(documents = pagesLDA, K = 10L, vocab = words5, seed = 123, folder=".")
result <- LDAgen(documents = pagesLDA, K = 15L, vocab = words5, seed = 123, folder=".")
result <- LDAgen(documents = pagesLDA, K = 20L, vocab = words5, seed = 123, folder=".")

# load LDA result

load(file.path(dirname(rstudioapi::getActiveDocumentContext()$path),".-k15alpha0.07eta0.07i200b70s123.RData"))

# list with topwords
topWords <- topWords(result$topics, numWords=100, values=TRUE)

# create wordcloud for 15 topics

for (i in 1:15){
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.04, paste("Topic", i, sep=" "))
png(file= paste("Topic", i, ".png", sep=" "), width=911, height=578)
wordcloud(words = topWords$word[,i], freq = topWords$val[,i], min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"), main="Title")
dev.off()
}

