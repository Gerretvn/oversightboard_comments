setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

library(tm)
library(stringr)
library(quanteda)
library(tosca)
library(wordcloud)
library(pdftools)

url <- "https://osbcontent.s3-eu-west-1.amazonaws.com/PC+Appendix+2021-001-FB-FBR.pdf"

download.file(url, 'public_comments.pdf', mode="wb")

pdf <- pdf_text(pdf = "public_comments.pdf")

# get "short comments"
short_comments <- str_match_all(pdf, "(?s)Short summary provided by the commenter\\s*(.*?)\\s*Full Comment")
short_comments_vec <- c()

for (i in 1:length(short_comments)){
  short_comments_vec[i] <- short_comments[[i]][2]
}

short_comments_vec <- short_comments_vec[!is.na(short_comments_vec)] # delet NAs

# get "full comments"
pdf_collapse <- paste(pdf,collapse="")
pdf_split <- unlist(strsplit(pdf_collapse, split='Link to Attachment', fixed=TRUE))
full_comments_vec <- sub(".*Full Comment", "", pdf_split) # 7608

# problem: in some cases the full comment says "see above" or similar - solution: 
# compare full and short comments - if short > full, choose short
text <- c()
for (i in 1:7607){
  if (ntoken(full_comments_vec[i]) > ntoken(short_comments_vec[i])){
    text[i] <- full_comments_vec[i]
  } else {
    text[i] <- short_comments_vec[i]
  }
}

# Preprocessing
text <- tolower(text)
text <- removePunctuation(text)

# remove boilerplate
text <- gsub(" public comments appendix for oversight board case 2021001fbfbr", " ", text)
text <- gsub("public comments appendix for oversightboard case 2021001fbfbr", " ", text)

# identify important bi and trigrams
corpus <- corpus(text)
tokenized_corpus <- tokens(corpus) 
tokenized_corpus_bigram <-tokens_ngrams(tokenized_corpus, n=2)
tokenized_corpus_trigram <-tokens_ngrams(tokenized_corpus, n=3)

topfeatures(dfm(tokenized_corpus_bigram), 100) 
topfeatures(dfm(tokenized_corpus_trigram), 100) 

# important collocations 2 token
text <- gsub("oversight board", "oversight_board", text)
text <- gsub("donald trump", "donald_trump", text)
text <- gsub("social media", "social_media", text)
text <- gsub("united states", "united_states", text)
text <- gsub("free speech", "free_speech", text)
text <- gsub("first amendment", "first_amendment", text)
text <- gsub("freedom of speech", "freedom_of_speech", text)
text <- gsub("donald j trump ", "donald_j_trump", text)
text <- gsub("january 6", "january_6 ", text)
text <- gsub("maxine waters", "maxine_waters ", text)

# remove persistent punctuation
text <- gsub("’", "", text)
text <- gsub("€", "", text)

df <- data.frame("id"=1:length(text), "text"=text)

write.csv(df, file="Comments_all.csv")

cols=c("id", "text")

# textmeta from csv
textmeta <- readTextmeta(path=".", file="Comments_all.csv", cols= cols, dateFormat =
                           "%Y-%m-%d",idCol = "id", textCol = "text", encoding = "") 

# preprocessing with tosca
corpusClean <- cleanTexts(object = textmeta, sw = c(tm::stopwords("en"),"said", "will", "us", "new", "also", "see"), checkUTF8=F)

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

