# Wesley Clark
# Text Mining
# Load the packages

install.packages("tm", lib="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
install.packages("wordcloud", lib="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
install.packages("fpc", lib="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
install.packages("cluster", lib="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")

install.packages("ggplot2", lib="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")


# Set WD and check directory

setwd("~/Downloads/Exercise1DataFiles")
dir(".")

library("wordcloud", lib.loc="~/Library/R/3.3/library")
library("fpc", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("cluster", lib.loc="~/Library/R/3.3/library")
library("ggplot2", lib.loc="~/Library/R/3.3/library")
library("SnowballC", lib.loc="~/Library/R/3.3/library")
library("tm", lib.loc="~/Library/R/3.3/library")

# Create a docs variable

docs <- Corpus(DirSource("."))
summary(docs)

# Remove the URLS and strip the white space

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
docs <-tm_map(docs, content_transformer(removeURL))
docs <-tm_map(docs, stripWhitespace)

inspect(docs[3])

# Remove punctuation

docs <- tm_map(docs, removePunctuation)
inspect(docs[3])

# Change special characters like @ to space

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "@")
docs <-tm_map(docs, removeNumbers)
inspect(docs[3])

# Change all the text to lower case

docs <- tm_map(docs, content_transformer(tolower))
inspect(docs[3])

# Remove pre loaded stop words
docs <- tm_map(docs, removeWords, stopwords("english"))
inspect(docs[3])

# Stem the document(replace words that are similar like grape/grapes

docs <- tm_map(docs, stemDocument)

# Create a DocumentTermMatrix(dtm)
dtm <- DocumentTermMatrix(docs)
dtm

# Create a new fariable freq to store frequency counts
freq <- colSums(as.matrix(dtm))
print(freq)
length(freq)

# Create a variable m to store the number of documents and number of
# distinct terms tracked in a matrix

m <- as.matrix(dtm)
dim(m)

# display m
m

# Find each term that is used at least 10 times
findFreqTerms(dtm, lowfreq=10)

# Create a variable dtms that will remove the words
# that appear in less than 70% of the documents

dtms <-removeSparseTerms(dtm, 0.7)
dtms
a
# Create a correlations plot with randomly selected 15 words
# that appear at least 5 times in all the documents
plot(dtm, terms=findFreqTerms(dtm, lowfreq = 5)[1:15], corThreshold = 0.5)
# Change the threshold and add a weighting(darker lines indicate more correlation)
plot(dtm, terms=findFreqTerms(dtm, lowfreq = 10)[1:6], corThreshold = 0.2, weighting = T)

# Word Frequency Plots
# Plot word frequencies for words that appear more than 5 times.

# Load the package

# Build a plot and store it in p
# Display p
p <- ggplot(subset(wf, freq>5), aes(word, freq))
p <- p + geom_bar(stat = "identity")
p <- p + theme(axis.text.x = element_text(angle=45, hjust=1))
p

# Word Cloud
library(wordcloud)
wordcloud(names(freq), freq, min.freq=5)

dtms <- removeSparseTerms(dtm, 0.7)
# get the sum of the columns in a numeric array
freq <- colSums(as.matrix(dtm))
# call brewer to color the word cloud
dark2 <- brewer.pal(6, "Dark2")

# create a word cloud
wordcloud(names(freq), freq, max.words = 100, rot.per = 0.2, colors=dark2)

# Run the library command to load the cluster package into memory
library("cluster")

# Cluster the words into 4 groups based on their appearance together and
# plot the results

# remove the terms that appear in less than 75% of the documents
dtms <-removeSparseTerms(dtm, 0.75)

#Build the dissimilarity matrix and store in variable d
d <-dist(t(dtms), method = 'euclidian')
# Build 4 clusters and store in a variable kfit
kfit <- kmeans(d, 4)


clusplot(as.matrix(d), kfit$cluster, color = T, shade = T, labels = 2, lines = 0)