setwd("G:\\projects\\text mining\\texts")

cname <- file.path("G:\\projects\\text mining", "texts")   
cname   
dir(cname)

library(tm)   

docs <- Corpus(DirSource(cname))   

summary(docs)  

#Removing punctuation:
docs <- tm_map(docs, removePunctuation) 

for(j in seq(docs))   
{   
  docs[[j]] <- gsub("/", " ", docs[[j]])   
  docs[[j]] <- gsub("@", " ", docs[[j]])   
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("'", " ", docs[[j]]) 
  docs[[j]] <- gsub("'", " ", docs[[j]])
} 

docs <- tm_map(docs, removeNumbers)

#Converting to lowercase:
docs <- tm_map(docs, tolower) 

strwrap(docs[[1]])

#Removing "stopwords" (common words) that usually have no analytic value.
# stopwords("english")   
docs <- tm_map(docs, removeWords, stopwords("english"))  

#Removing common word endings (e.g., "ing", "es", "s")
library(SnowballC)   
docs <- tm_map(docs, stemDocument)

#Stripping unnecesary whitespace from your documents:
docs <- tm_map(docs, stripWhitespace)

#To treat your preprocessed documents as text documents.
docs <- tm_map(docs, PlainTextDocument)  

#document term matrix.
dtm <- DocumentTermMatrix(docs)   
dtm
inspect(dtm)

#inspect(dtm[1:5, 1:20])
dim(dtm)
#transpose of this matrix
tdm <- TermDocumentMatrix(docs)   
tdm  

#Organize terms by their frequency:
freq <- colSums(as.matrix(dtm))   
length(freq)  
ord <- order(freq)  
#f<-as.data.frame(freq)
m <- as.matrix(dtm)   
dim(m)   

dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
inspect(dtms)

 
freq[head(ord)] 
freq[tail(ord)]
head(table(freq), 20) 

wf <- data.frame(word=names(freq), freq=freq)   
head(wf)

library(ggplot2)   
p <- ggplot(subset(wf, freq>10), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p



library(wordcloud)  
set.seed(142)   
wordcloud(names(freq), freq, min.freq=10, colors =c("blue","green","red") ) 
wordcloud(names(freq), freq, min.freq=10, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))


dtmss <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is only 15% empty space, maximum.   
inspect(dtmss)
#Hierarchal Clustering
library(cluster)   
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="ward.D2")   
fit  
plot(fit, hang=-1)   


plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters

library(fpc)   
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   

