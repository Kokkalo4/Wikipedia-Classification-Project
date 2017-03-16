#Done through guidance from [here](https://dzone.com/articles/manipulate-clusters-of-texts) and
#[here](http://brazenly.blogspot.gr/2015/02/r-categorization-clustering-of.html)


#Load Libraries
library(tm)
library(stringi)
library(proxy)
library(wordcloud)

#Read wikipedia.
#I chose topics from 3 distinct areas: Geography, Mathematics and Society.
#Geography: Amchitka, David A. Johnston, Alaska Purchase
#Mathematics: Theorem, Rational number, Platonic solid
#Society: Economy, Social, Nation state
wiki <- "http://en.wikipedia.org/wiki/"
titles <- c("Florida",
           "California",
           "Alaska",
           "Rectangular", "Square", "Rhombus",
           "Bulls", "Bucks", "Spurs")
articles <- character(length(titles))
for (i in 1:length(titles)) {
  articles[i] = stri_flatten(readLines(stri_paste(wiki, titles[i])), col = " ")
}

#transforming what we got(articles) into a corpus
docs <- Corpus(VectorSource(articles))

#Cleaning corpus
docs <- tm_map(docs, function(x) stri_replace_all_regex(x, "<.+?>", " ")) # Replace characters like + , . , ? etc with Space  " "
docs <- tm_map(docs, function(x) stri_replace_all_fixed(x, "\t", " ")) # Replace tab characters (\t) with Space
docs <- tm_map(docs, stripWhitespace) # Remove extra whitespaces from the documents.
docs <- tm_map(docs, removeWords, stopwords("english")) # Remove from the documents words which we find redundant for text mining (e.g. pronouns, conjunctions).
# We set this words as stopwords("english") which is a built-in list for English language
# (this argument is passed to the function removeWords.
docs <- tm_map(docs, removePunctuation) # Remove punctuation marks.
docs <- tm_map(docs, tolower) # Transform characters to lower case.
docs <- tm_map(docs, PlainTextDocument) # Convert previous result (returned type was "string") to "PlainTextDocument" so that we can apply the other functions from tm package, which require this type of argument.

# Now we are ready to proceed to the core of the analysis.
# The starting point is creating "Term document matrix".
# It describes the frequency of each term in each document in the corpus.
# This is a fundamental object in the text analysis.
#
docsTDM <- DocumentTermMatrix(docs)

#Transform docsTDM to a matrix
#Now, we simply count words in each page:
docsTDM2 <- as.matrix(docsTDM)
dim(docsTDM2) #getting the dimensions of the matrix
frequency <- colSums(docsTDM2)
frequency <- sort(frequency, decreasing=TRUE)
mots=frequency[frequency>20]
s=docsTDM2[1,which(colnames(docsTDM2) %in% names(mots))]
for(i in 2:nrow(docsTDM2)) s=cbind(s,docsTDM2[i,which(colnames(docsTDM2) %in% names(mots))])
colnames(s) <- titles

# Once we have that dataset, we can use a PCA to visualise the 'variables' i.e. the pages:
library(FactoMineR)
PCA(s)

#Clustering the text
s0=s/apply(s,1,sd)
h <- hclust(dist(t(s0)), method = "ward.D")
plot(h, labels = titles, sub = "")