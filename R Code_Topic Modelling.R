##df <- read.csv("C:\\Users\\acer\\Desktop\\SmartCities_TwitterData.csv",header=TRUE)
df <- read.csv("SmartCities.csv")
library(parallel)
cleanTweets<-function(tweet){
  ##Remove html links :
  tweet<-gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)","",tweet)
  ## Removal of Hashtags : 
  tweet<-gsub("#\\w+","",tweet)
  ## Removal of @ People : 
  tweet<-gsub("@\\w+","",tweet)
  ## Removal of numbers , as we need only text for topic modelling :
  tweet<-gsub("[[:digit:]]","",tweet)
  ## Removal of all except letters
  tweet <- gsub("[^a-zA-Z]"," ",tweet)
  ## Removal of unncessary spaces , tabs etc :
  tweet<-gsub("[\t]{2,}","",tweet)
  tweet<-gsub("^\\s+|\\s+$","",tweet)
  
}

CleanContent=sapply(df$contents,cleanTweets)

library(tm)
library(NLP)
library("wordcloud")
library("slam")
## Now we create the Corpus first
corpus <- Corpus(VectorSource(CleanContent))
## Conversion to lower
corpus <- tm_map(corpus,tolower)
## Removal of punctuations
corpus <- tm_map(corpus,removePunctuation)
## Removal of English Stop Words :
corpus <- tm_map(corpus,removeWords,stopwords("english"))
## Remove spaces
corpus <- tm_map(corpus, stripWhitespace)
## Stem document
corpus <- tm_map(corpus,stemDocument)
## Removal of numbers :
#Strip digits
corpus <- tm_map(corpus, removeNumbers)

#test corpus
writeLines(as.character(corpus[[1]]))

#define and eliminate all custom stopwords
myStopwords <- c("can", "say","one","way","use",
                 "also","howev","tell","will",
                 "much","need","take","tend","even",
                 "like","particular","rather","said",
                 "get","well","make","ask","come","end",
                 "first","two","help","often","may",
                 "might","see","someth","thing","point",
                 "post","look","right","now","think","'ve ",
                 "'re ","anoth","put","set","new","good",
                 "want","sure","kind","larg","yes,","day","etc",
                 "quit","sinc","attempt","lack","seen","awar",
                 "littl","ever","moreov","though","found","abl",
                 "enough","far","earli","away","win","achiev","draw",
                 "last","never","brief","bit","entir","brief",
                 "great","lot","smart","city","cities","winner","wins","many","won",
                 "via","year","time","best","week","today","part","gets","finds","less",
                 "plan","looses","read")

corpus <- tm_map(corpus, removeWords, myStopwords)

corpus <- tm_map(corpus,PlainTextDocument)


dftemp<-data.frame(text=unlist(sapply(corpus, `[`, "content")),stringsAsFactors=F)
dftemp2<- df
dftemp2$contents <- dftemp$text


################# for deleting duplicate tweets leaving retweets

dftemp2 <- dftemp2[with(dftemp2,order(host),order(contents)),]
dftemp3 <- dftemp2
lnth <- dim(dftemp2)[1]
flag = 1
cnt = 0
for(i in 2:lnth) {
  
  if(dftemp2$host[flag] == dftemp2$host[i]){
    if(dftemp2$contents[flag] == dftemp2$contents[i]){
      
      dftemp3 <- dftemp3[-i+cnt,]
      cnt = cnt + 1
    }
    else{
      flag = i
    }
  }
  else{
    flag = i
  }
}

############################

write.csv(dftemp3,"H:\\MBA_IIT K Studye materials\\MBA_IIT K Studye materials\\Capstone Project\\Evalueserve\\Smart Cities\\dftemp3.csv")

corpus <- Corpus(VectorSource(dftemp3$contents))

frequencies <- DocumentTermMatrix(corpus)
sparse <- removeSparseTerms(frequencies,0.995)
tdm=sparse

# create tf-idf matrix
term_tfidf <- tapply(tdm$v/row_sums(tdm)[tdm$i], tdm$j, mean) * log2(nDocs(tdm)/col_sums(tdm > 0))
summary(term_tfidf)
tdm <- tdm[,term_tfidf >= 0.1]

rowTotals <- apply(tdm , 1, sum)
sparse.new   <- tdm[rowTotals> 0, ]

dfSparse <- as.data.frame(as.matrix(sparse.new))
colnames(dfSparse) <- make.names(colnames(dfSparse))

write.csv(dfSparse,"H:\\MBA_IIT K Studye materials\\MBA_IIT K Studye materials\\Capstone Project\\Evalueserve\\Smart Cities\\dfSparse.csv")


## Now we start LDA :
##install.packages("topicmodels",dependencies=TRUE)
library(topicmodels)

###### We decide the number of optimal topics By Using Rmpfr Package & HM estimation #######

#install.packages("Rmpfr")
library(Rmpfr)
##harmonicMean <- function(logLikelihoods, precision=2000L) {
 ## library("Rmpfr")
  #llMed <- median(logLikelihoods)
  #as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

# The log-likelihood values are then determined by first fitting the model using for example
k = 20
burnin = 1000
iter = 1000
keep = 50

#fitted <- LDA(dfSparse, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) )

# where keep indicates that every keep iteration the log-likelihood is evaluated and stored. This returns all log-likelihood values including burnin, i.e., these need to be omitted before calculating the harmonic mean:

#logLiks <- fitted@logLiks[-c(1:(burnin/keep))]

# assuming that burnin is a multiple of keep and

#harmonicMean(logLiks)

# generate numerous topic models with different numbers of topics
#sequ <- seq(2, 200, 2) # in this case a sequence of numbers from 2 to 200, by twos.
#fitted_many <- lapply(sequ, function(k) LDA(dfSparse, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) ))

# extract logliks from each topic
#logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

# compute harmonic means
#hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

# inspect
#plot(sequ, hm_many, type = "l")

# compute optimum number of topics
#sequ[which.max(hm_many)]  ## MAx arrived at k=58

###### Another way to determine optimum no. of topics #####
library("ldatuning")

#result <- FindTopicsNumber(
  dfSparse,
  topics = seq(from = 10, to = 60, by = 2),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
#FindTopicsNumber_plot(result)
### Max arrived at 58 for Griffiths 2004 and mimum arrived at 58 for Deveaud 2014 ####


######## Set parameters for Gibbs sampling and LDA estimation #########
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 40   ## As determined by optimum number of topics

#Run LDA using Gibbs sampling
ldaOut <-LDA(sparse.new,k, method="Gibbs", 
             control=list(nstart=nstart, seed = seed, best=best, 
                          burnin = burnin, iter = iter, thin=thin))

#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

#top 10 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,10))
print(ldaOut.terms)
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))


## Application of CTM : 
ctmOut<-CTM(sparse.new, k, method ="VEM")
## Top 10 terms in each topic after CTM :
ctmOut.terms <- as.matrix(terms(ctmOut,10))
print(ctmOut.terms)
write.csv(ctmOut.terms,file=paste("CTMVEM",k,"TopicsToTerms_CTM.csv"))

#write out results
#docs to topics
ctmOut.topics <- as.matrix(topics(ctmOut))
write.csv(ctmOut.topics,file=paste("CTMVEM",k,"DocsToTopics_CTM.csv"))

#probabilities associated with each topic assignment
topicProbabilities_CTM <- as.data.frame(ctmOut@gamma)
write.csv(topicProbabilities_CTM,file=paste("CTMVEM",k,"TopicProbabilities_CTM.csv"))

## Alternative LDA Implementation and formation of word clouds :
##install.packages("mallet",dependencies = TRUE)
##install.packages("rJava")
##library(mallet)
##install.packages("tidyr")

##### Labelling the topics #####

lda.terms <- as.data.frame(topicmodels::terms(ldaOut, 20), stringsAsFactors = FALSE)
lda.terms[1:5]

lda.topics <- topicmodels::topics(ldaOut, 1)


topicTerms <- tidyr::gather(lda.terms, Topic)
topicTerms <- cbind(topicTerms, Rank = rep(1:20))
topTerms <- dplyr::filter(topicTerms, Rank < 6)
topTerms <- dplyr::mutate(topTerms, Topic = stringr::word(Topic, 2))
topTerms$Topic <- as.numeric(topTerms$Topic)
topicLabel <- data.frame()
for (i in 1:58){
  z <- dplyr::filter(topTerms, Topic == i)
  l <- as.data.frame(paste(z[1,2], z[2,2], z[3,2],z[4,2],z[5,2], sep = " " ), stringsAsFactors = FALSE)
  topicLabel <- rbind(topicLabel, l)
  
}
colnames(topicLabel) <- c("Label")
topicLabel

###### Code for wordcloud visualization ######


# get word counts in decreasing order
word_freqs = sort(colSums(dfSparse), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud
wordcloud(dm$word, dm$freq, min.freq=100,random.order=FALSE, colors=brewer.pal(8, "Dark2"))



library(parallel)
library(chron) #for time
df = read.csv("data_4195.csv",header=TRUE)
dffinal <- data.frame(x=nrow(df),y=3)

for(j in 1:nrow(df)){
  count = 0
  tweet = df[j,]$content
  day = as.Date(df[j,]$day)
  time = chron(times=df[j,]$time)
  dffinal[j,1] = toString(df[j,]$host)
  dffinal[j,2] = count
  if(identical(substring(tweet,1,3),"rt ") == FALSE){
    for(k in 1:nrow(df)){
      if(identical(tweet,df[k,]$content) == TRUE && j!=k){
        if(day < as.Date(df[k,]$day) || (day == as.Date(df[k,]$day) && time < chron(times=df[k,]$time))){
          count = count+1 }
      }
      else if(identical(paste("rt ",tweet),df[k,]$content) == TRUE){
        count = count + 1 }
    }
  }
  dffinal[j,2] = count
}

write.csv(dffinal,"H:\\MBA_IIT K Studye materials\\MBA_IIT K Studye materials\\Capstone Project\\Evalueserve\\Smart Cities\\influencers.csv")
  


### Code for influenc with only rt ###
library(chron) #for time
library(doParallel)
df = read.csv("data_4195.csv",header=TRUE)
dffinal <- data.frame(x=nrow(df),y=3)

for(i in 1:nrow(df)){
  count = 0
  tweet = df[i,]$content
  day = as.Date(df[i,]$day)
  time = chron(times=df[i,]$time)
  dffinal[i,1] = toString(df[i,]$host)
  dffinal[i,2] = count
  if(identical(substring(tweet,1,3),"rt ") == FALSE){
    for(j in 1:nrow(df)){
      
      if(identical(paste("rt ",tweet),df[j,]$content) == TRUE){
        count = count + 1 }
    }
  }
  dffinal[i,2] = count
}

write.csv(dffinal,,





