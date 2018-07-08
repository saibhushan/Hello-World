#Fetching Data from Twitter 
install.packages(c("twitteR", "ROAuth","plyr","stringr","ggplot2"), dependencies=T)

library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(ggplot2)


download.file(url="https://curl.haxx.se/ca/cacert.pem","cacert.pem")

reqURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'
consumerKey <- 'bLS8xCF7kdRlgO15fMXb1tQHH' #put the Consumer Key from Twitter Application
consumerSecret <- 'SUO0JhbvthXteSqiTNH6RLK8Jz6AFlJHBYaUT6zXeBc7iQFYYa'  #put the Consumer Secret from Twitter Application
Cred <- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret,requestURL=reqURL,accessURL=accessURL,authURL=authURL)
Cred$handshake(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl')) 

6911163

save(Cred, file='twitter authentication.Rdata')
load('twitter authentication.Rdata') 
registerTwitterOAuth(Cred)



setup_twitter_oauth(CUSTOMER_KEY, CUSTOMER_SECRET, ACCESS_TOKEN, ACCESS_secret, credentials_file=NULL
                    
                    accessToken <- '75840391-EYBPbHCEi7WEwq9POjWWtHvAdc5RSrLdPyXbSYYxH'
                    accessTokenSecret<-'SXqEtTKhPoNQrbBzLvmQSiRjp6f4sKuKkY0BlxlyylQdZ'
                    
                    setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)
                    
                    KarnatakaFloorTest.list <- searchTwitter('#KarnatakaFloorTest', n=1000,"cacert.pem")
                    
                    KarnatakaFloorTest.list <- searchTwitter("#KarnatakaFloorTest",n=1000,lang=NULL,since=NULL,until=NULL,locale=NULL, geocode=NULL, sinceID=NULL, maxID=NULL,resultType=NULL)
                    
                    KarnatakaFloorTest.df<-twListToDF(KarnatakaFloorTest.list)
                    
                    write.csv(KarnatakaFloorTest.df, file="C:/Users/Admin-pc/Desktop/Analysis/KarnatakaFloorTest.csv",row.names=F)
                    
                    Tweet Analysis
                    
                    install.packages("RColorBrewer")
                    library("RColorBrewer")
                    
                    sentences <- read.csv(file="C:/Users/Admin-pc/Desktop/Analysis/text.csv", header=TRUE, sep=",")
                    pos.words <- read.csv(file="C:/Users/Admin-pc/Desktop/Analysis/PositiveWordsCSV.csv", header=TRUE, sep=",")
                    pos.words <- unlist(pos.words)
                    neg.words <- read.csv(file="C:/Users/Admin-pc/Desktop/Analysis/NegativeWordsCSV.csv", header=TRUE, sep=",")
                    neg.words <- unlist(neg.words)
                    
                    
                    score <- data.frame(matrix(0, ncol = 1, nrow =nrow(sentences)))
                    
                    for(i in 1:nrow(sentences)){
                      sentence <-sentences[i,1]
                      sentence <- gsub('[[:punct:]]', "", sentence)
                      sentence <- gsub('[[:cntrl:]]', "", sentence)
                      sentence <- gsub('\\d+', "", sentence)
                      sentence <- tolower(sentence)
                      word.list <- str_split(sentence, '\\s+')
                      words <- unlist(word.list)
                      pos.matches <- match(words, pos.words)
                      neg.matches <- match(words, neg.words)
                      pos.matches <- !is.na(pos.matches)
                      neg.matches <- !is.na(neg.matches)
                      score[i,1] <- sum(pos.matches) - sum(neg.matches)
                    }
                    
                    scores.df<-data.frame(score,sentences)
                    
                    colnames(scores.df) <- c("score", "text")
                    
                    qplot(scores.df$score, xlab="Scoreof Tweets")
                    
                  #  Text Analysis
                    
                    library(tm)
                    library(wordcloud)
                    K.df<-as.data.frame(list(scores.df$text))
                    colnames(K.df)<-"text"
                    
                    K.df.v1<- matrix(as.character(K.df[,1]),nrow=nrow(K.df), ncol=1,byrow = TRUE)
                    
                    docs <-Corpus(VectorSource(K.df))
                    docs<- tm_map(docs,removeNumbers)
                    docs<- tm_map(docs, tolower)
                    docs<- tm_map(docs, removePunctuation)
                    docs <- tm_map(docs, removeWords, stopwords("english"))
                    docs <- tm_map(docs, removeWords, c("uuu", "uub","uue","uuuu","ueuu","ufu","uufuueufu")) 
                    docs<- tm_map(docs, stripWhitespace)
                    inspect(docs)
                    tdm<-TermDocumentMatrix(docs)
                    m1<-as.matrix(tdm)
                    m1
                    v1<- sort(rowSums(m1), decreasing=TRUE)
                    v1
                    d1<-data.frame(word=names(v1),freq=v1)
                    d1
                    library("wordcloud")
                    wordcloud(d1$word,d1$freq,col=brewer.pal(8,"Set2"),min.freq="10",max.words=100,rot.per=0.0)
                    
                    