library("ggplot2")         # GRAPHICAL VISUALIZATION 
library("quanteda")        # QUANTITATIVE ANALYSIS OF TEXTUAL DATA AND FOR TOKENS
library("plotly")          # PLOTTING POLAR GRAPH
library("syuzhet")         # CREATING SENTIMENTS USING GET_NRC_SENTIMENTS
library("wordcloud2")      # FOR CREATING WORD CLOUD
library("twitteR")         # FOR ACCESSING TWITTER DATA
library("ROAuth")          # FOR AUTHENTICATING WITH TWITTER SERVER USING KEYS

# TWITTER ACCESS

download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

cred <- OAuthFactory$new(consumerKey='5Fwc2mCwnkDzld8dph4ZgvQmt',
                         consumerSecret='xwSGiuEtjbt1STZ789pVByzwnzrLGXrJt04KnZSVm7hvAfKIgK',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")
setup_twitter_oauth('5Fwc2mCwnkDzld8dph4ZgvQmt','xwSGiuEtjbt1STZ789pVByzwnzrLGXrJt04KnZSVm7hvAfKIgK','1103898362141528064-P8HLymRzNN3Kv9md5FKhybyeQODWiX','7E4A2M7mCEFZpowqlePQHH5ybzb3maphb3MHddn6kXpjS')
load("twitter authentication.Rdata")
search.string <- "#kejriwal"
no.of.tweets <- 100
DATA1 <- searchTwitter(search.string, n=no.of.tweets, lang="en")
DATA1=twListToDF(DATA1)
View(DATA1)

search.string <- "#modi"
no.of.tweets <- 100
DATA2 <- searchTwitter(search.string, n=no.of.tweets, lang="en")
DATA2=twListToDF(DATA2)
View(DATA2)


# SENTIMENTAL ANALYSIS

DATA1.token <- tokens(DATA1$text, what = "word", 
                      remove_numbers = TRUE, remove_punct = TRUE,remove_twitter = TRUE,
                      remove_symbols = TRUE, remove_hyphens = TRUE,remove_url = TRUE)


DATA2.token <- tokens(DATA2$text, what = "word", 
                     remove_numbers = TRUE, remove_punct = TRUE,remove_twitter = TRUE,
                     remove_symbols = TRUE, remove_hyphens = TRUE,remove_url = TRUE)



DATA2.token<- tokens_select(DATA2.token, stopwords(), selection = "remove")
DATA2.token  <- tokens_wordstem(DATA2.token , language = "english")
DATA1.token<- tokens_select(DATA1.token, stopwords(), selection = "remove")
DATA1.token  <- tokens_wordstem(DATA1.token , language = "english")



DATA2.token.dfm <- dfm(DATA2.token, tolower = TRUE)
DATA2.token.matrix=as.matrix(DATA2.token.dfm)
DATA1.token.dfm <- dfm(DATA1.token, tolower = TRUE)
DATA1.token.matrix=as.matrix(DATA1.token.dfm)



DATA2_KEYWORD_COUNT=colSums(DATA2.token.matrix)
DATA1_KEYWORD_COUNT=colSums(DATA1.token.matrix)


DATA2_KEYWORD_COUNT=data.frame(LABEL=names(DATA2_KEYWORD_COUNT),FREQ=DATA2_KEYWORD_COUNT,stringsAsFactors = F)
row.names(DATA2_KEYWORD_COUNT)=NULL
DATA1_KEYWORD_COUNT=data.frame(LABEL=names(DATA1_KEYWORD_COUNT),FREQ=DATA1_KEYWORD_COUNT,stringsAsFactors = F)
row.names(DATA1_KEYWORD_COUNT)=NULL

DATA1_ONLY_KEYWORD=data.frame()
sptmp=NULL
for (i in 1:nrow(DATA1_KEYWORD_COUNT)){
  sptmp=rep(DATA1_KEYWORD_COUNT[i,1],DATA1_KEYWORD_COUNT[i,2])
  DATA1_ONLY_KEYWORD=rbind(DATA1_ONLY_KEYWORD,data.frame(sptmp))
}
DATA1_ONLY_KEYWORD$sptmp=as.character(DATA1_ONLY_KEYWORD$sptmp)



DATA2_ONLY_KEYWORD=data.frame()
htmp=NULL
for (i in 1:nrow(DATA2_KEYWORD_COUNT)){
  htmp=rep(DATA2_KEYWORD_COUNT[i,1],DATA2_KEYWORD_COUNT[i,2])
  DATA2_ONLY_KEYWORD=rbind(DATA2_ONLY_KEYWORD,data.frame(htmp))
}
DATA2_ONLY_KEYWORD$htmp=as.character(DATA2_ONLY_KEYWORD$htmp)



View(DATA1_ONLY_KEYWORD)


DATA2_SENTIMENTS=get_nrc_sentiment(DATA2_ONLY_KEYWORD$htmp)
colSums(DATA2_SENTIMENTS)

DATA1_SENTIMENTS=get_nrc_sentiment(DATA1_ONLY_KEYWORD$sptmp)
colSums(DATA1_SENTIMENTS)



par(mfrow=c(1,2))

barplot(colSums(DATA2_SENTIMENTS),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'SENTIMENTS OF MODI')

barplot(colSums(DATA1_SENTIMENTS),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'SENTIMENTS OF KEJRIWAL')




DATA1ENTIWORD <- data.frame(SENTIMENTS=names(DATA1_SENTIMENTS),FREQUENCY=colSums(DATA1_SENTIMENTS))
wordcloud2(DATA1ENTIWORD,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)



DATA2ENTIWORD <- data.frame(SENTIMENTS=names(DATA2_SENTIMENTS),FREQUENCY=colSums(DATA2_SENTIMENTS))
wordcloud2(DATA2ENTIWORD,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)


wordcloud2(DATA1_KEYWORD_COUNT,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)


wordcloud2(DATA2_KEYWORD_COUNT,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

plot_ly(
  type = 'scatterpolar',
  fill = 'toself'
) %>%
  add_trace(
    r = colSums(DATA2_SENTIMENTS),
    theta =names(colSums(DATA2_SENTIMENTS)),
    name = 'MODI'
  ) %>%
  add_trace(
    r = colSums(DATA1_SENTIMENTS),
    theta =names(colSums(DATA1_SENTIMENTS)),
    name = 'KEJRIWAL'
  ) %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,nrow(no.of.tweets))
      )
    )
  )
