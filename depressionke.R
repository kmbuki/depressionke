# load twitter library - the rtweet library is now highly advised over twitteR
library(rtweet)


# plotting, data manipulation , cleaning!
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(tidyverse)

# text mining library
library(tidytext)
library(tm)
library(widyr)

# plotting packages
library(wordcloud)
library(RColorBrewer)
library(igraph)
library(ggraph)




# Authentication using rtweet

# Search for tweets non retweeted
depresssionke <- search_tweets(
  "#depressionke", n = 50000, include_rts = FALSE
)

depression <- search_tweets(
  "depression", geocode = lookup_coords("kenya"), 
  n = 50000, retryonratelimit = TRUE
)

mental_health <- search_tweets(
  "#LetsTalkMentalHealth", n=50000,
  geocode = lookup_coords("kenya"),
  retryratelimit = TRUE
)

rt_data <- rbind(depressionke, depression,mental_health)

# preview  data
head(tweet_data$text)

v <- unique(tweet_data$screen_name)
total <- aggregate(data.frame(count = v), list(value = v), length)

users_data(rt_data)

ts_plot(rt)

ts_plot(rt, "3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #depression Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


extract_text <- data_dump %>% select(text) 
extract_hashtag <- data_dump %>% select(hashtags) 
extract_datetime <- data_dump %>% select(created_at)

# save in csv file
write.csv(extract_text,file = "camb_analy.csv")


# GetTweetsource
data_dump %>% group_by(source)%>% 
  summarise(Total=n()) %>% arrange(desc(Total)) %>% head(10) %>%
  ggplot(aes(reorder(source, Total), Total, fill = source)) + 
  geom_bar(stat="identity") + coord_flip() + 
  labs(title="Top Tweet Sources for Cambridge Analytica Scandal", x="", 
       subtitle="More tweets came from Twitter Web Client", 
       caption = "\nSource: Data collected from Twitter's REST API via rtweet")

# Tweets distribution
data_dump %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("weeks", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by news organization",
    subtitle = "Twitter status (tweet) counts aggregated by day from October/November 2017",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )
  

# Cleaning the Data
data_dump$stripped_text <- gsub("http.*","",  data_dump$text)

data_dump$stripped_text <- gsub("https.*","",  data_dump$stripped_text)

tweet_clean <- data_dump %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word,stripped_text)

tweets_no_stop <- tweet_clean %>%
  anti_join(stop_words)



# Word cloud
write.csv(tweets_no_stop,file = "clean.csv")

cleaned_data <- read.csv("clean.csv", header=TRUE, 
                         stringsAsFactors=FALSE, fileEncoding="latin1")

tweet_corpus <- Corpus(DataframeSource(cleaned_data))

tweet_transformed <- DocumentTermMatrix(tweet_corpus, control = list(
  removePunctuation = TRUE, stripWhitespace = TRUE, 
  removeNumbers = TRUE, stopwords =  TRUE, 
  tolower = TRUE, wordLengths=c(1,Inf)))

myStopwords <- c(stopwords('english'),
                 "to", "like", "for", "and", "it", "a", "of", "we", "is", "has",  
                 "if", "the", "i", "in", "all", "many", "can", "thus", "other",
                 "maybe", "will", "helpful", "use", "uses", "help", "thing",
                 "well", "because", "want", "more", "these")
# remove stopwords form corpus
tweet_transformed <- tm_map(tweet_corpus, removeWords , myStopwords)

DTM <- DocumentTermMatrix(tweet_transformed)


dtms <- removeSparseTerms(DTM, 0.99)
# Visualize
my.dtms <- as.matrix(DTM)

term.freq <- sort(colSums(my.dtms),decreasing=T)

head(term.freq,50)

wordcloud(words=names(term.freq),freq=term.freq, min.freq=100,
          random.order=F, colors=brewer.pal(8,'Dark2'))


#Sentiment Anaysis
library(syuzhet)
mySentiment <- get_nrc_sentiment(data_dump$text)

head(mySentiment)

tweet_subset = subset(data_dump, select = -c( 0:2, 5:87) )

tweet_senti <- cbind(tweet_subset, mySentiment)


# SentimentsGraph results
sentimentTotals <- data.frame(colSums(tweet_senti[,c(3:12)]))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Scores for Cambridge Analytica Tweets")
