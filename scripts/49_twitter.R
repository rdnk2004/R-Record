library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(tidyr)
library(wordcloud)
library(RColorBrewer)
tweets<-read.csv("data/Tweets.csv",stringsAsFactors=FALSE)
print(head(tweets,10))
str(tweets)
summary(tweets)
text_data<-tweets%>%
  select(airline,airline_sentiment,text= text)%>%
  filter(!is.na(text))%>%
  mutate(text=str_to_lower(text),
         text=str_replace_all(text,"http\\S+|www\\S+"," "),
         text=str_replace_all(text,"@\\w+"," "),
         text=str_replace_all(text,"[^a-z\\s]"," "))
words<-text_data%>%unnest_tokens(word,text)%>%
  anti_join(stop_words,by="word")
word_freq<-words%>%count(word,sort=TRUE)
print(head(word_freq,20))
ggplot(head(word_freq,20),aes(x=reorder(word,n),y=n))+
  geom_col()+coord_flip()+
  labs(title="Most Frequent Words",x="Word",y="Frequency")
bigrams<-text_data%>%
  unnest_tokens(bigram,text,token="ngrams",n=2)
print(head(bigrams,20))
bow<-words%>%count(word,sort=TRUE)
print(head(bow,20))
tfidf_data<-words%>%
  count(word,document=rep(seq_len(nrow(text_data)),length.out=n()))%>%
  bind_tf_idf(word,document,n)
print(head(tfidf_data%>%arrange(desc(tf_idf)),20))
bing_sentiment<-words%>%
  inner_join(get_sentiments("bing"),by="word")%>%
  count(sentiment,sort=TRUE)
print(bing_sentiment)
afinn_sentiment<-words%>%
  inner_join(get_sentiments("afinn"),by="word")%>%
  summarise(AFINN=sum(value))
print(afinn_sentiment)
nrc_sentiment<-words%>%
  inner_join(get_sentiments("nrc"),by="word")%>%
  count(sentiment,sort=TRUE)
print(nrc_sentiment)
positive_words<-words%>%
  inner_join(get_sentiments("bing"),by="word")%>%
  filter(sentiment=="positive")%>%count(word,sort=TRUE)
negative_words<-words%>%
  inner_join(get_sentiments("bing"),by="word")%>%
  filter(sentiment=="negative")%>%count(word,sort=TRUE)
print(head(positive_words,10))
print(head(negative_words,10))
airline_sentiment<-text_data%>%count(airline,airline_sentiment)
print(airline_sentiment)
ggplot(airline_sentiment,aes(x=airline,y=n,fill=airline_sentiment))+
  geom_col(position="dodge")+
  labs(title="Airline Sentiment Comparison",x="Airline",y="Number of Tweets")
ggplot(bing_sentiment,aes(x=sentiment,y=n))+
  geom_col()+
  labs(title="Bing Sentiment Analysis",x="Sentiment",y="Count")
ggplot(nrc_sentiment,aes(x=reorder(sentiment,n),y=n))+
  geom_col()+coord_flip()+
  labs(title="NRC Emotion Analysis",x="Emotion",y="Count")
set.seed(123)
wordcloud(words=word_freq$word,freq=word_freq$n,min.freq=10,max.words=100,random.order=FALSE,colors=brewer.pal(8,"Dark2"))
cat("Total Words =",nrow(words),"\n")
cat("Unique Words =",n_distinct(words$word),"\n")
cat("Most Frequent Word =",word_freq$word[1],"\n")
cat("Top Positive Word =",positive_words$word[1],"\n")
cat("Top Negative Word =",negative_words$word[1],"\n")