library(textclean)
library(katadasaR)
library(tokenizers)
library(wordcloud)
library(dplyr)
library(rtweet)
library(tidyr)
library(tidytext)
library(ggplot2)
library(textdata)
library(purrr)
library(httpuv)
library(devtools)
library(katadasaR)
library(sentimentr)


api_key <- "OeRcPb0E4JSDwjz7kBfzjODYc"
api_secret_key <- "R16tsxucMQhpAqogG3vYzq3wbfqJy4aYZxcM7WmNqPS81ujBSy"

token <- create_token(
  app = "SentimentAnalysisGusti",
  consumer_key = api_key,
  consumer_secret = api_secret_key
)
token

tweet <- search_tweets("#indonesiaterserah", n=1000, include_rts = FALSE, lang = "id")
tweet

tweet = tweet%>%select(screen_name, text)
tweet
head(tweet$text)

Kata = tweet%>%select(screen_name, text)
Kata
Kata$stripped_text1 <- gsub("http\\S+","",Kata$text)

Kata <- Kata%>%
  select(stripped_text1)%>%
  unnest_tokens(word, stripped_text1)
head(Kata)

Kata <- Kata%>%
  anti_join(stop_words)
head(Kata)
head(tweet$text)


#udah bener
Kata%>%
  count(word, sort = TRUE)%>%
  top_n(20)%>%
  mutate(word = reorder(word,n))%>%
  ggplot(aes(x = word, y = n))+
  geom_col()+
  coord_flip()+
  theme_classic()+
  labs(x = "Count",y = "Kata unik", title = "Unique word counts found in #indonesiaterserah Tweets")

bing_kata = Kata%>%
  inner_join(get_sentiments("bing"))%>%count(word, sentiment, sort = 
                                               TRUE)%>%ungroup()
bing_kata%>%
  group_by(sentiment)%>%top_n(10)%>%
  ungroup()%>%mutate(word=reorder(word,n))%>%
  ggplot(aes(word,n,fill=sentiment))+geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment,scales = "free_y")+labs(title = "Tweet Berisi #indonesiaterserah",
                                                y = "Contribution to sentiment",
                                                x=NULL)+coord_flip()+theme_bw()
tweet <- tweet$text %>% 
  replace_html() %>% # replace html with blank 
  replace_url()%>%   # replace URLs with blank
  replace_emoji(.) %>% 
  replace_html(.) %>% 
  replace_tag(kata, pattern = "@([A-Za-z0-9_]+)",replacement="") %>%  # remove mentions
  replace_hash(kata, pattern = "#([A-Za-z0-9_]+)",replacement="")      # remove hashtags

head(tweet)
tweet <- gsub("\n","",tweet)
head(tweet)

#hapus data duplikat
tweet <- tweet %>% 
  as.data.frame() %>% 
  distinct()

#hapus data baris index ke-2 s.d 6
tweet1 = tweet[-c(2, 3,4,5,6),]

head(tweet1)
setwd("D:/Tugas/Big Data")
#import kamus bahasa slang
spell.lex <- read.csv("colloquial-indonesian-lexicon.csv")

# replace internet slang

tweet1 <- replace_internet_slang(tweet1, slang = paste0("\\b",
                                                      spell.lex$slang, "\\b"),
                                replacement = spell.lex$formal, ignore.case = TRUE)
head(tweet1)

tweet1<- strip(tweet1)
head(tweet1)

tweet1 <- tweet1 %>% 
  as.data.frame() %>% 
  distinct()
head(tweet1)

tweet1 <- as.character(tweet1$.)
#stemming dengan library(katadasaR)
stemming <- function(x){
  paste(lapply(x,katadasar),collapse = " ")}

#tokenisasi
tweet1 <- lapply(tokenize_words(tweet1[]), stemming)
head(tweet1)

tweet1 <- tokenize_words(tweet1)
head(tweet1,3)

library(stopwords)
myStopwords <- readLines("list_stopword.txt")
tweet1 <- as.character(tweet1)
tweet1 <- tokenize_words(tweet1, stopwords = myStopwords)
head(tweet1, 3)

class(tweet1)
#buat wordcloud
tweet1 <- as.character(tweet1)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(tweet1,scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
warnings()
#Import kata positif dan negatif
opinion.lexicon.pos = scan("positive.txt", what = "character", comment.char = ";")
opinion.lexicon.neg = scan("negative.txt", what = "character", comment.char = ";")
head(opinion.lexicon.pos)
head(opinion.lexicon.neg)
pos.words = c(opinion.lexicon.pos)
neg.words = c(opinion.lexicon.neg)


getSentimentScore = function(sentences, pos.words, neg.words, .progress = "none")
{
  require(plyr)
  require(stringr)
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    #remove digit, punctuation, dan special/control character:
    sentence = gsub("[[:cntrl:]]", "", gsub("[[:punct:]]", "", gsub("\\d+", "", sentence)))
    
    #convert semua teks menjadi lowercase:
    sentence = tolower(sentence)
    
    #pisahkan setiap kalimat menggunakan spasi (space delimiter):
    words = unlist(str_split(sentence, "\\s+"))
    
    #lakukan boolean match dari setiap kata-kata menggunakan pos &amp;amp;amp; neg opinion-lexicon:
    pos.matches = !is.na(match(words, pos.words))
    neg.matches = !is.na(match(words, neg.words))
    
    #score sentimen = total positive sentiment - total negative:
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  
  #return data frame berisi kalimat beserta sentimennya:
  return(data.frame(text = sentences, score = scores))
}

#terapkan ke data tweet yang telah kita bersihkan:

tweetResult = getSentimentScore(tweet1, pos.words, neg.words)
hist(tweetResult$score)
count(tweetResult$score)
