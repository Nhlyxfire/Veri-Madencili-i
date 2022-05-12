install.packages("twitteR")
install.packages("ROAuth") 
install.packages("tm") 
install.packages("RCurl")
install.packages("magrittr")
install.packages("dplyr") 
install.packages("tidyverse")
install.packages("ggplot2") 
install.packages("funModeling")
install.packages("lubridate")
install.packages("stringi")
install.packages("stringr")
install.packages("readxl")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("tible")
install.packages("tidyr")
install.packages("ggthemes")
install.packages("readr")
install.packages("readxl")
install.packages("ggpubr")
install.packages("formattable")
install.packages("ggstance")
install.packages("pastecs")
install.packages("psych")
install.packages("GGally")
install.packages("pander")
install.packages("rstatix")
install.packages("sentimentr")
install.packages("webshot")
install.packages("wordcloud2")
install.packages("htmlwidgets")
install.packages("syuzhet")

library(lubridate)
library(twitteR)
library(tm)
library(ROAuth)
library(RCurl)
library(magrittr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(funModeling)
library(stringi)
library(stringr)
library(tm)
library(tidytext)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tible)
library(tidyr)
library(ggthemes)
library(readr)
library(readxl)
library(ggpubr)
library(formattable)
library(ggstance)
library(pastecs)
library(psych)
library(GGally)
library(pander)
library(rstatix)
library(sentimentr)
library(webshot)
library(wordcloud)
library(wordcloud2)
library(htmlwidgets)
library(magrittr)
library(dplyr) 

consumer_key <- "7uyR1KKNxdMNO7fEewpmdRnre"
consumer_secret <- "ahL6bto8sNWh8XKbDjt0I4h9w0OAk9Gf2c4q4qKrz0ctjSmEdz"
access_token <- "1040993810820542469-CV0ateBjXfGZqKCgkPFXnwA76xgZb6"
access_secret <- "CeXpVMjyxfCD8oJowxU8J2QofsHHpHWHAoiZkNSWUJhTS"

setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

tweets.df <- twListToDF(tweets)
tweet_clean <- tweets.df
tweet_clean$text <- stri_enc_toutf8(tweet_clean$text)

#####rtlerin kaldırılması
tweet_clean$text <- ifelse(str_sub(tweet_clean$text,1,2) == "RT",
                           substring(tweet_clean$text,3),
                           tweet_clean$text)

#URL linklerinin temizlenmesi
tweet_clean$text <- str_replace_all(tweet_clean$text, "http[^[:space:]]*", "")


#Hashtag "#" ve "@" işaretlerinin kaldırılması
tweet_clean$text <- str_replace_all(tweet_clean$text, "#\\S+", "")
tweet_clean$text <- str_replace_all(tweet_clean$text, "@\\S+", "")

#Noktalama işaretlerinin temizlenmesi
tweet_clean$text <- str_replace_all(tweet_clean$text, "[[:punct:][:blank:]]+", " ")

#Tüm harflerin küçük harfe dönüştürülmesi
tweet_clean$text  <- str_to_lower(tweet_clean$text, "tr")

#Rakamların temizlenmesi
tweet_clean$text <- removeNumbers(tweet_clean$text)

#stopwordsler
stopwords::stopwords("tr", source = "stopwords-iso")
"ekonomi" 

#Gereksi tekrarlar ve bağlaçların temizlenmesi
tweet_clean$text = removeWords(tweet_clean$text,liste)

#ASCII formatına uymayan karakterlerin temizlenmesi
tweet_clean$text <- str_replace_all(tweet_clean$text, "[<].*[>]", "")
tweet_clean$text <- gsub("\uFFFD", "", tweet_clean$text, fixed =  TRUE)
tweet_clean$text <- gsub("\n", "", tweet_clean$text, fixed =  TRUE)
view(tweet_clean)

library(tidytext)
library(dplyr)
library(ggplot2)

tidy_tweets <- tweet_clean %>% select(text) %>% 
  mutate(linenumber = row_number()) %>% unnest_tokens(word, text)


#Rakamların temizlenmesi
tweet_clean$text <- removeNumbers(tweet_clean$text)

# Kelime bulutu oluşturma
head(tidy_tweets)

tidy_tweets %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + theme_minimal() +
  ggtitle("Tweetlerde en çok kullanılan kelimeler")

library(wordcloud)
tidy_tweets %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 50))


# Dosyayı kaydetme

# write.csv(tweet_clean$text,file = "deneme2.csv")

deneme <- read.table(file.choose(),header=T, sep = ";", encoding= "UTF-8" )
str(deneme)

#Gereksiz kelimeler
#stopwordsler
liste=c(stopwords("en"),"bonus","arkada�lar")

#Gereksiz tekrarlar ve bağlaçların temizlenmesi
deneme$ekonomi = removeWords(deneme$data,liste)


#kelime bulutu
wordcloud(deneme$data,min.freq=4,scale=c(3,1,5),max.words=60)


deneme$data<-deneme$data %>% as_tibble()%>%rename(word=value)

# Polarite
polarite<-sentiment(deneme$data$word)

tablo<-cbind(deneme$data, polarite[,c(3,4)])

ggplot(tablo, aes(word_count, sentiment))+
  geom_point(color="red")+
  geom_hline(yintercept = mean(tablo$sentiment), color= "black", size=1)+
  labs(y ="skor", x = "Kelimelerin durumu") +
  theme_igray()+
  labs()+
  theme(plot.caption = element_text(hjust = 0, face = "italic"))

stat.desc(polarite$sentiment, basic=T) %>% pander()






















