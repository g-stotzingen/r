#Import libraries
library(dplyr)
library(tidyverse)
library(tidytext)
library(stringr)
library(ggplot2)
library(scales)
library(wordcloud)
library(reshape2)
library(textreadr)
library(tidyr)
library(ggraph)
library(igraph)

#Read in the Data
Bangkok <- read_document(file="C:/Users/Georg/Desktop/Hult/MSBA/Text Analytics/Inisght Report/Bangkok.txt")
London <- read_document(file="C:/Users/Georg/Desktop/Hult/MSBA/Text Analytics/Inisght Report/London.txt")
Paris <- read_document(file="C:/Users/Georg/Desktop/Hult/MSBA/Text Analytics/Inisght Report/Paris.txt")
Dubai <- read_document(file="C:/Users/Georg/Desktop/Hult/MSBA/Text Analytics/Inisght Report/Dubai.txt")
Singapore <- read_document(file="C:/Users/Georg/Desktop/Hult/MSBA/Text Analytics/Inisght Report/Singapore.txt")

#####General data preparation#####

#Prepare a total data frame
ConnectCombo <- c(Bangkok, London, Paris, Dubai, Singapore)
my_connect <- data_frame(text=ConnectCombo)
Total_df <- my_connect

#Prepare individual data frames
Bangkok_df<- data_frame(text=Bangkok)
London_df<- data_frame(text=London)
Paris_df<- data_frame(text=Paris)
Dubai_df<- data_frame(text=Dubai)
Singapore_df<- data_frame(text=Singapore)

#Prepare a my_junk dictionary and stop words
my_junk <- data_frame(
  word = c('london','tourist','tourists', 'bangkok', 'paris', 'singapore', 'dubai', 't.co','IT',
           'tourism', 'al','1', '2', '3', '4', '5', 'de', 'du', 'la', 'le', 'road',
           'st', 'edit', 'visitors', 'million', 'total', 'main', 'people', '2011',
           '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', 'visitor',
           'visits', 'des'),
  lexicon = 'junk'
)

data(stop_words)

#Creating tidy data frames

Total_tidy <- my_connect %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  count(word, sort=TRUE)
  
Bangkok_tidy <- Bangkok_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  count(word, sort=TRUE)

London_tidy <- London_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  count(word, sort=TRUE)

Paris_tidy <- Paris_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  count(word, sort=TRUE)

Dubai_tidy <- Dubai_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  count(word, sort=TRUE)

Singapore_tidy <- Singapore_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  count(word, sort=TRUE)



#####Creating frequency histogramms#####

#Creating token frequency histogram of Total dataframe
Total_freq_hist <- my_connect %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  count(word, sort=TRUE) %>%
  top_n(20)%>%
  mutate(word = reorder(word,n ))%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  ggtitle('Total')

print(Total_freq_hist)

#Token frequency histograms of Bangkok
Bangkok_freq_hist <- Bangkok_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  count(word, sort=TRUE) %>%
  top_n(20)%>%
  mutate(word = reorder(word,n ))%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  ggtitle('Bangkok')

print(Bangkok_freq_hist)

#Token frequency histograms of London
London_freq_hist <- London_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  count(word, sort=TRUE) %>%
  top_n(19)%>%
  mutate(word = reorder(word,n ))%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  ggtitle('London')

print(London_freq_hist)

#Token frequency histograms of Paris
Paris_freq_hist <- Paris_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  count(word, sort=TRUE) %>%
  top_n(20)%>%
  mutate(word = reorder(word,n ))%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  ggtitle('Paris')

print(Paris_freq_hist)

#Token frequency histograms of Dubai
Dubai_freq_hist <- Dubai_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  count(word, sort=TRUE) %>%
  top_n(20)%>%
  mutate(word = reorder(word,n ))%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  ggtitle('Dubai')

print(Dubai_freq_hist)

#Token frequency histograms of Singapore
Singapore_freq_hist <- Singapore_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  count(word, sort=TRUE) %>%
  top_n(20)%>%
  mutate(word = reorder(word,n ))%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  ggtitle('Singapore')

print(Singapore_freq_hist)


######Correlograms#####

#Preparing the dataframe for Bangkok
frequency <- bind_rows(mutate(Bangkok_tidy, city="Bangkok"),
                       mutate(London_tidy, city= "London"),
                       mutate(Paris_tidy, city= "Paris"),
                       mutate(Dubai_tidy, city= "Dubai"),
                       mutate(Singapore_tidy, city= "Singapore")
)%>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(city, word) %>%
  group_by(city) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(city, proportion) %>%
  gather(city, proportion, `London`, `Paris`, `Dubai`, `Singapore` )

#Plotting the correlogram for Bangkok
ggplot(frequency, aes(x=proportion, y=`Bangkok`, 
                      color = abs(`Bangkok`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~city, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Bangkok", x=NULL)

#Preparing the dataframe for London
frequency <- bind_rows(mutate(Bangkok_tidy, city="Bangkok"),
                       mutate(London_tidy, city= "London"),
                       mutate(Paris_tidy, city= "Paris"),
                       mutate(Dubai_tidy, city= "Dubai"),
                       mutate(Singapore_tidy, city= "Singapore")
)%>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(city, word) %>%
  group_by(city) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(city, proportion) %>%
  gather(city, proportion, `Bangkok`, `Paris`, `Dubai`, `Singapore` )

#Plotting the correlogram for London
ggplot(frequency, aes(x=proportion, y=`London`, 
                      color = abs(`London`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~city, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "London", x=NULL)

#Preparing the dataframe for Dubai
frequency <- bind_rows(mutate(Bangkok_tidy, city="Bangkok"),
                       mutate(London_tidy, city= "London"),
                       mutate(Paris_tidy, city= "Paris"),
                       mutate(Dubai_tidy, city= "Dubai"),
                       mutate(Singapore_tidy, city= "Singapore")
)%>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(city, word) %>%
  group_by(city) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(city, proportion) %>%
  gather(city, proportion, `Bangkok`, `Paris`, `London`, `Singapore` )

#Plotting the correlogram for Dubai
ggplot(frequency, aes(x=proportion, y=`Dubai`, 
                      color = abs(`Dubai`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~city, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Dubai", x=NULL)


#####Sentiment Analysis#####

#Bing Counts positiv vs negative

#Bing sentiment Total
Total_bing_counts <- Total_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

Total_bing_counts

Total_bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip() +
  ggtitle('Total')


#Bing sentiment Bangkok
Bangkok_bing_counts <- Bangkok_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

Bangkok_bing_counts

Bangkok_bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip() +
  ggtitle('Bangkok')

#Bing sentiment London
London_bing_counts <- London_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

London_bing_counts

London_bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip() +
  ggtitle('London')

#Bing sentiment Paris
Paris_bing_counts <- Paris_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

Paris_bing_counts

Paris_bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip() +
  ggtitle('Paris')

#Bing sentiment Dubai
Dubai_bing_counts <- Dubai_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

Dubai_bing_counts

Dubai_bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip() +
  ggtitle('Dubai')

#Bing sentiment Singapore
Singapore_bing_counts <- Singapore_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

Singapore_bing_counts

Singapore_bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip() +
  ggtitle('Singapore')

######Word Clouds#####

#Word Cloud for the Total Data
Total_tidy %>%
  with(wordcloud(word, n, max.words = 100))

#Word Cloud for Bangkok
Bangkok_tidy %>%
  with(wordcloud(word, n, max.words = 100))

#Word Cloud for London
London_tidy %>%
  with(wordcloud(word, n, max.words = 100))

#Word Cloud for Paris
Paris_tidy %>%
  with(wordcloud(word, n, max.words = 100))

#Word Cloud for Dubai
Dubai_tidy %>%
  with(wordcloud(word, n, max.words = 100))

#Word Cloud for Singapore
Singapore_tidy %>%
  with(wordcloud(word, n, max.words = 100))

#####Word clouds with sentiments#####

#Sentiment word cloud for Total
Total_tidy %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale = c(0.6,0.6),
                   fixed.asp=T, title.size=1, title.colors=c("red","blue"),
                   title.bg.colors=c("grey70","grey90"))

Total_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale = c(0.6,0.6),
                   fixed.asp=T, title.size=1, title.colors=c("red","blue"),
                   title.bg.colors=c("grey70","grey90"))

#Sentiment word cloud for Bangkok
Bangkok_tidy %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale = c(0.6,0.6),
                   fixed.asp=T, title.size=1, title.colors=c("red","blue"),
                   title.bg.colors=c("grey70","grey90"))

Bangkok_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale = c(0.6,0.6),
                   fixed.asp=T, title.size=1,title.colors=c("red","blue"),
                   title.bg.colors=c("grey70","grey90"))

#Sentiment word cloud for London
London_tidy %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale = c(0.6,0.6),
                   fixed.asp=T, title.size=1, title.colors=c("red","blue"),
                   title.bg.colors=c("grey70","grey90"))

London_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale = c(0.6,0.6),
                   fixed.asp=T, title.size=1,title.colors=c("red","blue"),
                   title.bg.colors=c("grey70","grey90"))

#Sentiment word cloud for Paris
Paris_tidy %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale = c(0.6,0.6),
                   fixed.asp=T, title.size=1, title.colors=c("red","blue"),
                   title.bg.colors=c("grey70","grey90"))

Paris_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale = c(0.6,0.6),
                   fixed.asp=T, title.size=1,title.colors=c("red","blue"),
                   title.bg.colors=c("grey70","grey90"))

#Sentiment word cloud for Dubai
Dubai_tidy %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale = c(0.6,0.6),
                   fixed.asp=T, title.size=1, title.colors=c("red","blue"),
                   title.bg.colors=c("grey70","grey90"))

Dubai_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale = c(0.6,0.6),
                   fixed.asp=T, title.size=1,title.colors=c("red","blue"),
                   title.bg.colors=c("grey70","grey90"))

#Sentiment word cloud for Singapore
Singapore_tidy %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale = c(0.6,0.6),
                   fixed.asp=T, title.size=1, title.colors=c("red","blue"),
                   title.bg.colors=c("grey70","grey90"))

Singapore_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale = c(0.6,0.6),
                   fixed.asp=T, title.size=1,title.colors=c("red","blue"),
                   title.bg.colors=c("grey70","grey90"))


#####TF-IDF#####

#TF-IDF data preparations
Total_data <- bind_rows(mutate(Bangkok_tidy, city="Bangkok"),
                       mutate(London_tidy, city= "London"),
                       mutate(Paris_tidy, city= "Paris"),
                       mutate(Dubai_tidy, city= "Dubai"),
                       mutate(Singapore_tidy, city= "Singapore")
)

total_words <- Total_data %>%
  group_by(city) %>%
  summarize(total=sum(n))

data_words <- left_join(Total_data, total_words)

print(data_words)

ggplot(data_words, aes(n/total, fill = city))+
  geom_histogram(show.legend=FALSE)+
  stat_bin(bins = 60) +
  xlim(NA, 0.01) +
  facet_wrap(~city, ncol=2, scales="free_y")

#ZIPF's law
freq_by_rank <- data_words %>%
  group_by(city) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank

#Plotting ZIPF's Law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=city))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

#TF_IDF
data_words <- data_words %>%
  bind_tf_idf(word, city, n)

data_words 

data_words %>%
  arrange(desc(tf_idf))

#Plotting TF_IDF
data_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(city) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=city))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~city, ncol=2, scales="free")+
  coord_flip()

#####Bigrams#####

#Preparing data frames
Total_bigrams_all <- my_connect %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)
  
Total_bigrams_separated <- Total_bigrams_all %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  
Total_bigrams_filtered <- Total_bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
  
Total_bigram_counts <- Total_bigrams_filtered %>%
    count(word1, word2, sort = TRUE)

Total_bigram_counts

#Bangkok
Bangkok_bigrams_all <- Bangkok_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

Bangkok_bigrams_separated <- Bangkok_bigrams_all %>%
  separate(bigram, c("word1", "word2"), sep = " ")

Bangkok_bigrams_filtered <- Bangkok_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

Bangkok_bigram_counts <- Bangkok_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

Bangkok_bigram_counts

#London
London_bigrams_all <- London_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

London_bigrams_separated <- London_bigrams_all %>%
  separate(bigram, c("word1", "word2"), sep = " ")

London_bigrams_filtered <- London_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

London_bigram_counts <- London_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

London_bigram_counts

#Paris
Paris_bigrams_all <- Paris_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

Paris_bigrams_separated <- Paris_bigrams_all %>%
  separate(bigram, c("word1", "word2"), sep = " ")

Paris_bigrams_filtered <- Paris_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

Paris_bigram_counts <- Paris_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

Paris_bigram_counts

#Dubai
Dubai_bigrams_all <- Dubai_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

Dubai_bigrams_separated <- Dubai_bigrams_all %>%
  separate(bigram, c("word1", "word2"), sep = " ")

Dubai_bigrams_filtered <- Dubai_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

Dubai_bigram_counts <- Dubai_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

Dubai_bigram_counts

#Singapore
Singapore_bigrams_all <- Singapore_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

Singapore_bigrams_separated <- Singapore_bigrams_all %>%
  separate(bigram, c("word1", "word2"), sep = " ")

Singapore_bigrams_filtered <- Singapore_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

Singapore_bigram_counts <- Singapore_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

Singapore_bigram_counts

#Visualizing Bigram Networks

#Total Bigram Network
Total_bigram_graph <- Total_bigram_counts %>%
  filter(n>5) %>%
  graph_from_data_frame()

Total_bigram_graph

ggraph(Total_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#Bangkok Bigram Network
Bangkok_bigram_graph <- Bangkok_bigram_counts %>%
  filter(n>3) %>%
  graph_from_data_frame()

Bangkok_bigram_graph

ggraph(Bangkok_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#London Bigram Network
London_bigram_graph <- London_bigram_counts %>%
  filter(n>2) %>%
  graph_from_data_frame()

London_bigram_graph

ggraph(London_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#Paris Bigram Network
Paris_bigram_graph <- Paris_bigram_counts %>%
  filter(n>3) %>%
  graph_from_data_frame()

Paris_bigram_graph

ggraph(Paris_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#Dubai Bigram Network
Dubai_bigram_graph <- Dubai_bigram_counts %>%
  filter(n>2) %>%
  graph_from_data_frame()

Dubai_bigram_graph

ggraph(Dubai_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), size=6, vjust =1, hjust=1)

#Singapore Bigram Network
Singapore_bigram_graph <- Singapore_bigram_counts %>%
  filter(n>3) %>%
  graph_from_data_frame()

Singapore_bigram_graph

ggraph(Singapore_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

