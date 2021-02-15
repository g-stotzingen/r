library(shiny)
library(textreadr)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(tidyr)
library(ggplot2)
library(quanteda)
library(RColorBrewer)

#imports data

survey <- read_document(file="Complete Data Set.docx") 

class_combo <- c(survey)

a <- 80 #number of surveys
b <- 6 #number of questions

my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
    for(i in 1:a){
        my_df[i,z]<- class_combo[i*b+z-b]
    }
}

#junk words
lda_nb_junk <- data_frame(
    word = c('increasing',
             'school',
             'high',
             'am',
             'pm',
             "I'm",
             "You're",
             "you're	",
             "o'clock",
             "o'clock	",
             'classes',
             "favorite",
             'developed',
             'developing',
             'buy',
             'class',
             'day',
             'christmas'),
    lexicon = 'junk'
)

my_junk <- data_frame(
    word = c('increasing',
             'school',
             'high',
             'am',
             'pm',
             "I'm",
             "You're",
             "you're	",
             "o'clock",
             "o'clock	",
             'classes',
             "favorite",
             'developed',
             'developing',
             'buy',
             'class',
             'day',
             'christmas',
             'country',
             "facing",
             "biggest",
             "challenges",
             "challenge"),
    lexicon = 'junk'
)

bigram_junk <- data_frame(
    word = c('increasing',
             'school',
             'high',
             'am',
             'pm',
             "I'm",
             "You're",
             "o'clock",
             'classes',
             "favorite",
             "christmas",
             "list",
             "dinner",
             "shopping"),
    lexicon = 'junk'
)

fre_junk <- data_frame(
    word = c('increasing',
             'school',
             'high',
             'am',
             'pm',
             "I'm",
             "You're",
             "o'clock",
             'classes',
             "favorite",
             "facing",
             "biggest",
             "country",
             "challenge",
             "challenges",
             "christmas",
             "sport",
             "sports",
             "buy",
             "day"),
    lexicon = 'junk'
)

#structure the answers
#=======================================================================
#analyze all and each question one dataframe per question
my_txt_q6 <- my_df$V6
my_txt_q6 <- substr(my_txt_q6, start=1 , stop = 10000)
#analyze all and each question one dataframe per question

#location information
mydf_q6 <- data_frame(survey=1:a, text=my_txt_q6)
#print(mydf_q6)

#Tokenizing and get counts
data(stop_words)
frequencies_tokens_nostop_q6 <- mydf_q6 %>%
    group_by(survey)%>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    anti_join(lda_nb_junk) %>%
    count(word, sort=TRUE)

#=======================================================================
#analyze all and each question one dataframe per question
my_txt_q5 <- my_df$V5
my_txt_q5 <- substr(my_txt_q5, start=1 , stop = 10000)
#analyze all and each question one dataframe per question

#location information
mydf_q5 <- data_frame(survey=1:a, text=my_txt_q5)
#print(mydf_q5)

#Tokenizing and get counts
data(stop_words)
frequencies_tokens_nostop_q5 <- mydf_q5 %>%
    group_by(survey)%>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    anti_join(lda_nb_junk) %>%
    count(word, sort=TRUE)

#=======================================================================
#analyze all and each question one dataframe per question
my_txt_q4 <- my_df$V4
my_txt_q4 <- substr(my_txt_q4, start=1 , stop = 10000)
#analyze all and each question one dataframe per question

#location information
mydf_q4 <- data_frame(survey=1:a, text=my_txt_q4)
#print(mydf_q4)

#Tokenizing and get counts
data(stop_words)
frequencies_tokens_nostop_q4 <- mydf_q4 %>%
    group_by(survey)%>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    anti_join(lda_nb_junk) %>%
    count(word, sort=TRUE)

#=======================================================================
#analyze all and each question one dataframe per question
my_txt_q3 <- my_df$V3
my_txt_q3 <- substr(my_txt_q3, start=1 , stop = 10000)
#analyze all and each question one dataframe per question

#location information
mydf_q3 <- data_frame(survey=1:a, text=my_txt_q3)
#print(mydf_q3)

#Tokenizing and get counts
data(stop_words)
frequencies_tokens_nostop_q3 <- mydf_q3 %>%
    group_by(survey)%>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    anti_join(lda_nb_junk) %>%
    count(word, sort=TRUE)

#=======================================================================
#analyze all and each question one dataframe per question
my_txt_q2 <- my_df$V2
my_txt_q2 <- substr(my_txt_q2, start=1 , stop = 10000)
#analyze all and each question one dataframe per question

#location information
mydf_q2 <- data_frame(survey=1:a, text=my_txt_q2)
#print(mydf_q2)

#Tokenizing and get counts
data(stop_words)
frequencies_tokens_nostop_q2 <- mydf_q2 %>%
    group_by(survey)%>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    anti_join(lda_nb_junk) %>%
    count(word, sort=TRUE)

#=======================================================================
#analyze all and each question one dataframe per question
my_txt_q1 <- my_df$V1
my_txt_q1 <- substr(my_txt_q1, start=1 , stop = 10000)
#analyze all and each question one dataframe per question

#location information
mydf_q1 <- data_frame(survey=1:a, text=my_txt_q1)
#print(mydf_q1)

#Tokenizing and get counts
data(stop_words)
frequencies_tokens_nostop_q1 <- mydf_q1 %>%
    group_by(survey)%>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    anti_join(lda_nb_junk) %>%
    count(word, sort=TRUE)

#Merge questions into a complete data frame
my_df_complete <- bind_rows(
    mutate(frequencies_tokens_nostop_q1,question='question_1'),
    mutate(frequencies_tokens_nostop_q2,question='question_2'),
    mutate(frequencies_tokens_nostop_q3,question='question_3'),
    mutate(frequencies_tokens_nostop_q4,question='question_4'),
    mutate(frequencies_tokens_nostop_q5,question='question_5'),
    mutate(frequencies_tokens_nostop_q6,question='question_6'))

my_df_no_q4 <- bind_rows(
    mutate(frequencies_tokens_nostop_q1,question='question_1'),
    mutate(frequencies_tokens_nostop_q2,question='question_2'),
    mutate(frequencies_tokens_nostop_q3,question='question_3'),
    mutate(frequencies_tokens_nostop_q5,question='question_5'),
    mutate(frequencies_tokens_nostop_q6,question='question_6'))


#####################################################JP new global code########################


developed_df <- my_df %>%
    filter(grepl('developed',tolower(my_df$V6)))

developing_df <- my_df %>%
    filter(grepl('developing',tolower(my_df$V6)))

developed_df$binary <- rep(c("1"), each=nrow(developed_df))

developed_df$text <- paste(toString(developed_df$V1),
                           toString(developed_df$V2),
                           toString(developed_df$V3),
                           toString(developed_df$V4),
                           toString(developed_df$V5))

developing_df$binary <- rep(c("0"), each=nrow(developing_df))

developing_df$text <- paste(toString(developing_df$V1),
                            toString(developing_df$V2),
                            toString(developing_df$V3),
                            toString(developing_df$V4),
                            toString(developing_df$V5))

total_survey_df <- rbind(developed_df,developing_df)[,c("text", "binary")]



######################################## lda funciton##################################
lda_clustering_function <- function(x){
    survey_dtm <- my_df_no_q4 %>%
        cast_dtm(survey, word, n)
    
    survey_lda <- LDA(survey_dtm,k= x, control = list(seed=112))
    survey_lda
    
    survey_wordTopics <- tidy(survey_lda, matrix = "beta")
    survey_wordTopics
    
    survey_top_terms <- survey_wordTopics %>%
        group_by(topic) %>%
        top_n(10, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)
    
    
    response_plot <- survey_top_terms %>%
        mutate(term=reorder(term, beta)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend=FALSE) +
        facet_wrap(~topic, scales = "free") +
        coord_flip()
    
    return(response_plot)
}

# #NaiveBayes model ======================================================
# 
# developed_df <- my_df %>%
#     filter(grepl('developed',tolower(my_df$V6)))
# developing_df <- my_df %>%
#     filter(grepl('developing',tolower(my_df$V6)))
# set.seed(225)
# 
# 
# developed_df$binary <- rep(c("1"), each=nrow(developed_df))
# 
# developed_df$text <- paste(toString(developed_df$V1),
#                            toString(developed_df$V2),
#                            toString(developed_df$V3),
#                            toString(developed_df$V4),
#                            toString(developed_df$V5))
# 
# developing_df$binary <- rep(c("0"), each=nrow(developing_df))
# 
# developing_df$text <- paste(toString(developing_df$V1),
#                             toString(developing_df$V2),
#                             toString(developing_df$V3),
#                             toString(developing_df$V4),
#                             toString(developing_df$V5))
# 
# total_survey_df <- rbind(developed_df,developing_df)[,c("text", "binary")]
# total_survey_df <- total_survey_df[sample(nrow(total_survey_df)),]
# 
# 
# 
# survey_corpus <- corpus(total_survey_df$text) #creating the corpus on the $text var
# srv.dfm <- dfm(survey_corpus, tolower = TRUE) #generating document 
# srv.dfm <- dfm_trim(srv.dfm, min_termfreq = 3, min_docfreq = 0)
# srv.dfm <- dfm_weight(srv.dfm)
# 
# head(srv.dfm)
# 
# srv.dfm.train<-srv.dfm[1:60,]
# srv.dfm.test<-srv.dfm[61:80,]
# 
# #building the Naive Bayes model:
# NB_classifier <- textmodel_nb(srv.dfm.train, total_survey_df$binary[1:60]) # we need to tell which 1 and 0 to use
# NB_classifier
# summary(NB_classifier)
# 
# # predicting the testing data
# pred <- predict(NB_classifier, srv.dfm.test)
# pred


##################bigram function################

Bigram_function <- function(x,ss){
    q5_bigrams <- x %>%
        unnest_tokens(bigram, text, token = "ngrams", n=2)
    
    q5_bigrams %>%
        count(bigram, sort = TRUE)
    
    q5_bigrams_separated <- q5_bigrams %>%
        separate(bigram, c("word1", "word2"), sep = " ")
    
    q5_bigrams_filtered <- q5_bigrams_separated %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word1 %in% bigram_junk$word) %>%
        filter(!word2 %in% stop_words$word) %>%
        filter(!word2 %in% bigram_junk$word)
    
    q5_bigram_counts <- q5_bigrams_filtered %>%
        count(word1, word2, sort = TRUE)
    
    q5_bigram_united <- q5_bigrams_filtered %>%
        unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section
    
    q5_bigram_tf_idf <- q5_bigram_united %>%
        count(survey, bigram) %>%
        bind_tf_idf(bigram, survey, n) %>%
        arrange(desc(tf_idf))
    
    q5_bigram_tf_idf
    
    library(igraph)
    q5_bigram_graph <- q5_bigram_counts %>%
        filter(n>=ss) %>%
        graph_from_data_frame()
    
    library(ggraph)
    bigram_graph_1<-ggraph(q5_bigram_graph, layout = "fr") +
        geom_edge_link(color = 'grey')+
        geom_node_point(shape= 23, color = 'grey')+
        theme_void()+
        geom_node_text(aes(label=name), vjust =1, hjust=1, repel = T)
        
    
    return(bigram_graph_1)
}


#######################################Frequency#################################################3


c<-40
d<-40 
###preparing one developed filtered dataframe per question

#Q1 filtered for developed
my_dev_txt_q1 <- developed_df$V1
my_dev_txt_q1 <- substr(my_dev_txt_q1, start=1 , stop = 10000)
mydf_dev_q1 <- data_frame(survey=1:c, text=my_dev_txt_q1)

#Q2 filtered for developed
my_dev_txt_q2 <- developed_df$V2
my_dev_txt_q2 <- substr(my_dev_txt_q2, start=1 , stop = 10000)
mydf_dev_q2 <- data_frame(survey=1:c, text=my_dev_txt_q2)

#Q3 filtered for developed
my_dev_txt_q3 <- developed_df$V3
my_dev_txt_q3 <- substr(my_dev_txt_q3, start=1 , stop = 10000)
mydf_dev_q3 <- data_frame(survey=1:c, text=my_dev_txt_q3)

#Q4 filtered for developed
my_dev_txt_q4 <- developed_df$V4
my_dev_txt_q4 <- substr(my_dev_txt_q4, start=1 , stop = 10000)
mydf_dev_q4 <- data_frame(survey=1:c, text=my_dev_txt_q4)

#Q5 filtered for developed
my_dev_txt_q5 <- developed_df$V5
my_dev_txt_q5 <- substr(my_dev_txt_q5, start=1 , stop = 10000)
mydf_dev_q5 <- data_frame(survey=1:c, text=my_dev_txt_q5)

#Q6 filtered for developed
my_dev_txt_q6 <- developed_df$V6
my_dev_txt_q6 <- substr(my_dev_txt_q6, start=1 , stop = 10000)
mydf_dev_q6 <- data_frame(survey=1:c, text=my_dev_txt_q6)

###preparing one developing filtered dataframe per question

#Q1 filtered for developing
my_deving_txt_q1 <- developing_df$V1
my_deving_txt_q1 <- substr(my_deving_txt_q1, start=1 , stop = 10000)
mydf_deving_q1 <- data_frame(survey=1:d, text=my_deving_txt_q1)

#Q2 filtered for developing
my_deving_txt_q2 <- developing_df$V2
my_deving_txt_q2 <- substr(my_deving_txt_q2, start=1 , stop = 10000)
mydf_deving_q2 <- data_frame(survey=1:d, text=my_deving_txt_q2)

#Q3 filtered for developing
my_deving_txt_q3 <- developing_df$V3
my_deving_txt_q3 <- substr(my_deving_txt_q3, start=1 , stop = 10000)
mydf_deving_q3 <- data_frame(survey=1:d, text=my_deving_txt_q3)

#Q4 filtered for developing
my_deving_txt_q4 <- developing_df$V4
my_deving_txt_q4 <- substr(my_deving_txt_q4, start=1 , stop = 10000)
mydf_deving_q4 <- data_frame(survey=1:d, text=my_deving_txt_q4)

#Q5 filtered for developing
my_deving_txt_q5 <- developing_df$V5
my_deving_txt_q5 <- substr(my_deving_txt_q5, start=1 , stop = 10000)
mydf_deving_q5 <- data_frame(survey=1:d, text=my_deving_txt_q5)


#Q6 filtered for developing
my_deving_txt_q6 <- developing_df$V6
my_deving_txt_q6 <- substr(my_deving_txt_q6, start=1 , stop = 10000)
mydf_deving_q6 <- data_frame(survey=1:d, text=my_deving_txt_q6)



histogram_function_dev <- function(dev,gg){
    #Q1 developed
    dev_freq_hist <- dev %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        anti_join(fre_junk) %>%
        count(word, sort=TRUE) %>%
        top_n(gg)%>%
        mutate(word = reorder(word,n ))%>%
        ggplot(aes(word, n))+
        geom_col(fill = "#016C5A")+
        theme_minimal()+
        xlab(NULL)+
        coord_flip()+
        ggtitle('Developed')
    
    return(dev_freq_hist)
    
}

histogram_function_deving <- function(deving,gg){
    #Q1 developed
    deving_freq_hist <- deving %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        anti_join(fre_junk) %>%
        count(word, sort=TRUE) %>%
        top_n(gg)%>%
        mutate(word = reorder(word,n ))%>%
        ggplot(aes(word, n))+
        geom_col(fill = "#016C5A")+
        theme_minimal()+
        xlab(NULL)+
        coord_flip()+
        ggtitle('Developing')
    
    return(deving_freq_hist)
    
}
#######################################Continent Economy########################################

# plotting tokens to see distribution by continent and economic status
Continent <- function() {
    cont_dist <-  mydf_q6 %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        filter(word %in% c('europe', 'northamerica', 'latinamerica','africa', 'asia')) %>%
        count(word, sort=TRUE) %>%
        mutate(dist = round((n/sum(n) * 100)))
    
    
    response_plot <- ggplot(cont_dist, aes(x = '', y = n, fill = word)) +
        theme_void() +
        scale_fill_brewer(palette="PuBuGn") +
        geom_bar(stat = 'identity') +
        coord_polar(theta = 'y', start = 0) +
        geom_text(aes(label = paste(dist,"%")),
                  position = position_stack(vjust = 0.5)) +
        labs(x = NULL,
             y = NULL,
             title = 'Geographic Distribution')
    
    return(response_plot)
}




# plotting tokens to see distribution by economic status
Economy <- function() {
    stat_dist <-  mydf_q6 %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        filter(word %in% c('developing', 'developed')) %>%
        count(word) %>%
        mutate(dist = round((n/sum(n) * 100)))
    
    response_plot <- ggplot(stat_dist, aes(x='', y=n, fill=word)) +
        theme_void() +
        scale_fill_brewer(palette="Paired") +
        geom_bar(stat = 'identity') +
        coord_polar(theta = 'y', start = 0) +
        geom_text(aes(label = paste(dist,"%")),
                  position = position_stack(vjust = 0.5)) +
        labs(x = NULL,
             y = NULL,
             title = 'Developed v Developing Economies')
    
    return(response_plot)
}

####################################Country bigram sample###########################################

firstpagebigram <- function(x){
    q6_bigrams <- mydf_q6 %>%
        unnest_tokens(bigram, text, token = "ngrams", n=2)
    
    q6_bigrams %>%
        count(bigram, sort = TRUE)
    
    q6_bigrams_separated <- q6_bigrams %>%
        separate(bigram, c("word1", "word2"), sep = " ")
    
    q6_bigrams_filtered <- q6_bigrams_separated %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word1 %in% bigram_junk$word) %>%
        filter(!word2 %in% stop_words$word) %>%
        filter(!word2 %in% bigram_junk$word)
    
    q6_bigram_counts <- q6_bigrams_filtered %>%
        count(word1, word2, sort = TRUE)
    
    q6_bigram_united <- q6_bigrams_filtered %>%
        unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section
    
    q6_bigram_graph <- q6_bigram_counts %>%
        filter(n>=x) %>%
        graph_from_data_frame()
    
    q6_bigram_graph
    
    ggraph(q6_bigram_graph, layout = "fr") +
        geom_edge_link(color='grey')+
        geom_node_point(shape=23, color='grey')+
        theme_void()+
        geom_node_text(aes(label=name), vjust =1, hjust=1, repel = T)
}

###################################tfidf_developedVSdeveloping##########################################

tfidf_developedVSdeveloping <- function(x){
    total_survey_df <- rbind(developed_df,developing_df)[,c("text", "binary")]
    
    frequencies_totalSurvey_split <- total_survey_df %>%
        group_by(binary)%>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        anti_join(my_junk) %>%
        count(word, sort=TRUE)
    
    frequencies_totalSurvey_split <- frequencies_totalSurvey_split %>%
        bind_tf_idf(word, binary, n)
    
    frequencies_totalSurvey_split %>%
        arrange(desc(tf_idf))
    
    response_plot <- frequencies_totalSurvey_split %>%
        arrange(desc(tf_idf)) %>%
        mutate(word=factor(word, levels=rev(unique(word)))) %>%
        group_by(binary) %>%
        top_n(x) %>%
        ungroup %>%
        ggplot(aes(word, tf_idf, fill=binary))+
        geom_col(show.legend=FALSE)+
        labs(x=NULL, y="tf-idf")+
        facet_wrap(~binary, ncol=2, scales="free")+
        coord_flip()
    
    return(response_plot)
}

#####################################Naive Bayes########################################

naive_bayes <- function(){
    set.seed(225)
    
    
    total_survey_df <- total_survey_df[sample(nrow(total_survey_df)),]
    
    survey_corpus <- corpus(total_survey_df$text) #creating the corpus on the $text var
    srv.dfm <- dfm(survey_corpus, tolower = TRUE) #generating document 
    srv.dfm <- dfm_trim(srv.dfm, min_termfreq = 3, min_docfreq = 0)
    srv.dfm <- dfm_weight(srv.dfm)
    
    head(srv.dfm)
    
    srv.dfm.train<-srv.dfm[1:60,]
    srv.dfm.test<-srv.dfm[61:80,]
    
    #building the Naive Bayes model:
    NB_classifier <- textmodel_nb(srv.dfm.train, total_survey_df$binary[1:60]) # we need to tell which 1 and 0 to use
    NB_classifier
    summary(NB_classifier)
    
    # predicting the testing data
    pred <- predict(NB_classifier, srv.dfm.test)
    pred
    
    actual <- total_survey_df[61:80,]$binary
    predicted <- pred
    tab_class <- data.frame(table(actual, predicted))
    tab_class
    
    response_plot <- ggplot(data =  tab_class, mapping = aes(x = actual, y = predicted)) +
        geom_tile(aes(fill = Freq), colour = "white") +
        geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1,size=10) +
        scale_fill_gradient(low = "#becae1", high = "#68a9d0") +
        theme_bw() + theme(legend.position = "none")
    
    return(response_plot)
}

###############################word lcoud#####################################
wordc <- function(){
    library(wordcloud2)
    thanks <- tibble('word'=c('ThankYou', 'ThankYou', 'ThankYou', 'ThankYou', 'ThankYou',
                              'Spasibo', 'Spasibo', 'Spasibo', 'Spasibo', 'Spasibo',
                              'Gracias', 'Gracias', 'Gracias', 'Gracias', 'Gracias',
                              'Danke', 'Danke', 'Danke', 'Danke', 'Danke',
                              'Akpe', 'Akpe', 'Akpe', 'Akpe', 'Akpe',
                              'Dhanyavaad', 'Dhanyavaad', 'Dhanyavaad', 'Dhanyavaad', 'Dhanyavaad',
                              'Dziekuje', 'Dziekuje', 'Dziekuje', 'Dziekuje', 'Dziekuje',
                              "Xiexie","Xiexie","Xiexie","Xiexie","Xiexie"
    ))
    
    name <- tibble('word'=c('Georg', 'Georg', 'Georg',
                            'Kapilesh', 'Kapilesh', 'Kapilesh',
                            'JP', 'JP', 'JP', 'Esie', 'Esie', 'Esie',
                            'Galina', 'Galina', 'Galina',
                            'AnnTing', 'AnnTing', 'AnnTing'))
    
    team <- tibble('word'=c('Team7', 'Team7', 'Team7', 'Team7', 'Team7',
                            'Team7', 'Team7', 'Team7', 'Team7', 'Team7'))
    
    end_cloud <- bind_rows(thanks, name, team)
    
    cloud <- end_cloud %>%
        count(word, sort = T)
    
    wordcloud2(data = cloud, minRotation = pi/6, maxRotation = -pi/4, minSize = 1,
               rotateRatio = 1, size = 0.5)
}







































































# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

    output$distPlot2 <- renderPlot({

        sell <- input$Question
        if(sell == "q1"){
            dis <- mydf_dev_q1
        }
        else if (sell == "q2"){
            dis <- mydf_dev_q2
        }
        else if (sell == "q3"){
            dis <- mydf_dev_q3
        }
        else if (sell == "q4"){
            dis <- mydf_dev_q4
        }
        else if (sell == "q5"){
            dis <- mydf_dev_q5
        }
        else {
            dis <- mydf_dev_q6
        }
        
        histogram_function_dev(dis,input$gg)

    })

    output$distPlot3 <- renderPlot({
    
        sel <- input$Question
        if(sel == "q1"){
            dis3 <- mydf_deving_q1
        }
        else if (sel == "q2"){
            dis3 <- mydf_deving_q2
        }
        else if (sel == "q3"){
            dis3 <- mydf_deving_q3
        }
        else if (sel == "q4"){
            dis3 <- mydf_deving_q4
        }
        else if (sel == "q5"){
            dis3 <- mydf_deving_q5
        }
        else {
            dis3 <- mydf_deving_q6
        }

        histogram_function_deving(dis3,input$gg)

    })
    
    output$distPlot4 <- renderPlot({
        
        sel2 <- input$Question2
        if(sel2 == "q1"){
            dis2 <- mydf_dev_q1
        }
        else if (sel2 == "q2"){
            dis2 <- mydf_dev_q2
        }
        else if (sel2 == "q3"){
            dis2 <- mydf_dev_q3
        }
        else if (sel2 == "q4"){
            dis2 <- mydf_dev_q4
        }
        else if (sel2 == "q5"){
            dis2 <- mydf_dev_q5
        }
        else {
            dis2 <- mydf_dev_q6
        }
        
        Bigram_function(dis2,input$ss)
        
    })
    
    output$distPlot5 <- renderPlot({
        
        sel3 <- input$Question2
        if(sel3 == "q1"){
            dis4 <- mydf_deving_q1
        }
        else if (sel3 == "q2"){
            dis4 <- mydf_deving_q2
        }
        else if (sel3 == "q3"){
            dis4 <- mydf_deving_q3
        }
        else if (sel3 == "q4"){
            dis4 <- mydf_deving_q4
        }
        else if (sel3 == "q5"){
            dis4 <- mydf_deving_q5
        }
        else {
            dis4 <- mydf_deving_q6
        }
        
        Bigram_function(dis4,input$ss)
        
    })
    
    output$wordcloud <- renderWordcloud2({
        wordc()
    })
    
    output$confusionplot <- renderPlot({
        naive_bayes()
    })
    
    output$tfidfplot <- renderPlot({
        tfidf_developedVSdeveloping(input$bb)
    })
    
    output$value <- renderPlot({ 
        lda_clustering_function(input$Clusters)
    })
    
    output$pieplot <- renderPlot({ 
        sel3<-input$plotType
        
        if(sel3=="eco"){
            Economy()
        }
        else if(sel3 =="Co"){
            Continent()
        }
        else{
            firstpagebigram(input$aa)
        }
    })

})
