#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(textreadr)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(quanteda)
library(RColorBrewer)
library(scales)
library(caret)
setwd("C:/Users/Georg/Desktop/Hult/MSBA/Text Analytics/Group/Final Shinies/Naive Baise") #put your path here
survey <- read_document(file="Complete Data Set_v3.docx") 

class_combo <- c(survey)

a <- 80 #number of surveys
b <- 6 #number of questions

my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
    for(i in 1:a){
        my_df[i,z]<- class_combo[i*b+z-b]
    }
}

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
set.seed(225)

total_survey_df <- total_survey_df[sample(nrow(total_survey_df)),]

survey_corpus <- corpus(total_survey_df$text) #creating the corpus on the $text var
srv.dfm <- dfm(survey_corpus, tolower = TRUE) #generating document 
srv.dfm <- dfm_trim(srv.dfm, min_termfreq = 3, min_docfreq = 0)
srv.dfm <- dfm_weight(srv.dfm)

srv.dfm.train<-srv.dfm[1:60,]
srv.dfm.test<-srv.dfm[61:80,]

#building the Naive Bayes model:
NB_classifier <- textmodel_nb(srv.dfm.train, total_survey_df$binary[1:60]) # we need to tell which 1 and 0 to use
NB_classifier


naive_bayes <- function(newData){
    
    new_survey_df<- rbind(total_survey_df, c(newData))
    newCorpus <- corpus(new_survey_df$text) #creating the corpus on the $text var
    nC.dfm <- dfm(newCorpus, tolower = TRUE) #generating document 
    nC.dfm <- dfm_trim(nC.dfm, min_termfreq = 3, min_docfreq = 0)
    nC.dfm <- dfm_weight(nC.dfm)
    
    pred <- predict(NB_classifier, nC.dfm)
    
    if (toString(tail(pred, 1)) == '1') {
        return('Developed')
    }
    else{
        return('Developing')
    }
    
}




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$value <- renderText({ 
            naive_bayes(newData=paste(toString(input$q1),
                                      toString(input$q2),
                                      toString(input$q3),
                                      toString(input$q4),
                                      toString(input$q5)))
        })

})
