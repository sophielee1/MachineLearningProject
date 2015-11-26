rm(list=ls())

##### Set up ####
setwd("/Users/sophie/Documents/Data/")
base.path=getwd()
library("openNLPmodels.en")
library("NLP")
library("openNLP")
library(plyr) # ddply function in the parse_sentences function
library(tidyr) #spread function in the parse_sentences function
library(stringr) #string matching
library(tm) #stem words, etc.
library(magrittr)


##### Load data ####

icews <- read.csv(paste0(base.path, "/all_protest_from2001_2014_20151111.csv"))
icews$headline = as.character(icews$headline)
icews$text = as.character(icews$text)



##### helper function ####
parse_sentences <- function(text, aggregation="sentence") {
  
  nouns <- c("NN")
  nouns_proper <- c("NNP","NNPS")
  verbs <-c("VB","VBD","VBG","VBN","VBP","VBZ")
  
  document <- as.String(text)
  document_annotations <- annotate(document, pipeline)
  document_annotations_df <- as.data.frame(document_annotations)
  document_annotations_df$features_type <- names(unlist(document_annotations$features,recursive=FALSE) )
  document_annotations_df$features <- unlist(document_annotations$features,recursive=FALSE)
  document_annotations_df$features <- unlist(sapply(document_annotations$features, FUN=function(x) x[[1]][[1]]))
  
  document_annotations_df$features_category <- NA
  condition <- document_annotations_df$features %in% c("location","organization","date","person")
  document_annotations_df$features_category[condition] <- document_annotations_df$features[condition]
  
  condition <- document_annotations_df$features %in% nouns
  document_annotations_df$features_category[condition] <- "nouns_regular"
  
  condition <- document_annotations_df$features %in% nouns_proper
  document_annotations_df$features_category[condition] <- "nouns_proper"
  
  condition <- document_annotations_df$features %in% verbs
  document_annotations_df$features_category[condition] <- "verbs"
  
  document_annotations_df$string <- as.character(document[document_annotations]) #Have to convert this from a string
  condition <- document_annotations_df$type=="sentence"
  document_annotations_df$sentence_number <- as.numeric(cut(document_annotations_df$start,
                                                            breaks=c(1,document_annotations_df$end[condition]), include.lowest = T, ordered_result = T, right=F))
  
  #class(document_annotations_df$features_category)
  
  setence_strings <- subset(document_annotations_df, type=="sentence")
  
  document_annotations_df <- subset(document_annotations_df,!is.na(features_category))
  
  sentence_annotations_df <- ddply(document_annotations_df,
                                   .variables=c("features_category") ,  mutate,
                                   features_collapsed = paste(string, collapse="; " ) )
  
  sentence_annotations_df <- ddply(document_annotations_df,
                                   .variables=c("sentence_number","features_category") ,  mutate,
                                   features_collapsed = paste(string, collapse="; " ) )
  
  subset_sentence_feature <- subset(sentence_annotations_df, !is.na(features_category) & features_category !="NA",  
                                    select= c(sentence_number,features_category,features_collapsed) )
  
  condition <- !duplicated(subset_sentence_feature[,c("sentence_number","features_category")])
  
  subset_sentence_feature <- subset_sentence_feature[condition,c("sentence_number","features_category","features_collapsed")]
  
  subset_sentence_feature <- spread(data=subset_sentence_feature, key=features_category , value=features_collapsed  )
  
  sentence_number<-unique(subset_sentence_feature$sentence_number)
  
  subset_sentence_feature$sentence_string <-setence_strings$string[sentence_number]
  
  return(subset_sentence_feature)
}



# Create pipeline: Combine individual annotators into a single "pipeline".
person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")
date_ann <- Maxent_Entity_Annotator(kind = "date")
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()

pipeline <- list(sent_token_annotator,word_token_annotator, pos_tag_annotator, # we can add more here
                 person_ann,  # person nouns
                 location_ann, # location nouns
                 organization_ann, # org nouns
                 date_ann)


##### STEP 1: (Pre-treatment I) Cut out the parts citing sources ####





#1. remove the parts that contain the location of news reporting agencies
sources<-c("AFP","Reuters","AP","in Chinese", "web site")#stopwords
#2. remove the last sentence if it contains the word "source:"
indicators<-c("Description of","Source")

data=icews

newvar<-rep(NA, nrow(data))
# names(newvar)<-"newvar"
for(i in 1:length(newvar)){
  sentences<- parse_sentences(data$text[i]) 
  cell<-sentences[1,ncol(sentences)]
  
  ## IF
  TF<-str_detect(cell,sources)
  if(length(which(TF==TRUE))>0){
    no<-which(TF==TRUE)
        if(length(no)>1){   # if no is longer than 1, use the first case
        no<-no[1]
        }
    src<-sources[no]
    split <-str_split(cell,src)
    newcell<-split[[1]][2]
    sentences[1,ncol(sentences)]<-newcell
  }
  ### End of IF 1
  
  
  
  ## IF 2
  
  cell<-sentences[nrow(sentences),ncol(sentences)]
  TF<-str_detect(cell,indicators)
  if(length(which(TF==TRUE))>0){
 
    no<-which(TF==TRUE)
        if(length(no)>1){  # if no is longer than 1, use the first case
        no<-no[1]
        }
    indicator<-indicators[no]
    split <-str_split(cell,indicator)
    newcell<-split[[1]][1]
        if(length(newcell)==0){  #if the indicator words appears at the beginning
        newcell<-"\n\n"
        }
    sentences[nrow(sentences),ncol(sentences)]<-newcell
  }
  
  ## End of IF 2
#   sentences=sentences[1:nrow(sentences)-1,]
  
  coerced<-as.String(sentences[,ncol(sentences)])
  newvar[i]<-as.character(coerced)
}



data=cbind(data,newvar)




##### Step 2: (Pre-Treatment II) stem words & get rid of special characters etc. #####

# icews$text = str_replace_all(icews$text,"\\?\\???","") %>% # rid of question marks
#    str_replace_all("[","") %>%  # rid of [Desciption of source: ...]
#    str_replace_all("(","") %>% # rid of (2500 chrts; jjbd1031a)
#   str_replace_all("]","") %>%  # rid of [Desciption of source: ...]
#   str_replace_all(")","") %>% # rid of (2500 chrts; jjbd1031a)  
#    str_replace_all("http\\S+\\s*", "") %>%  # rid of http
#    str_replace_all("Attachments:", "") %>%  # we need sentences
#   str_replace_all("Guangzhou$", "") %>%
#   str_replace_all("?", "") %>%
#   str_replace_all("\n\n.*pdf$", " ") %>% # get rid of ending with pdf
#   str_replace_all("\n\nsai.*?$", "") %>% # row 26, 68
#   str_replace_all("\n\nbur.*?$", "") %>% # row 589 -> \n\nbur/slb/dwa
#   str_replace_all(".com","") %>%
#   str_replace_all("Apr.", "April") %>% # row632
#    str_replace_all("\\$|\\=|\\(|\\)|-|--|'|\"|\\.\\.\\.|_|______|_____|:|\\*", " ")  # rid of symbols (always the last)

### need to run the parse_sentences function below first
# df <- list()
# 
# for (i in 1:642){
#   text=icews$text[i]
#   df[[i]] <- parse_sentences(icews$text[i])
#   # Anhui (human: Zhejiang, because workers from Anhui protested in Zhejing province)
#   print(i)
# }
# 
# View(df[[11]])
# icrews.df = lapply(df, as.data.frame())
# write.csv(icrews.df, file = "/Users/howardliu/dropbox/Chinese NLP project/Data/icewslists.csv")




##### STEP 2: Run NLP -> LDA -> NLP ####
test<-parse_sentences(newvar[4])

# delete China
#get k

string<-GetCorpus(newvar[4])
dtm<-DocumentTermMatrix(string)

testfinal<-parse_sentences(testdata[,1])
testdata<-as.data.frame(terms(gibbs, 10))

# run NLP -> then get the number of location identifiers, N

# k = n, extract LDA matrices  -> run NLP again for actors 

# 
# treat text -- base formn words
# cluster




