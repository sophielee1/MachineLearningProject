


##### Set up #####

setwd("/Users/sophie/Documents/Data/")  # my working path
base.path=getwd()
# data<-read.csv("NLP_firstalgorithm.csv")

##### Load news article data #####

icews <- read.csv(paste0(base.path, "/all_protest_from2001_2014_20151111.csv"))
icews$headline = as.character(icews$headline)
icews$text = as.character(icews$text)

##### helper functions ####
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

# text into corpora
GetCorpus <-function(textVector)
{
  doc.corpus <- Corpus(VectorSource(textVector))
  doc.corpus <- tm_map(doc.corpus, tolower)
  doc.corpus <- tm_map(doc.corpus, removeNumbers)
  doc.corpus <- tm_map(doc.corpus, removePunctuation)
  doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))
  doc.corpus <- tm_map(doc.corpus, stemDocument, "english")
  doc.corpus <- tm_map(doc.corpus, stripWhitespace)
  doc.corpus <- tm_map(doc.corpus, PlainTextDocument)  
  return(doc.corpus)
}


# scoring function for comparison statistics at the end

score<-function(youranswer, solution){
  total=0
  for(i in 1:length(youranswer)){
    # test<-strsplit(as.character(youranswer[i])," ")
    # binary<-agrep(test[[1]], as.character(solution[i]), ignore.case = TRUE, max=5)
    binary<-agrep(as.character(solution[i]), as.character(youranswer[i]), ignore.case = TRUE, max=2)  
    total<-as.numeric(sum(total,binary))
  }
  return(total/length(youranswer))
}







##### pre-treatment (to lower cases, remove special characters) #####
data=icews

nv<-newvar
vector<-c("\n","?","http\\S+\\s*",") -",";","??","???","]") #things you want to remove

for(i in 1:length(vector)){
  nv=str_replace_all(nv,vector[i],"")
}


# to lower case -> remove stop words -> stem words
for(i in 1:length(nv)){
  text<-nv[i]
  
  vc<-VCorpus(VectorSource(text))  
  vc<-tm_map(vc, content_transformer(tolower))  
  vc<-tm_map(vc, removeWords, stopwords("english"))  
  vc<-tm_map(vc, stemDocument)
  coerced<-as.String(vc[[1]]$content)
  newvar[i]<-as.character(coerced)
}





#new variables 
province_machine2=rep(NA, nrow(data))
actor_machine2=rep(NA, nrow(data))





# space to search
newvar=tolower(data$nv)
# list of province names to search for
names=as.vector(as.character(unique(data$province_ICEWS))) #provinces
remove<-c("Sheng","Shi","Autonomous Region","Zizhiqu")
for(i in 1:length(remove)){
  names<-str_replace_all(names,remove[i],"") 
}
names=names[!is.na(names)]
names=c(names,"Hong Kong","Taiwan")
names[13]<-"Xinjiang"
names[26]<-"Inner Mongolia"
names[21]<-"Guangxi"
names<-str_trim(names)
names<-tolower(names)

#list of actor types to search for
types=as.character(unique(data$source_actor))
types<-str_replace(types,"\\(.*?\\)","")
types<-str_trim(types)
types<-tolower(types)

for(i in 1:length(types)){
  obj<-strsplit(as.character(types[i])," ")
  types[i]<-as.character(obj[[1]][1])
}





i=1
min=max=5
for(i in 1:nrow(data)){
  
  # search 
  TF<-str_detect(newvar[i], names)
  if(length(which(TF==TRUE))>1){
    locations<-names[which(TF==TRUE)]
    k=length(locations)
    
    #stem words
    text<-newvar[i]
    vc<-VCorpus(VectorSource(text))  
    vc<-tm_map(vc, content_transformer(tolower))  
    vc<-tm_map(vc, removeWords, stopwords("english"))  
    vc<-tm_map(vc, stemDocument)
    coerced<-as.String(vc[[1]]$content)
    
    
    
    


  

phrases=rep(NA, nrow(data))

## for loop begins
test<-NGramTokenizer(newvar[i], Weka_control(min=min,max=max))
ma<-locations  # from the previous function
hu<-data$province_human[i]
no1<-length(ma)
no2<-str_count(hu, "\\S+")
table<-as.character(c(tolower(hu), rep(FALSE, no1-no2)))
TF<-table==locations
wrong<-locations[which(TF==!TRUE)]  
phrases<-test[which(!is.na(str_match(test, wrong)))]
phrases<-str_replace_all(phrases, wrong, "\\")

# phrases<-concatenate(test[which(!is.na(str_match(test, wrong)))]) #which of the ngram phrases contain the wrong words?


corp <- Corpus(VectorSource(phrases))
tdm <- TermDocumentMatrix(corp, control = list(tokenize = BigramTokenizer))
findFreqTerms(tdm, 2, Inf) #lowest freq. bound =2


matrix <- as.matrix(tdm[findFreqTerms(tdm, lowfreq = 2),])

 
DF <- data.frame(my_matrix, sums = rowSums(my_matrix))
DF
## end of for loop
  
  
  
  
  
  
  






