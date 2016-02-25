##################################
# Sophie Lee
# Machine Learning Class final project: Annotated script
# Dec. 3rd. 2015.
##################################


rm(list=ls())


##### Set up ####
setwd("/Users/sophie/Documents/Data/")  # my working path
base.path=getwd()


library("openNLPmodels.en") # NLP dictionary for English
library("NLP")
library("openNLP") 
library(plyr) # ddply function in the parse_sentences function
library(tidyr) #spread function in the parse_sentences function
library(stringr) #string matching
library(tm) #stem words, etc.
library(magrittr) # for speedy processing of data management
library(topicmodels) # fitting the LDA model

# packages for plotting
library(rgdal) #reading the shape files
library(maptools) # reading shape files
library(RColorBrewer) # for palettes and color schemes
library(classInt) # for defining intervals



##### Load data ####

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



##### STEP 1: (Pre-treatment I) Cut out the parts citing sources ####




#1. remove the parts that contain the location of news reporting agencies
sources<-c("AFP","Reuters","AP","in Chinese", "web site")#stopwords
#2. remove the last sentence if it contains the following phrases
indicators<-c("Description of","Source")

data=icews


newvar<-rep(NA, nrow(data))
# names(newvar)<-"newvar"
for(i in 1:length(newvar)){
  sentences<- parse_sentences(data$text[i]) 
  cell<-sentences[1,ncol(sentences)]
  
  ## IF condition 1
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
  ### End of IF condition 1
  
  
  
  ## IF condition 2
  
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
  
  ## End of IF condition 2
#   sentences=sentences[1:nrow(sentences)-1,]
  
  coerced<-as.String(sentences[,ncol(sentences)])
  newvar[i]<-as.character(coerced)
}





##### Step 2: (Pre-Treatment II) stem words & remove special characters & stop words etc. #####

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



data=cbind(data,nv)
write.csv(data, "Chinese_nlp_treated_data_sophie.csv")





##### STEP 3: search for provinces & actors ####




### set up

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

# types<-c("protest", "citizen", "worker",	"employee","farmer","teacher")
# farmers	Farm Worker
# teachers	Education
# residents	citizens
# operators	
# villagers	
# coal miners	
# migrant Workers	Immigrants
# Tibetans	
# students	
# military men	
# rioters	
# rights activist	
# petitioners	
# doctors	Medical Personnel
# Muslims	
# Buddhist monks	
# thugs	Criminal
# mob	
# fishermen	
# furniture makers	Business
# investors	

# parameters for LDA
SEED=2015
burn=100
odens=100
sims=100
grams=1:2


###!!!! if you want to run the sample case for quick trial, run the line below. Otherwise, skip it
data=data[1:15,]


province_machine=rep(NA, nrow(data))
actor_machine=rep(NA, nrow(data))


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

#LDA
dtm<-GetCorpus(coerced)
dtm<-DocumentTermMatrix(dtm)
gibbs=LDA(dtm, k=k, method="Gibbs",
          control=list(seed=SEED, burnin=burn, thin=odens, iter=sims))
freqwords<-terms(gibbs, 20)

answer=matrix(NA,nrow=nrow(freqwords),ncol=ncol(freqwords))

for(j in 1:k){  #number of topics
actors=matrix(data=NA, ncol=k, nrow=1)
  
  for(r in 1:10){  # number of rows in the freqwords table
  TF<-str_detect(freqwords[r,j], types)
      
      if(length(which(TF==TRUE))>0){
       answer[r,j]<-as.character(as.String(types[which(TF==TRUE)]))
       } else{
       answer[r,j]<-NA
       }
    
   }
 
  if(length(which(!is.na(answer[,j])==TRUE))>0){
    no<-min(which(!is.na(answer[,j])==TRUE))
    actors[1,j]<-answer[no,j]
  } else{
    actors[1,j]<-NA
  }
  
}


}else{
  locations=NA
}

province_machine[i]<-as.String(locations)
province_machine<-str_replace_all(province_machine,"\n"," ")
province_machine<-str_replace_all(province_machine,"NA","")
actor_machine[i]<-as.String(actors)
actor_machine<-str_replace_all(actor_machine,"\n","")
actor_machine<-str_replace_all(actor_machine,"NA","")
}



#comparison


score(province_machine, data$province_human)
score(data$province_ICEWS, data$province_human)

actor_ICEWS<-str_replace(data$source_actor,"\\(.*?\\)","")
actor_ICEWS<-str_trim(actor_ICEWS)
actor_ICEWS<-tolower(actor_ICEWS)

for(i in 1:length(actor_ICEWS)){
  obj<-strsplit(as.character(actor_ICEWS[i])," ")
  actor_ICEWS[i]<-as.character(obj[[1]][1])
}

score(actor_machine, actor_ICEWS)


data_new=as.data.frame(cbind(data, province_machine, actor_machine, actor_ICEWS))
write.csv(data_new, "Sophie_1201_data.csv")
data=data_new





##### STEP 4: Plotting ####



par(mfrow=c(1,3))
par(mar=c(0,0,1,0))
data=read.csv("Sophie_1201_data.csv")

colors = brewer.pal(9, "Spectral")


# names<-unique(names)
# names<-str_replace(names,"\n"," ")
# 
# name_cells<-strsplit(as.character(names, " "))
# 
# protestdata <- data_new
chinaShape<-readShapePoly('/Users/sophie/Documents/Data/CHN_adm1.shp')

#make the province name convention consistent

provsShape<-chinaShape@data %>% .$NAME_1 %>% unique() %>% as.character()
provsShapeKey = provsShape
provsShape[provsShape=='Xizang'] = 'Tibet'
provsShape[provsShape=='Ningxia Hui'] = 'Ningxia'
provsShape[provsShape=='Nei Mongol'] = 'Inner-Mongolia'
provsShape[provsShape=='Xinjiang Uygur'] = 'Xinjiang'
provsData = data.frame(provsShape, provsShapeKey, stringsAsFactors=FALSE)
provsData$id = 1:nrow(provsData)
provsData[,1]=tolower(provsData[,1])



# (1) Human coded
names<-data$province_human
# names<-str_replace(names,"hong kong","hong-kong")
# names<-str_trim(names)
names<-tolower(names)
names<-str_replace(names, "inner mongolia","inner-mongolia")
unique(names)


# count variable
names_list=names
tab<-table(names_list)

count=as.vector(rep(NA, nrow(provsData)))
for(i in 1:nrow(provsData)){
  TF<- names(tab)==provsData[i,1] 
  
  if( length(which(TF==TRUE)) > 0 ) {
    count[i]<-as.numeric(tab[which(names(tab)==provsData[i,1])])
  } else {
    count[i]=0
  }
}

provsData=as.data.frame(cbind(provsData, "count"=count))


chinaShape$count = provsData$count[match(chinaShape@data$NAME_1, provsData$provsShapeKey)]

brks = classIntervals(chinaShape$count , n=9, style="fixed", 
                      fixedBreaks=c( 0, 15, 30, 45, 60, 75, 90, 105 ,120, 151) )  ### choose "equal or not"

brks = brks$brks
plot(chinaShape,
     col=colors[findInterval(chinaShape$count , brks,all.inside=TRUE)],
     axes=F,
     main="Human-coded", cex.main=1.2)





# (2) ICEWS
names=as.vector(as.character(data$province_ICEWS)) #provinces
remove<-c("Sheng","Shi","Autonomous Region","Zizhiqu")
for(i in 1:length(remove)){
  names<-str_replace_all(names,remove[i],"") 
}

names<-str_trim(names)
names<-tolower(names)
unique(names)

names<-str_replace(names,"xinjiang uygur","xinjiang")
names<-str_replace(names, "inner mongolia","inner-mongolia")
names<-str_replace(names,"guangxi zhuangzu","guangxi")

# count variable
names_list=names
tab<-table(names_list)

count=as.vector(rep(NA, nrow(provsData)))
for(i in 1:nrow(provsData)){
  TF<- names(tab)==provsData[i,1] 
  
  if( length(which(TF==TRUE)) > 0 ) {
    count[i]<-as.numeric(tab[which(names(tab)==provsData[i,1])])
  } else {
    count[i]=0
  }
}

provsData=as.data.frame(cbind(provsData, "count"=count))


chinaShape$count = provsData$count[match(chinaShape@data$NAME_1, provsData$provsShapeKey)]

brks = classIntervals(chinaShape$count , n=9, style="fixed", 
                      fixedBreaks=c( 0, 10, 20, 30, 40, 50, 60, 70 , 80, 151) )  ### choose "equal or not"

brks = brks$brks
plot(chinaShape,
     col=colors[findInterval(chinaShape$count , brks,all.inside=TRUE)],
     axes=F,
     main="ICEWS", cex.main=1.2)





# (3) Machine coded data

# make the count variable 

names<-data$province_machine
names<-str_replace(names,"hong kong","hong-kong")
names<-str_replace(names, "inner mongolia","inner-mongolia")


names_list<-unlist(strsplit(names[1], " "))
for(i in 2:nrow(data)){
  if(names[i]==names[i-1]){
    names_list=c(names_list, NA)
  } else{
    names_list<-c(names_list, 
                  unlist(strsplit(names[i], " ")) 
    )
  }
}
names_list=names_list[which(!is.na(names_list))]



tab<-table(names_list)

count=as.vector(rep(NA, nrow(provsData)))
for(i in 1:nrow(provsData)){
  TF<- names(tab)==provsData[i,1] 

  if( length(which(TF==TRUE)) > 0 ) {
  count[i]<-as.numeric(tab[which(names(tab)==provsData[i,1])])
  } else {
    count[i]=0
  }
}

provsData=as.data.frame(cbind(provsData, "count"=count))


chinaShape$count = provsData$count[match(chinaShape@data$NAME_1, provsData$provsShapeKey)]

brks = classIntervals(chinaShape$count , n=9, style="fixed", 
                      fixedBreaks=c( 0, 15, 30, 45, 60, 75, 90, 105 ,120, 151) )  ### choose "equal or not"

brks = brks$brks
plot(chinaShape,
     col=colors[findInterval(chinaShape$count , brks,all.inside=TRUE)],
     axes=F,
     main="Machine-coded", cex.main=1.2)



#add a legend

legend(
  x=122, y=28,
  #   'bottomright', # or lazy positioning
  legend=leglabs(brks),
  fill=colors,
  bty="n",x.intersp = .5, y.intersp = .5, cex=0.9)


write.csv(data, "NLP_firstalgorithm.csv")


########## End of the codes








