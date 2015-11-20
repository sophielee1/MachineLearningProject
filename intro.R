
#####################################
### Probabilistic Machine Learning 
### Fall, 2015
### Howard, Haohan, Sophie 
### Git up repository -- git@github.com:sophielee1/MachineLearningProject.git
#####################################

setwd('~/R/Machine Learning Project')
source('setup.R')
set.seed(SEED)
library(topicmodels)  #install it locally

#import data
setwd(data.path)
articles=list()
n=640   #number of news articles 
for(i in 1:n){
  filename=paste0(i,".txt")
  articles[i]<-readChar(filename, file.info(filename))
}


# create dtm objects
dtms=list()
for(i in 1:n){
  filename=paste0(i,".txt")
  string=readChar(filename, file.info(filename))
  string<-GetCorpus(string)
  dtms[i]<-DocumentTermMatrix(string)
}


####################################################
# LDA --- requires topicmodels package
####################################################

k=2
burn=1000
odens=100
sims=1000
grams=1:2
gibbs=LDA(dtm, k=k, method="Gibbs",
          control=list(seed=SEED, burnin=burn, thin=odens, iter=sims))
df=rbind(dtm[[3]],dtm[[6]][2])
df<-DocumentTermMatrix(df)
freqwords<-findFreqTerms(dtm, 3)

freqency=sort(dtm[[3]], decreasing=T)
frequentest<-dtm[[6]][[2]][order(dtm[[3]], decreasing=T)]

#visualization

wordcloud(frequentest, freqency, 
          max.words=30, scale=c(3.5, .5), use.r.layout=F)

#The top 5 terms under the 10 topics are here
terms(gibbs, 5)


####################################################
#### Natural Language Processing
####################################################

#packages
install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
if (!require("pacman")) install.packages("pacman", dependencies = T) 
pacman::p_load(png, grid, foreign) #pacman will automatically try to install if package is missing
p_load(NLP,openNLP,RWeka,magrittr, png, grid, foreign, rJava)
######## sample data
d1<-readChar(filename, file.info(filename))
d1<-as.data.frame(rbind(d1,d1))
names(d1)="text"
d<-as.vector(d1[1])
d<-as.String(d1[1])
o<-Maxent_Entity_Annotator(language = "en",kind = "person", probs =FALSE)

sent_token_annotator <- Maxent_Sent_Token_Annotator(kind = "person")
a1 <- annotate(d, sent_token_annotator)
a2 <- annotate(d, word_token_annotator, a1)
a2


document_annotations <- annotate(s, 
                                 list(location_ann, 
                                      person_ann))

document_annotations_df <- as.data.frame(document_annotations)
document_annotations_df$features_type <- names(unlist(document_annotations$features,recursive=FALSE) )
document_annotations_df$features <- unlist(document_annotations$features,recursive=FALSE) 
document_annotations_df$features <- unlist(sapply(document_annotations$features, FUN=function(x) x[[1]][[1]]))
document_annotations_df$string <- as.character(document[document_annotations])

vars_factor <- c("type","features","features_type")
document_annotations_df[,vars_factor] <- lapply(document_annotations_df[,vars_factor], factor)
vars_numeric <- c("start","end","id")
document_annotations_df[,vars_numeric] <- lapply(document_annotations_df[,vars_numeric], as.numeric)
document_annotations_df
################


#dd



library(openNLPmodels.en)
p_load(tidyr,plyr,qdap)
df2 <- parse_sentences(text=d1)
DT::datatable(df2,
              options = list(pageLength = 30),
              class = 'cell-border stripe',
              rownames = FALSE, colnames = c('#'='sentence_number'  ))


# testing