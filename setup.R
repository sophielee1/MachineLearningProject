
rm(list=ls())
set.seed(20151023)


check_packages = function(names)
{
  for(name in names)
  {
    if (!(name %in% installed.packages()))
      install.packages(name, repos="http://cran.us.r-project.org")
    
    library(name, character.only=TRUE)
  }
}


packs=c(
  'foreach', 'doParallel', 'magrittr',
  'tm', 'SnowballC', 'slam', # text processing packages
  'ggplot2','wordcloud', # viz packages
  'modeltools', # modeling (also use topicmodels, see topicsModels.R script for instructions on installing)
  'zoo', 'reshape2', 'xtable','topicmodels'
)
check_packages(packs)



# Helper functions
trim = function (x) gsub("^\\s+|\\s+$", "", x) # Remove extra white space
char=function(x){ as.character(x) } # Convert to character
num=function(x){ as.numeric(char(x)) } # Convert to numeric
wordCnt=function(x) { sapply(strsplit( trim(x), " "), length) }

# This function takes in a binary datafile from
## the data directory and returns
## a dataframe object. This function is used in
## multiple R analysis scripts so that we can get
## descriptive information for each news article

convertToDF=function(x){
  x=strsplit(x, '|', fixed=TRUE)
  x=do.call('rbind', x)
  x=data.frame(x, stringsAsFactors=FALSE)
  colnames(x)=x[1,]
  return( x[-1,] )
}




