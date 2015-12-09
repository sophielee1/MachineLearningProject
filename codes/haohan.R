# haohan


library(tm)
library(RTextTools)


### ICEWS_NLP ###
rm(list=ls())
load("~/Dropbox/WORK/Research (now)/Chinese NPL project/Data/icews_nlp.Rdata")
icews <- read.csv("~/Dropbox/WORK/Research (now)/Chinese NPL project/Data/all_protest_from2001_2014_20151111.csv")
names(icews)
icews$issue_f <- as.factor(icews$issue)
# Re-code issues according to Howard's new schemes
icews$issue2 <- icews$issue_human
icews$issue2[icews$issue2 %in% c(1,13)] <- 18
icews$issue2[icews$issue2 %in% c(2,7)] <- 19
icews$issue2[icews$issue2 %in% c(8,12,14)] <- 20
icews$issue2[icews$issue2 %in% c(16, 17)] <- 21

icews$issue2_f <- factor(icews$issue2, levels = unique(icews$issue2)[order( unique(icews$issue2) )])

issue_num <- unique(icews$issue2)
issue_num
issue_txt <- c( "Social policy", "Econ policy", "Pollution", "Religious", 
                "Anti-Japan", "Student", "Democracy", "Labor", "Land-Corruption", "Riot-Justice", "Ethnic")

icews$issue2_f2 <- NA
icews$issue2_f2[icews$issue2_f == 3] <- "Social policy"
icews$issue2_f2[icews$issue2_f == 4] <- "Econ policy"
icews$issue2_f2[icews$issue2_f == 5] <- "Pollution"
icews$issue2_f2[icews$issue2_f == 6] <- "Religious"
icews$issue2_f2[icews$issue2_f == 9] <- "Anti-Japan"
icews$issue2_f2[icews$issue2_f == 10] <- "Student"
icews$issue2_f2[icews$issue2_f == 11] <- "Democracy"
icews$issue2_f2[icews$issue2_f == 18] <- "Labor"
icews$issue2_f2[icews$issue2_f == 19] <- "Land-Corruption"
icews$issue2_f2[icews$issue2_f == 20] <- "Riot-Justice"
icews$issue2_f2[icews$issue2_f == 21] <- "Ethnic"

# 15 ismissing


icews$issue2_f2 <- as.factor(icews$issue2_f2)

data.frame(issue_num, issue_txt)
table(icews$issue)
# Recode issue types according to Howard's new coding

list.files("~/Dropbox/WORK/Research (now)/Chinese NPL project/Data/")




set.seed(10)
dim(icews)
ntrain <- sample(1:642, 642 * .8, replace = F); icews_train <- icews[ntrain, ]
ntest <- which( !(1:642 %in% ntrain) ); icews_test <- icews[ntest, ]


as.character(icews$text[3])
#doc_matrix <- create_matrix(icews_train$text, language = "english", removeNumbers = T, stemWords = T,
#                            removePunctuation = T, weighting=weightTfIdf)

doc_matrix <- create_matrix(icews$text, language = "english", removeNumbers = T, stemWords = T,
                            removeSparseTerms = .998)

#View(inspect(doc_matrix))

tmp_wordvec <- strsplit(as.character(icews$text[3]), " ")[[1]]

data.frame(tmp_wordvec, wordStem( tmp_wordvec ))

str(doc_matrix)
doc_matrix$ncol
doc_matrix$nrow


container <- create_container(doc_matrix, icews$issue2_f, trainSize = ntrain, testSize = ntest, virgin = F)



SVM <- train_model(container,"SVM")
#SVM2 <- train_model(container,"SVM", kernel = "polynomial")
#GLMNET <- train_model(container,"GLMNET")
#MAXENT <- train_model(container,"MAXENT")
#SLDA <- train_model(container,"SLDA")
BOOSTING <- train_model(container,"BOOSTING")
BAGGING <- train_model(container,"BAGGING")
RF <- train_model(container,"RF")
#NNET <- train_model(container,"NNET")
TREE <- train_model(container,"TREE")

SVM_CLASSIFY <- classify_model(container, SVM)
#SVM_CLASSIFY2 <- classify_model(container, SVM2)
#GLMNET_CLASSIFY <- classify_model(container, GLMNET)
#MAXENT_CLASSIFY <- classify_model(container, MAXENT)
#SLDA_CLASSIFY <- classify_model(container, SLDA)
BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
BAGGING_CLASSIFY <- classify_model(container, BAGGING)
RF_CLASSIFY <- classify_model(container, RF)
#NNET_CLASSIFY <- classify_model(container, NNET)
TREE_CLASSIFY <- classify_model(container, TREE)


analytics <- create_analytics(container,
                              cbind(SVM_CLASSIFY, #SLDA_CLASSIFY,
                                    BOOSTING_CLASSIFY, BAGGING_CLASSIFY, RF_CLASSIFY,
                                    #NNET_CLASSIFY, 
                                    TREE_CLASSIFY
                                    #MAXENT_CLASSIFY
                              ))

analytics <- create_analytics(container,
                              cbind(SVM_CLASSIFY, #SLDA_CLASSIFY,
                                    BOOSTING_CLASSIFY, BAGGING_CLASSIFY, RF_CLASSIFY,
                                    NNET_CLASSIFY, TREE_CLASSIFY,
                                    MAXENT_CLASSIFY))

summary(analytics)

library(xtable)



# CREATE THE data.frame SUMMARIES
topic_summary <- analytics@label_summary
topic_summary
alg_summary <- analytics@algorithm_summary
alg_summary
ens_summary <-analytics@ensemble_summary
ens_summary
doc_summary <- analytics@document_summary
doc_summary


create_ensembleSummary(analytics@document_summary)



SVM <- cross_validate(container, 4, "SVM")
#GLMNET <- cross_validate(container, 4, "GLMNET")
MAXENT <- cross_validate(container, 4, "MAXENT")
SLDA <- cross_validate(container, 4, "SLDA")
BAGGING <- cross_validate(container, 4, "BAGGING")
BOOSTING <- cross_validate(container, 4, "BOOSTING")
#RF <- cross_validate(container, 4, "RF")
NNET <- cross_validate(container, 4, "NNET")
TREE <- cross_validate(container, 4, "TREE")




# Code location (for Howard)
############################
load("~/Dropbox/WORK/Research (now)/Chinese NPL project/Data/locs_nlp.Rdata")
locs

locs_d <- as.data.frame(matrix(ncol = 20, nrow = 642))
for (i in 1:642){
  if (length(locs[[i]]) > 0){
    locs_d[i, 1:length(locs[[i]])] <- locs[[i]]
  }
  print(i)
}
names(locs_d) <- paste0("location_", 1:20)
names(locs_d)

head(icews)
dim(icews)
icews[643:657, 2]
icews2 <- read.csv("~/Dropbox/WORK/Research (now)/Chinese NPL project/Data/all_protest_from2001_2014_20151111.csv")
dim(icews2)
icews_loc <- cbind(icews2, locs_d)

save(locs_d, file = "~/Dropbox/WORK/Research (now)/Chinese NPL project/Data/locs_nlp_recode.Rdata")
write.csv(locs_d, "~/Dropbox/WORK/Research (now)/Chinese NPL project/Data/locs_nlp_recode.csv")
names(icews_loc)
save(icews_loc, file = "~/Dropbox/WORK/Research (now)/Chinese NPL project/Data/icews+loc.Rdata")
write.csv(icews_loc, "~/Dropbox/WORK/Research (now)/Chinese NPL project/Data/icews+loc")

############################




# http://www.svm-tutorial.com/2014/11/svm-classify-text-r/


# Step 5: Create and train the SVM model




