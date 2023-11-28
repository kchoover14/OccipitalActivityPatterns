library(tidyverse); library(EnvStats)
library(psych); library(car); library(MASS)

#load data
occ.combined <- read.csv("data-occ-combined-index.csv", stringsAsFactors = TRUE)

#DFA
occ.dfa <- read.csv("data-occ-combined-index.csv", stringsAsFactors = TRUE)
occ.dfa <- dplyr::select(occ.dfa, Sex, Index_FM)
occ.dfa <- occ.dfa[complete.cases(occ.dfa), ]
#scale predictors
occ.dfa[2] <- scale(occ.dfa[2])
#create train and test datasets
set.seed(1122)
sample <- sample(c(TRUE, FALSE), nrow(occ.dfa), replace=TRUE, prob=c(0.7,0.3))
train <- occ.dfa[sample, ]
test <- occ.dfa[!sample, ]
rm(sample)
#fit LDA model
fm.model <- MASS::lda(Sex ~ ., data = train, na.action = na.pass)
fm.model
#predictions
predicted <- predict(fm.model, test, na.action = na.pass)
#accuracy
mean(predicted$class==test$Sex) #71%

#two predictors
occ.dfa2 <- read.csv("data-occ-combined-index.csv", stringsAsFactors = TRUE)
occ.dfa2 <- dplyr::select(occ.dfa2, Sex, Index_FM,BicondylarBreadth)
occ.dfa2 <- occ.dfa2[complete.cases(occ.dfa2), ]
#scale predictors
occ.dfa2[2:3] <- scale(occ.dfa2[2:3])
#create train and test datasets
set.seed(1122)
sample <- sample(c(TRUE, FALSE), nrow(occ.dfa2), replace=TRUE, prob=c(0.7,0.3))
train2 <- occ.dfa2[sample, ]
test2 <- occ.dfa2[!sample, ]
rm(sample)
#fit LDA model
fm.model2 <- MASS::lda(Sex ~ ., data = train2, na.action = na.pass)
fm.model2
#predictions
predicted <- predict(fm.model2, test2, na.action = na.pass)
#accuracy
mean(predicted$class==test2$Sex) #66%

#tidy
rm(list = ls())
gc()