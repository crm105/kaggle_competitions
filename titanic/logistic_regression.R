#Title: Logistic_Regression.R 
#Author: Chris Montgomery
# Input: cleaned_train.csv
#Output: 
#Description: This script produces a stepwise logistic regression model for the titanic data series. 

library(dplyr)

#Load in data
#df <- read.csv('cleaned_train.csv')
df <- read.csv('cleaned_train_test.csv')
#df <- na.omit(df)
df$Pclass_cat <- as.factor(df$Pclass)


#Create a Train Test Split
# #set.seed(69)
#  accuracy <- c()
# for (i in 1 : 1000) {
#  smp_size <- floor(0.8 * nrow(df))
# # 
# # ## set the seed to make your partition reproducible
# train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[!is.na(df$Survived), ]
test <- df[is.na(df$Survived), ]

# train <- df[train_ind, ]
# test <- df[-train_ind, ]



#Stepwise logistic regression


# fullmod = glm(Survived ~ . , data = train, family='binomial')
# 
# step_glm <- step(fullmod, direction = "both")

step_mod <- glm(Survived ~  Sex + SibSp + Pclass_cat + lnFare + class_gen + title + 
                  Age + Age2 + single , data = train, family = binomial); summary(step_mod)


#Create a dataframe that will hold predictions
pred_frame <- data.frame(test$PassengerId)
pred_frame$observed <- test$Survived
pred_frame$step_modperc <- predict(step_mod, test, type = 'response')
pred_frame$step_modPred <- ifelse(pred_frame$step_modperc <= .50, 0, 1)

# pred_frame$error <-  (( pred_frame[,'observed'] - pred_frame$step_modPred)^2)
# 
# accuracy[i] <- 1 - sum(pred_frame$error) / nrow(test)
# print(mean(accuracy))
# #}

# 
# library(caret)
# pdata <- predict(step_mod, test, type = 'response')
# confusionMatrix(data = as.factor(pred_frame$step_modPred), reference = as.factor(pred_frame$observed))
# 
write.csv(pred_frame, 'pred_frame.csv')
