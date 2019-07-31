#Title: RandomForest.R 
#Author: Chris Montgomery
# Input: cleaned_train.csv
#Output: 

#Description: This script produces a Random Forest model for the titanic dataset in R.


library(randomForest)

df <- read.csv('cleaned_train_test.csv')
df <- df[, !colnames(df) %in% c("Cabin", "Name", "Ticket", 'X')]
df <- na.omit(df)
df$Survived <- as.factor(df$Survived)
df$Pclass_cat <- as.factor(df$Pclass)

#Create a Train Test Split
# set.seed(69)
# smp_size <- floor(0.8 * nrow(df))
# 
# ## set the seed to make your partition reproducible
# train_ind <- sample(seq_len(nrow(df)), size = smp_size)
# 
# train <- df[train_ind, ]
# test <- df[-train_ind, ]

# train <- df[!is.na(df$Survived), ]
# test <- df[is.na(df$Survived), ]


train_label <- train$Survived
test_label <- test$Survived

model1 <- randomForest(Survived ~ ., data = train, ntree = 10000, importance = TRUE, mtry = 10)
# model1
# 
# model1$confusion
# 
# model1
# model1$importance

pred_frame <- read.csv('pred_frame.csv')
pred_frame$forest_perc <-as.numeric( predict(model1, test)) 
pred_frame$forest_pred <- ifelse(pred_frame$forest_perc > .5 , 1, 0)


rforest_perc <- data.frame (predict(model1, test, type = "prob"))

# 
# 
# pred_frame <- data.frame(test$PassengerId)
# pred_frame$Survived <- predict(model1, test);
 write.csv(pred_frame, 'pred.csv')



pred_frame$ensemble <- ifelse(pred_frame$forest_pred > 1 , (2/3 * pred_frame$forest_perc) + (pred_frame$step_modperc/ 3), 
                              (1/3 * pred_frame$forest_perc ) + (2/3 * pred_frame$step_modperc) ) 

pred_frame$ensemble_pred <- ifelse(pred_frame$ensemble >= .5 , 1, 0)