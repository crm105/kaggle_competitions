#Title: ames_eda.R
#Author: Chris Montgomery (crm105@gwu.edu)
#Last Revised: 7/3/2019

#Input: train.csv

#Description: This script performs data cleaning and EDA tasks for the Ames housing dataset
set.seed(1)
library(dplyr); library(pls); library(moments)


setwd("C:/Users/montg/Documents/active_projects/ames_housing")

df <- read.csv("train.csv")

df$SalePrice <- log(df$SalePrice)
skewness(df$SalePrice, na.rm = TRUE)
test <- read.csv("test.csv"); test$SalePrice <- ''
df <- rbind(df, test)
df$YrSold <- as.factor(df$YrSold)
df$MoSold <- as.factor(df$MoSold)

summary(df$Condition2)

#drop <- c("Condition2", "Condition1"); df <- df[,!colnames(df) %in% drop]

ext_other <- c("AsbShng", "AsphShn", "BrkComm", "CBlock", "ImStucc", "Stone")
df$Exterior1st <- ifelse(df$Exterior1st %in% ext_other, 'other', df$Exterior1st); df$Exterior1st <- as.factor(df$Exterior1st)
summary(df$Exterior1st)

ext2_other <- c("AsbShng", "AsphShn", "Brk Comm", "CBlock", "ImStucc", "Stone")
df$Exterior2nd <- ifelse(df$Exterior2nd %in% ext_other, 'other', df$Exterior2nd); df$Exterior2nd <- as.factor(df$Exterior2nd)



#Consider Log Transforming Some Variables that have wide variation

#df$LotArea <- log(df$LotArea)
#df$ln_livarea <- log(df$GrLivArea)
#summary(df$GrLivArea)
#summary(df$LotArea) 

hist(df$LotArea)
cor(df[is.numeric(df),])

df$garage <- ifelse(df$GarageArea == 0, 0, 1)
df$pool <- ifelse(df$PoolArea == 0, 0 , 1)
df$Alley <- as.character(df$Alley);df[is.na(df$Alley), "Alley"] <- 'none'; df$Alley <- as.factor(df$Alley)
df$Fence <- as.character(df$Fence);df[is.na(df$Fence), "Fence"] <- 'none'; df$Fence <- as.factor(df$Fence)
df$BsmtCond <- as.character(df$BsmtCond);df[is.na(df$BsmtCond), "BsmtCond"] <- 'none'; df$BsmtCond <- as.factor(df$BsmtCond)
df$BsmtQual <- as.character(df$BsmtQual);df[is.na(df$BsmtQual), "BsmtQual"] <- 'none'; df$BsmtQual <- as.factor(df$BsmtQual)

#Garage
df$GarageQual <- as.character(df$GarageQual);df[is.na(df$GarageQual), "GarageQual"] <- 'none'; df$GarageQual <- as.factor(df$GarageQual)
df$GarageCond <- as.character(df$GarageCond);df[is.na(df$GarageCond), "GarageCond"] <- 'none'; df$GarageCond <- as.factor(df$GarageCond)
df$GarageType <- as.character(df$GarageType);df[is.na(df$GarageType), "GarageType"] <- 'none'; df$GarageType <- as.factor(df$GarageType)
df$GarageFinish <- as.character(df$GarageFinish);df[is.na(df$GarageFinish), "GarageFinish"] <- 'none'; df$GarageFinish <- as.factor(df$GarageFinish)

#Fireplace
df$FireplaceQu <- as.character(df$FireplaceQu);df[is.na(df$FireplaceQu), "FireplaceQu"] <- 'none'; df$FireplaceQu <- as.factor(df$FireplaceQu)

df <- df[,!colnames(df) %in% c("PoolQC", "MiscFeature")]

#Lot Frontage zero?
df[is.na(df$LotFrontage), "LotFrontage"] <- 0

#Garage year built
df[is.na(df$GarageYrBlt), "GarageYrBlt"] <- mean(df$GarageYrBlt, na.rm = TRUE)

#Basement
df[is.na(df$BsmtExposure), "BsmtExposure"] <- "No"

for (i in colnames(df[ , purrr::map_lgl(df, is.factor)])){
  df[,i] <- as.character (df[,i])
  df[is.na(df[,i]), i] <- "none"
  df[,i] <- as.factor (df[,i])
  }

for (i in colnames(df[ , purrr::map_lgl(df, is.numeric)])){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
  }

  

na_count <-data.frame (sapply(df, function(y) sum(length(which(is.na(y))))), sapply(df, class), colnames(df))
na_count <- na_count[na_count$sapply.df..function.y..sum.length.which.is.na.y..... > 0,]

a <- 0
for(i in (na_count[,3])){
  a <- a + 1
 if(na_count[a, 2] %in% c('integer', 'numeric')){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)

 }
  
 else if (na_count[a,2] == 'factor') {
   
   df[, colnames(df) %in% na_count[[a,3]]  ] <- as.character(   df[, colnames(df) %in% na_count[a,3]])
   
  df[is.na(df[,i]), i] <- 'none'
  df[,i] <- as.factor(df[,i])
   
 }
}
na_count <-data.frame (sapply(df, function(y) sum(length(which(is.na(y))))), sapply(df, class), colnames(df))
na_count <- na_count[na_count$sapply.df..function.y..sum.length.which.is.na.y..... > 0,]
# 
for (i in colnames(df[ , purrr::map_lgl(df, is.numeric)])){
  if(skewness(df[,i], na.rm = TRUE) <= -.5 | skewness(df[,i], na.rm = TRUE) > .5){
    df[,i] <- log(df[,i] + 1)
  }
}

 df[df$SalePrice == '', 'SalePrice'] <- NA ; df$SalePrice <- as.numeric(df$SalePrice)

library(glmnet)
train <- df[!is.na(df$SalePrice),]
test <- df[is.na(df$SalePrice), ]; test$SalePrice <- ''; nrow(na.omit(test))

y_train <- train$SalePrice

full <- lm (SalePrice ~ . , data = train)
best_step <- step(full, direction = 'both', steps = 10000)
best_step$call
# train the model

library(caret)
train_control <- trainControl(method="CV", number = 10)

model <- train(SalePrice ~ MSSubClass + MSZoning + LotArea + Street + 
                 LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + 
                 OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofMatl + 
                 Exterior1st + ExterCond + Foundation + BsmtQual + BsmtExposure + 
                 BsmtFinType1 + BsmtFinSF1 + BsmtUnfSF + TotalBsmtSF + Heating + 
                 HeatingQC + CentralAir + LowQualFinSF + GrLivArea + BsmtFullBath + 
                 FullBath + HalfBath + KitchenAbvGr + KitchenQual + Functional + 
                 Fireplaces + GarageCars + GarageArea + GarageQual + GarageCond + 
                 WoodDeckSF + EnclosedPorch + ScreenPorch + PoolArea + SaleType + 
                 SaleCondition + pool, data = train, trControl=train_control, method="lm", metric = "RMSE")

model$results
train_frame <- data.frame(y_train)
train_frame$ols_pred <- model$pred


train_x <- model.matrix(SalePrice~., data=train)
test$SalePrice <- 0
test_x <- model.matrix(SalePrice ~. , data = test)

train_x <- train_x[,colSums(train_x) > 10]
test_x <- test_x[,colSums(test_x) > 10]

test$SalePrice <- ''; test_x <- model.matrix(SalePrice ~., data = test)
ols_pred <- predict(model, test)
ols_ensemble <- predict(model, train)

#find the best lambda from our list via cross-validation
cv.out <- cv.glmnet(train_x, train$SalePrice, alpha = 0, standardize = TRUE, nfolds = 10)
plot(cv.out)
bestlam <- cv.out$lambda.min; bestlam
ridge.mod <- glmnet(x = train_x, y = train$SalePrice, alpha = 0, lambda = bestlam, standardize = TRUE)
cv.out$cvm
my_control <-trainControl(method="cv", number=10)
lassoGrid <- expand.grid(alpha = 0, lambda = seq(0.001,1,by = 0.0005))

lasso_mod <- train(x=train_x, y=train$SalePrice, method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 
lasso_mod$bestTune
lasso_mod$results

lass_reg <- glmnet(x = train_x, y = y_train, alpha = 1, lambda = .0025)
train_frame$lasso <- predict(lasso_mod, train_x)
pred_frame$lasso <- predict(lasso_mod, as.data.frame(test_x))
# 
# my_control <-trainControl(method="cv", number=10)
# ridgeGrid <- expand.grid(alpha = 0, lambda = c(0.001,0.1, 1 , 10, 100, 1000))
# 
# model_ridge <- train(x=train_x,y=y_train,
#                      method="glmnet",
#                      metric="RMSE",
#                      
#                      trControl=trainControl(method = "cv", number = 10),
#                      tuneGrid=ridgeGrid)
# 
# model_ridge$bestTune
# model_ridge$results
# 
# ridge_mod <- train(x=x_train, y=y_train, method='glmnet', trControl= my_control, tuneGrid=ridgeGrid) 
# ridge_mod$bestTune
# plot(cv.out)
# 
# #make predictions
# ridge.pred <- predict(ridge.mod, s = bestlam, newx = train_x)
#check MSE


library(randomForest)
train_rf <- randomForest(train_x, y_train, ntree = 4000, mtry = length(colnames(train_x))^.5)
train_rf$mse[4000]^.5

train_frame$rf_pred <- predict(rf, train_x)
pred_frame$rf_pred <- predict(rf, test_x)


#First lets determine some of the most important variables:

rf_imp <- data.frame (  (rf$importance))
rf_imp$cols <- row.names(rf_imp)



knn_train <- train_x[,colnames(train_x) %in% rf_imp[rf_imp$IncNodePurity > 1.5, "cols"]]
knn_test <- test_x[,colnames(test_x) %in% colnames(knn_train)]

library(FNN)
knn_rmse <- c()
a <- c()
for( i in 1:100){
  a[i] <- i
knn_model <- knn.reg(knn_train, y = y_train, k = i)
knn_rmse[i] <- mean(((y_train - knn_model$pred)^2)^.5)


}
knn_frame <- data.frame(a, knn_rmse)

knn_frame[knn_frame$knn_rmse == min(knn_rmse),]

knn_train_mod <- knn.reg(train_x, y = y_train, k = 10)
train_frame$knn_pred <- knn_train_mod$pred


knn_test <- knn.reg(knn_train, knn_test, y = y_train, k = 10)
pred_frame$knn_pred <- knn_test$pred

cor(pred_frame)

#Let's do some stacking with our training frame


#Get OLS CV predictions up, dont worry about log transformations. 


library(gbm )

caretGrid <- expand.grid(interaction.depth=c(1, 3, 4), n.trees = (0:50)*100,
                         shrinkage=c(0.01, .005, 0.001),
                         n.minobsinnode=c(10))
metric <- "RMSE"
trainControl <- trainControl(method="cv", number=10)

set.seed(99)
gbm.caret <- train(y_train~ ., data=as.data.frame(cbind (train_x, y_train)), distribution="gaussian", method="gbm",
                   trControl=trainControl, verbose=FALSE, 
                   tuneGrid=caretGrid, metric=metric, bag.fraction=0.75)                  


gbm.caret$results
gbm.caret$bestTune

gbm_pred <- gbm(SalePrice ~ . , data = train, distribution = 'gaussian', n.trees = 4200, interaction.depth = 4, 
shrinkage = .01, n.minobsinnode = 10, verbose = FALSE, bag.fraction = .75, keep.data = TRUE)

train_frame$gbm <- gmb_pred$fit
pred_frame$gbm <- predict(poop, test_x)


train_frame$avg <- ((train_frame[,2] + train_frame[,3] + train_frame[,4] + train_frame[,5]) )/ 4

mean((train_frame[,2] - train_frame[,1])^2)^.5
mean((train_frame[,3] - train_frame[,1])^2)^.5
mean((train_frame[,4] - train_frame[,1])^2)^.5
mean((train_frame[,5] - train_frame[,1])^2)^.5
mean((train_frame[,6] - train_frame[,1])^2)^.5


#Build a dataframe containing new predictions from individual models 


pred_frame$avg <- ((pred_frame[,2] + pred_frame[,3] + pred_frame[,4] + pred_frame[,5]) )/ 4




library(randomForest)
ensemble_rf <- randomForest(y_train ~ . , data = train_frame, n.trees = 5000)
ensemble_rf$mse
ensemble_gbm <- gbm (y_train ~. , data = train_frame, cv.folds = 10, n.trees = 1000, shrinkage = c(.01, .001) )


#Predict using boosted ensemble
pred_frame <- pred_frame %>% select(-ols_pred) %>% rename(gbm = gbm_pred)
pred_frame$ensemble_pred <- predict(ensemble_gbm, pred_frame)

cor(pred_frame)

pred_frame_exp <- exp(pred_frame[,2:7]); pred_frame_exp$Id <- pred_frame$test.Id
write.csv(pred_frame_exp, "predictions.csv")

