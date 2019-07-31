library(FNN); library("gmodels")


#Load in data
df <- read.csv('cleaned_train.csv')

#First lets only keep binary and continuous variables
keep <- c('Survived',   'Pclass', 'Age', 'SibSp', 'Parch', 'lnFare' , 'miss', 'Sex_num')
df <- df[,keep]

#Scale df
scaledf <- as.data.frame(scale(df[,-1], center = TRUE, scale = TRUE))
scaledf$Survived <- df$Survived


#Create a Train Test Split 
set.seed(69)
smp_size <- floor(.8 * nrow(scaledf))

## set the seed to make your partition reproducible
train_ind <- sample(seq_len(nrow(scaledf)), size = smp_size)

train <- scaledf[train_ind, ]
test <- scaledf[-train_ind, ]

train_label <- train$Survived
test_label <- test$Survived

train <- train[,-8]; test <- test[,-8] 





error <- c()
k = c()
for (i in seq(1,100, by =1 )){
  pred <- knn(train = train, test = test, cl=train_label, k=i)
  k[i] = i
  knn_pred <- c(pred)
  knn_pred <- knn_pred - 1
  error[i] <- sum ((knn_pred - test_label)^2 )

}

error <- data.frame(error, k) 
plot(error$k, error$error)

pred <- knn(train = train, test = test, cl=train_label, k=4, prob = TRUE)
IRISPREDCross <- CrossTable(test_label, pred, prop.chisq = FALSE)

pred_frame <- read.csv('pred_frame.csv')
pred_frame$knn_pred <- as.numeric(pred) - 1

pred_frame$knn_perc <- (attr(pred, "prob"))
pred_frame$knn_perc <- ifelse(pred_frame$knn_pred == 1, pred_frame$knn_perc, 1 - pred_frame$knn_perc)


cor(pred_frame[,c(2,4,5)])

sum((pred_frame$observed - pred_frame$log_pred)^2)

write.csv(pred_frame, 'pred_frame.csv')