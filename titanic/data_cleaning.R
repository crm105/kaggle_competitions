#Title: data_cleaning.R
#Author: Chris Montgomery (crm105@gwu.edu)
#Last Revised: 6/23/2019
#Input: train.csv
#Output: tain_clean.csv

#Description: This script performs exploratory data analysis and generates new features for the Titanic dataset 

library('qdapRegex'); library('dplyr')

setwd('C:/Users/montg/Documents/active_projects/titanic')

df <- read.csv('train.csv')
test_df <- read.csv('test.csv'); test_df$Survived <- ''
df<- rbind(df, test_df); df$Survived <- as.numeric(df$Survived)


df$Fare <- ifelse(is.na(df$Fare), mean(df$Fare, na.rm = TRUE), df$Fare)

#Pclass as categorical
df$Pclass_cat <- as.factor(df$Pclass)


#Let's try and do a better job of imputing the missing age variables
##Create a linear model to predict age
###R2 is not very high
####Stepwise logistic regression gets prediction accuracy of 82% just using mean age 
df$sib <- ifelse(df$SibSp > 1, 1, 0)
df$ch <- ifelse(df$Parch == 2  , 1 ,0)
df$couple <- ifelse(df$Parch == 0 & df$SibSp == 1, 1 , 0)
df$single <- ifelse(df$SibSp == 0 & df$Parch == 0 , 1 ,0)
df$par <- ifelse(df$Parch != 2 & df$Parch != 0 , 1, 0)
df$mom <- ifelse(df$mrs == 1 & df$Parch > 0, 1 , 0)

mom <- df[df$mrs == 1 & df$Parch > 0 ,  ]

#Consolidate Cabins to A-E
df$Cab <- 'none'
df$Cab <- ifelse(grepl('A',  df$Cabin), 'A' , df$Cab)
df$Cab <- ifelse(grepl('B',  df$Cabin), 'B' , df$Cab)
df$Cab <- ifelse(grepl('C',  df$Cabin), 'C' , df$Cab)
df$Cab <- ifelse(grepl('D',  df$Cabin), 'D' , df$Cab)
df$Cab <- ifelse(grepl('E',  df$Cabin), 'E' , df$Cab)
df$Cab <- ifelse(grepl('F',  df$Cabin), 'F' , df$Cab)
df$Cab <- ifelse(grepl('T',  df$Cabin), 'T' , df$Cab)

df$Cab <- as.factor(df$Cab)
summary(df$Cab)

#Add Log transformed variables
df$Fare <- ifelse(df$Fare == 0, 1, df$Fare)
df$noFare <- ifelse(df$Fare == 1 , 1 , 0)
df$lnFare <- log(df$Fare)


#Add some interaction terms
df$Sex_num <- ifelse(df$Sex == 'male', 1, 0)
df$Fare_gen <- df$Sex_num * df$Fare
df$class_gen <- df$Pclass * df$Sex_num

df$title <- as.character (ex_between(df$Name, ",", ".", exact = TRUE))
unique(df$title)
df$title <- as.factor(df$title)
df$title <- as.factor (gsub( 'Mme', 'Mrs', df$title))
df$title <- as.factor (gsub( 'Mlle', 'Miss', df$title))
df$title <- as.factor (gsub( 'Ms', 'Miss', df$title))

df$title <- as.factor (gsub( 'Capt', 'officer', df$title))
df$title <- as.factor (gsub( 'Col', 'officer', df$title))
df$title <- as.factor (gsub( 'Major', 'officer', df$title))
df$title <- as.factor (gsub( 'Master', 'officer', df$title))

df$title <- as.factor (gsub( 'Don', 'other', df$title))
df$title <- as.factor (gsub( 'Rev', 'other', df$title))
df$title <- as.factor (gsub( 'Sir', 'other', df$title))
df$title <- as.factor (gsub( 'Dr', 'other', df$title))


df$title <- as.factor (gsub( 'Lady', 'other', df$title))
df$title <- as.factor (gsub( 'the Countess', 'other', df$title))
df$title <- as.factor (gsub( 'Jonkheer', 'other', df$title))
df$title <- as.factor (gsub( 'othera', 'other', df$title))

df %>% group_by(title) %>% summarize( mean(Survived, na.rm = TRUE))

#We dont need these columns any more
df <- df[,!colnames(df) %in% c("Name", "Ticket", "Cabin")]

full.age <- lm(Age ~  . , data = df[,!colnames(df) %in% 'Survived']);# summary(age.lm)
step(full.age, direction = 'both')

df$missing_age <- ifelse(is.na(df$Age), 1,0)

lm1 <- lm(Age ~ Parch + Embarked + Pclass_cat + sib + ch + couple + par + 
            class_gen + title, data = df); summary(lm1)
age.pred <- df[is.na(df$Age),]
df[is.na(df$Age), 'Age'] <-   (predict(lm1, age.pred))

#Add some new squared variables
#Just replace na with median age for now
df$Age <- ifelse(is.na(df$Age), mean(df$Age, na.rm = TRUE), df$Age)
df$Age2 <- df$Age **2
df$age_gen <- df$Age * (df$Sex_num)

df$age_cat <- ifelse(df$Age < 11, 'u11', 'missing')

df$age_cat <- ifelse(df$Age >=11 & df$Age < 21, 'u11_20', df$age_cat)
df$age_cat <- ifelse(df$Age >=21 & df$Age < 31, 'u21_30', df$age_cat)

df$age_cat <- ifelse(df$Age >=31 & df$Age < 51, 'u31_50', df$age_cat)
df$age_cat <- ifelse(df$missing_age == 1 , 'missing', df$age_cat)


df$age_cat <- ifelse(df$Age > 50, 'o49', df$age_cat)
df$age_cat <- as.factor(df$age_cat)
df$u17 <- ifelse(df$Age < 17, 1 ,0 ); df[is.na(df$Age), 'u17'] = 0 ; df$u17 <- as.factor(df$u17); summary(df$u17)

colnames(df)[colSums(is.na(df)) > 0]
write.csv(df, 'cleaned_train_test.csv')
