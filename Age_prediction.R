setwd("C:\\Users\\hp\\Desktop\\R_Course\\Kaggle")
titanic.train<- read.csv(file="train.csv",stringsAsFactors=FALSE,header=TRUE)
titanic.test<- read.csv(file="test.csv",stringsAsFactors=FALSE,header=TRUE)

titanic.train$IsTrainSet<-TRUE # To take agin separate for training and testing
titanic.test$IsTrainSet<-FALSE

names(titanic.test)  # To check if col names are same
names(titanic.train)

# We want to merge both for cleaning and then separate
#Test set has one less column- "Survived"..so will add 

titanic.test$Survived<- NA

#Combine both 
titanic.full <- rbind(titanic.test,titanic.train)
table(titanic.full$IsTrainSet) # Just to check 
table(titanic.full$Embarked) # 2 empty strings

# Cleaning Data...

#Embarked
titanic.full[titanic.full$Embarked=='',"Embarked"] <-'S' #Replaced with MOde

table(titanic.full$Embarked) # Values added to S


titanic.full$Pclass<- as.factor(titanic.full$Pclass) #Converting into factor
titanic.full$Sex<- as.factor(titanic.full$Sex)
titanic.full$Embarked<- as.factor(titanic.full$Embarked)



table(is.na(titanic.full$Age))

boxplot(titanic.full$Age)
boxplot.stats(titanic.full$Age)

upper.whisker<- boxplot.stats(titanic.full$Age)$stats[5]

outlier.filter<- titanic.full$Age < upper.whisker
titanic.full[outlier.filter,]

str(titanic.full)

age.equation <- "Age ~ Fare+Pclass+Parch+SibSp+Embarked+Sex"

age.model<- lm(
  formula = age.equation,
  data = titanic.full[outlier.filter,]
)

age.row <- titanic.full[is.na(titanic.full$Age), c("Pclass", "Sex", "Fare", "SibSp", "Embarked", "Parch", "Cabin") ]
table(is.na(titanic.full$Age))
age.prediction <- predict(age.model, newdata = age.row)
titanic.full[is.na(titanic.full$Age),"Age"] <- age.prediction

table(is.na(titanic.full$Age))
titanic.full$age <- NULL


# SPlit back into train and test using IsTrainSet 
titanic.train<- titanic.full[titanic.full$IsTrainSet=="TRUE",]
titanic.test<- titanic.full[!titanic.full$IsTrainSet=="TRUE",]

titanic.train$Survived<- as.factor(titanic.train$Survived)
table(is.na(titanic.test$Age))

install.packages("randomForest")

library("randomForest")

survived.equation<- "Survived ~ Pclass + Sex + Age + SibSp + Embarked + Parch + Cabin + Fare "

survived.formula<- as.formula(survived.equation)

titanic.model <- randomForest(formula = survived.formula, data= titanic.train, ntree = 500,mtry = 3, nodesize = 0.01*nrow(titanic.test))

features.equation <- "Pclass + Sex + Age + SibSp + Embarked + Parch + Cabin + Fare "
Survived <- predict(titanic.model, newdata = titanic.test)
Survived

PassengerId<- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)

output.df$Survived <- as.data.frame(Survived)
output.df

write.csv(output.df, file = "kaggle_submission_2.csv", row.names = FALSE)


