getwd()
setwd("C:\\Users\\Hp\\Desktop\\Applied AI\\R_Course\\Advanced Analytics Part 2")
getwd()

fin<- read.csv("P3-Future-500-The-Dataset.csv", na.strings = c(""))  #So that all empty spaces are changed tp NA
head(fin)
tail(fin)
str(fin)

#Data Cleaning

fin$ID <- factor(fin$ID)
fin$Inception <- factor(fin$Inception)

fin$Expenses <- gsub(" Dollars","",fin$Expenses) #Removing Dollars from Expenses...auto converted into char from a factor
fin$Expenses <- gsub(",","",fin$Expenses) #Removing ',' from Expenses
fin$Expenses <-  as.numeric(fin$Expenses)


fin$Revenue <- gsub("\\$","",fin$Revenue) #Removing '$'  and ',' sign from Expenses
fin$Revenue <- gsub(",","",fin$Revenue)
fin$Revenue <-  as.numeric(fin$Revenue)


fin$Growth <- gsub("%","",fin$Growth)
fin$Growth <- as.numeric(fin$Growth)

#Locating Missing Data

complete.cases(fin)
fin[!complete.cases(fin),]

#Mediant Imputation for NA values in Dataset

fin_backup <- fin

median_emp_retail <- median(fin[fin$Industry == "Retail","Employees"],na.rm = T)
median_emp_retail
fin[is.na(fin$Employees) & fin$Industry == "Retail", "Employees"] <- median_emp_retail  # Median imputation for Employees in  Retail Industry 

fin[!complete.cases(fin),]

median_emp_FinServices <- median(fin[fin$Industry == 'Financial Services',"Employees"],na.rm = T)
median_emp_FinServices
fin[fin$Industry == "Financial Services" & is.na(fin$Employees), "Employees"] <- median_imp_FinServices ## Median imputation for Employees in  Financial Services Industry 

fin[!complete.cases(fin),]

median_growth_construction <- median(fin[fin$Industry == "Construction", "Growth"], na.rm = T)
median_growth_construction
fin[fin$Industry == 'Construction' & is.na(fin$Growth), "Growth"] <- median_growth_construction  ## Median imputation for Growth in Construction Industry 

median_exp_construction <- median(fin[fin$Industry== "Construction","Expenses"], na.rm = T)
median_exp_construction
fin[fin$Industry == "Construction" & is.na(fin$Expenses) & is.na(fin$Profit), "Expenses"] <- median_exp_construction  #Median imputation for Expenses where Profit,Revenue and Exp were NA
fin[!complete.cases(fin),]

median_rev_construction <- median(fin[fin$Industry== "Construction","Revenue"], na.rm = T)
median_rev_construction
fin[fin$Industry == "Construction" & is.na(fin$Revenue) & is.na(fin$Profit), "Revenue"] <- median_rev_construction #Median imputation for Revenue where Profit,Revenue and Exp were NA

fin[!complete.cases(fin),]

# Imputing values using formula Profit = Revenue - Expenses

fin[is.na(fin$Profit),"Profit"] <- fin[is.na(fin$Profit),"Revenue"] - fin[is.na(fin$Profit),"Expenses"]

fin[is.na(fin$Expenses),"Expenses"] <- fin[is.na(fin$Expenses),"Revenue"] - fin[is.na(fin$Expenses),"Profit"] 

# Imputing NA values in state using City column

fin[fin$City == "New York",]
fin[is.na(fin$State) & fin$City == "New York","State"] <- "NY"

fin[!complete.cases(fin),]

fin[fin$City == "San Francisco",]
fin[is.na(fin$State) & fin$City == "San Francisco","State"] <- "SF"

# Median imputation for Inception 
fin$Inception <- as.numeric(as.character(fin$Inception)) 
median_Inception_Health <- median(fin[fin$Industry == "Health","Inception"],na.rm = T)
fin[is.na(fin$Inception) & fin$Industry == "Health","Inception"] <- median_Inception_Health
fin$Inception <- as.factor(fin$Inception)
str(fin)
fin[!complete.cases(fin),]

#Delete left 2 rows where Industry is NA
fin2<- fin
fin <- fin[!is.na(fin$Industry),]
str(fin)

# Data Visualization
library(ggplot2)

  ## A scatterplot classified by industry showing revenue, expenditure and profit
p<- ggplot(data = fin,aes(x = Revenue, y = Expenses, colour = Industry, size = Profit))
p + geom_point() + geom_smooth(fill = NA, size = 1.2)

  ## Boxplot to analyze the Growth Pattern
f <- ggplot(data = fin, aes(x=Industry, y= Growth, colour= Industry))
f + geom_jitter() + geom_boxplot(size = 1, alpha = 0.5, outlier.color= NA)

# Prediction for growth using Models

  ## Dividing into train and test
fin$Growth <- as.factor(fin$Growth)
typeof(fin$Growth)
str(fin)
fin$Growth
ind <- sample(2,nrow(fin), replace = T, prob = c(0.7,0.3))
train <- fin[ind == 1,]
test <- fin[ind == 2,] 
typeof(train$Growth) 


#growth.model <- "Growth ~  Industry + Inception + Employees + State + City + Revenue + Expenses + Profit


growth.model <- lm(formula = "Growth ~  Industry + Inception + Employees + State + City + Revenue + Expenses + Profit"
,data = train) 
growth.model
growth.pred <- predict(growth.model, data = test)
typeof(growth.pred)
growth.pred <- as.factor(growth.pred)
test$Growth <- as.factor(test$Growth)
nrow(test) 

nrow(growth.pred)

library(caret)
confusionMatrix(growth.pred,test$Growth)





