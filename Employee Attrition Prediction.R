#loading packages
install.packages("caret")
install.packages("naivebayes")
install.packages("rpart.plot")
install.packages("randomForest")
library(moments)
library(Hmisc)
library(lm.beta)
library(rstatix)
library(dplyr)
library(DescTools)
library(moments)
library(ggplot2)
library(purrr)
library(Hmisc)
library(regclass)
library(dvmisc)
library(caret)
library(naivebayes)
library(rpart.plot)
library(randomForest)

# loading data
EA <- EmployeeAttrition
EA <- as.data.frame(EA)

# Correcting the data types
str(EA)
EA[,c(2,3,4,6,8,9,10)] <- lapply(EA[,c(2,3,4,6,8,9,10)], factor)#categorical to factors
EA[,c(12,14)] <- lapply(EA[,c(12,14)],as.numeric)#Char data type to numeric data type

# Subsetting into required dataset/ Removing unwanted column (Employee ID)
EA <- EA[,c(-7)]
str(EA)

#Dealing with Missing Data - Imputing the missing values
map(EA,~sum(is.na(.)))
EA[11]<-impute(EA[11],fun = median) #NumCompaniesWorked
EA[13]<-impute(EA[13],fun = median) #TotalWorkingYears

#Checking Normality and outliers
skewness(EA[c(1,5,10,12,13,14,15,16)])
kurtosis(EA[c(1,5,10,12,13,14,15,16)])
boxplot(EA[c(1,5,10,12,13,14,15,16)])
# Outliers found in column numbers 10, 13, 14, 15, 16

#Storing Outliers in Vectors
out10<-boxplot(EA$MonthlyIncome)$out
out13<-boxplot(EA$TotalWorkingYears)$out
out14<-boxplot(EA$YearsAtCompany)$out
out15<-boxplot(EA$YearsSinceLastPromotion)$out
out16<-boxplot(EA$YearsWithCurrManager)$out

# Num of Outliers in each vector 
length(out10)
length(out13)
length(out14)
length(out15)
length(out16)


#################################### TREATMENT FOR NORMALITY & OUTLIERS #######################################################

# Creating Data Copies
EA1<-EA
EA2<-EA
EA3<-EA

#Method 1: Removing Outliers in EA1 -----------------------------------------------------------------------------------------
EA1<-EA1[-which(EA1$MonthlyIncome %in% out10),]
EA1<-EA1[-which(EA1$TotalWorkingYears %in% out13),]
EA1<-EA1[-which(EA1$YearsAtCompany %in% out14),]
EA1<-EA1[-which(EA1$YearsSinceLastPromotion %in% out15),]
EA1<-EA1[-which(EA1$YearsWithCurrManager %in% out16),]

boxplot(EA1[c(1,5,10,12,13,14,15,16)])
skewness(EA1[c(1,5,10,12,13,14,15,16)])
kurtosis(EA1[c(1,5,10,12,13,14,15,16)])


#Method 2: Imputing Outliers in EA2 -----------------------------------------------------------------------------------------
# Replacing Outliers with NAs
EA2[EA2$MonthlyIncome %in% out10, "MonthlyIncome"]= NA
EA2[EA2$TotalWorkingYears %in% out13, "TotalWorkingYears"]= NA
EA2[EA2$YearsAtCompany %in% out14, "YearsAtCompany"]= NA
EA2[EA2$YearsSinceLastPromotion %in% out15, "YearsSinceLastPromotion"]= NA
EA2[EA2$YearsWithCurrManager %in% out16, "YearsWithCurrManager"]= NA

# Imputing those NAs
EA2[10]<-impute(EA2[10],fun = median)
EA2[13]<-impute(EA2[13],fun = median)
EA2[14]<-impute(EA2[14],fun = median)
EA2[15]<-impute(EA2[15],fun = median)
EA2[16]<-impute(EA2[16],fun = median)

boxplot(EA2[c(1,5,10,12,13,14,15,16)])
skewness(EA2[c(1,5,10,12,13,14,15,16)])
kurtosis(EA2[c(1,5,10,12,13,14,15,16)])

#Method 3: Transformation of Variable in EA3 -------------------------------------------------------------------------------

# Creating copies of EA3 
EA3a<-EA3
EA3b<-EA3

# Square Root Transformation in EA3a
EA3a$MonthlyIncome <- sqrt(EA3a$MonthlyIncome)
EA3a$TotalWorkingYears <- sqrt(EA3a$TotalWorkingYears)
EA3a$YearsAtCompany <- sqrt(EA3a$YearsAtCompany)
EA3a$YearsSinceLastPromotion <- sqrt(EA3a$YearsSinceLastPromotion)
EA3a$YearsWithCurrManager <- sqrt(EA3a$YearsWithCurrManager)
skewness(EA3a[c(1,5,10,12,13,14,15,16)])
kurtosis(EA3a[c(1,5,10,12,13,14,15,16)])
boxplot(EA3a[c(1,5,10,12,13,14,15,16)])

# Log Transformation in EA3b
EA3b$MonthlyIncome <- log10(EA3b$MonthlyIncome)
EA3b$TotalWorkingYears <- log10(EA3b$TotalWorkingYears+1)
EA3b$YearsAtCompany <- log10(EA3b$YearsAtCompany+1)
EA3b$YearsSinceLastPromotion <- log10(EA3b$YearsSinceLastPromotion+1)
EA3b$YearsWithCurrManager <- log10(EA3b$YearsWithCurrManager+1)
skewness(EA3b[c(1,5,10,12,13,14,15,16)])
kurtosis(EA3b[c(1,5,10,12,13,14,15,16)])
boxplot(EA3b[c(1,5,10,12,13,14,15,16)])

# Performing sqrt transformation & then removing outliers -----------------------------------------------------------------

# creating copy of sqrt transformed data set
EA5<-EA3a

# storing outliers in vectors & checking number of outliers in each
boxplot(EA5[c(1,5,10,12,13,14,15,16)])
out13b<-boxplot(EA5$TotalWorkingYears)$out
length(out13b)
out14b<-boxplot(EA5$YearsAtCompany)$out
length(out14b)

# Removing the records with outliers
EA5<-EA5[-which(EA5$TotalWorkingYears %in% out13b),]
EA5<-EA5[-which(EA5$YearsAtCompany %in% out14b),]

# Checking normality and outliers
skewness(EA5[c(1,5,10,12,13,14,15,16)])
kurtosis(EA5[c(1,5,10,12,13,14,15,16)])
boxplot(EA[c(1,5,10,12,13,14,15,16)])

# Performing Log transformation & then removing outliers -----------------------------------------------------------------

# creating copy of log transformed data set
EA4<-EA3b 

# storing outliers in vectors & checking number of outliers in each
boxplot(EA4[c(1,5,10,12,13,14,15,16)])
out13a<-boxplot(EA4$TotalWorkingYears)$out
length(out13a)
out14a<-boxplot(EA4$YearsAtCompany)$out
length(out14a)

# Removing the records with outliers
EA4<-EA4[-which(EA4$TotalWorkingYears %in% out13a),]
EA4<-EA4[-which(EA4$YearsAtCompany %in% out14a),]

# Checking normality and outliers
skewness(EA4[c(1,5,10,12,13,14,15,16)])
kurtosis(EA4[c(1,5,10,12,13,14,15,16)])
boxplot(EA4[c(1,5,10,12,13,14,15,16)])


################################### CREATING ML MODELS ####################################################################

# Splitting the final dataset into testing & traning 
set.seed(100)
intrain<-createDataPartition(y = EA4$Attrition, p = 0.8, list = FALSE)
training<-EA4[intrain,]
testing<-EA4[-intrain,]
str(training)
str(testing)

# logistic Regression --------------------------------------------------------------------------------------------------------------

# ASSUMPTIONS FOR LOGISTIC REGRESSION
skewness(EA4[c(1,5,10,12,13,14,15,16)])
kurtosis(EA4[c(1,5,10,12,13,14,15,16)])
X <- EA4[c(1,5,10,12,13,14,15,16)]
rcorr(as.matrix(X))

# Model 1 - logistic regression
M1<-train(data=training,Attrition~.,method="glm", family="binomial")
summary(M1)
predAttritionLR1<-predict(M1,newdata=testing)
confusionMatrix(predAttritionLR1,testing$Attrition, positive = "Yes") #Positive Class is Yes
confusionMatrix(predAttritionLR1,testing$Attrition)

# Model 2 - logistic regression with only significant IVs
M2<-train(data=training,Attrition~BusinessTravel+Department+EducationField+MaritalStatus+NumCompaniesWorked+TotalWorkingYears+YearsSinceLastPromotion,method="glm", family="binomial")
summary(M2)
predAttritionLR2<-predict(M2,newdata=testing)
confusionMatrix(predAttritionLR2,testing$Attrition,positive = "Yes") 
confusionMatrix(predAttritionLR2,testing$Attrition) 

# Naive Bayes -------------------------------------------------------------------------------------------------------------------------------

# Model 3 - Naive Bayes
M3<- train(Attrition~., data=training, method="naive_bayes")
predAttritionNB1<-predict(M3, newdata = testing)
confusionMatrix(predAttritionNB1, testing$Attrition)
confusionMatrix(predAttritionNB1, testing$Attrition, positive = "Yes")

# Decision Trees ------------------------------------------------------------------------------------------------------------------------------

# Model 4 - Decision Tree - Gini Index
M4<-train(data=training,Attrition~., method="rpart")
rpart.plot(M4$finalModel, extra=104)
predAttritionDT1<-predict(M4, newdata = testing)
confusionMatrix(predAttritionDT1, testing$Attrition)
confusionMatrix(predAttritionDT1, testing$Attrition, positive = "Yes")

# Model 5 - Decision Tree - Information Gain
M5<-train(data=training,Attrition~., method="rpart",parms=list(split="information"))
rpart.plot(M5$finalModel)
predAttritionDT2<-predict(M5, newdata = testing)
confusionMatrix(predAttritionDT2, testing$Attrition)
confusionMatrix(predAttritionDT2, testing$Attrition, positive = "Yes")

# Random Forest ---------------------------------------------------------------------------------------------------------------

# Model 6 - Random Forest
M6 <- train(data=training,Attrition~., method="rf")
M6$finalModel
predAttritionRF <- predict(M6, newdata=testing)
confusionMatrix(predAttritionRF, testing$Attrition)
confusionMatrix(predAttritionRF, testing$Attrition, positive="Yes")

############################## CREATING ML MODELS (NON PARAMETRIC) ##########################################################################################################################################################################################################################

# Let's create a copy of the original dataset 
EA_copy <- EA

# Splitting the final dataset into testing & traning 
set.seed(100)
intrainC <-createDataPartition(y = EA_copy$Attrition, p = 0.8, list = FALSE)
trainingC <-EA_copy[intrain,]
testingC <-EA_copy[-intrain,]
str(trainingC)
str(testingC)

# Model 7 - Decision Tree - Gini Index
M7<-train(data=trainingC,Attrition~., method="rpart")
rpart.plot(M7$finalModel)
predAttritionDT3<-predict(M7, newdata = testingC)
confusionMatrix(predAttritionDT3, testingC$Attrition, positive = "Yes")

# Model 8 - Decision Tree - Information Gain
M8<-train(data=trainingC,Attrition~., method="rpart",parms=list(split="information"))
rpart.plot(M8$finalModel, extra=104)
predAttritionDT4<-predict(M8, newdata = testingC)
confusionMatrix(predAttritionDT4, testingC$Attrition, positive = "Yes")

# Model 9 - Random Forest
M9 <- train(data=trainingC,Attrition~., method="rf")
M9$finalModel
predAttritionRF2 <- predict(M9, newdata=testingC)
confusionMatrix(predAttritionRF2, testingC$Attrition, positive="Yes")

################################################################################################################################
