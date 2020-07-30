install.packages("mice")
library(mice)
install.packages("VIM")
install.packages("SparseM")
library(VIM)
library(SparseM)

#0 - reading data
dataset <- read.csv('titanicDataset.csv', header=TRUE)
workingData <- dataset
#1 - structure verification

#1.1 Missing values
workingData[workingData == ""] <- NA
countNA <- as.data.frame(sapply(workingData, function(x) sum(is.na(x))))

#age missing 263 times
#fare missing once
#cabin missing 1014 times
#embarked missing twice

#basic info about missing data for selected variables
md.pattern(workingData)

#more precise info about misisng data for variables
mice_plot <- aggr(workingData, col=c('blue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(workingData), cex.axis=.7,
                  gap=1, ylab=c("Missing data","Pattern"))

#1.2 # Data structure for sex, pclass, embarked
install.packages('plyr')
install.packages('ggpubr')
library(plyr)
library(ggplot2)
library(ggpubr)

# SEX - no missing data
#freq
sexStruct <- count(workingData, 'sex')
#female	466
#male	843
sexPercentages <- c()
for (row in 1:length(c(sexStruct$sex))) {
  sexPercentages <- append(sexPercentages,round((sexStruct$freq[row]/sum(sexStruct$freq))*100,digits=2))
}
sexStruct$percentages <- as.data.frame(sexPercentages)

sexStruct <- sexStruct %>%
  arrange(desc(sex)) %>%
  mutate(pos = round(cumsum(sexPercentages) - 0.5*sexPercentages, digits = 2))
sexStruct

#bar graph for sex
ggplot(workingData, aes(sex)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
ggplot(sexStruct, aes(x = "", y = sexPercentages, fill = sex)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = pos, label = sexPercentages), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()


#PCLASS - nomissing data
#freq
pclassStruct <- count(workingData, 'pclass')
#1	323
#2	277
#3 709
pclassPercentages <- c()
for (row in 1:length(c(pclassStruct$pclass))) {
  pclassPercentages <- append(pclassPercentages,round((pclassStruct$freq[row]/sum(pclassStruct$freq))*100,digits=2))
}
pclassStruct$percentages <- as.data.frame(pclassPercentages)

pclassStruct <- pclassStruct %>%
  arrange(desc(pclass)) %>%
  mutate(pos = round(cumsum(pclassPercentages) - 0.5*pclassPercentages, digits = 2))
pclassStruct

pclassStruct$pclass <- as.factor(pclassStruct$pclass)

#bar graph for pclass
ggplot(workingData, aes(pclass)) +
  geom_bar(fill = c("#0073C2FF", "#EFC000FF", "#CD534CFF")) +
  theme_pubclean()

mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
ggplot(pclassStruct, aes(x = "", y = pclassPercentages, fill = pclass)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = pos, label = pclassPercentages), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()


#EMBARKED - 2 missing values -> skipping NA
#freq
embarkedStruct <- count(workingData, 'embarked')
#C	270
#Q	123
#S 914
#NA - unspecified 

embarkedPercentages <- c()
for (row in 1:length(c(embarkedStruct$embarked))) {
  embarkedPercentages <- append(embarkedPercentages,round((embarkedStruct$freq[row]/sum(embarkedStruct$freq))*100,digits=2))
}
embarkedStruct$percentages <- as.data.frame(embarkedPercentages)

embarkedStruct <- embarkedStruct %>%
  arrange(desc(embarked)) %>%
  mutate(pos = round(cumsum(embarkedPercentages) - 0.5*embarkedPercentages, digits = 2))
embarkedStruct

#embarkedStruct$embarked <- as.factor(embarkedStruct$embarked)

#bar graph for embarked
ggplot(embarkedStruct, aes(embarked)) +
  geom_bar(fill = c("#0073C2FF", "#EFC000FF", "#CD534CFF",'GREEN')) +
  theme_pubclean()

mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF")
ggplot(embarkedStruct, aes(x = "", y = embarkedPercentages, fill = embarked)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = pos, label = embarkedPercentages), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()


#2 - frequencies, percentages for survived
#freq
survivedStruct <- count(workingData, 'survived')
#0 - not survived - 809
#1 - survived - 500

survivedPercentages <- c()
for (row in 1:length(c(survivedStruct$survived))) {
  survivedPercentages <- append(survivedPercentages,round((survivedStruct$freq[row]/sum(survivedStruct$freq))*100,digits=2))
}

survivedStruct$percentages <- as.data.frame(survivedPercentages)
#0 - not survived - 61,8%
#1 - survived - 38,2%

#3 -> point #1

#4 - descriptive statistics for age
summary(workingData$age)

#not applying imputations -> 263 values missing
#min - 0.1667 -> Youngest person was a baby younger than a year
#max - 80.00 -> Oldest person was 80 years old
#avg - 29.88 -> The average of age among titanic passengers was 29.88 years
#med - 28.00 -> The median of age among titanic passengers was 28.00 years
#Q1 - 21.00 -> 25% of titanic passengers were in the age <= 21.00 years and 75% of passengers were in the age >= 21.00
#Q3 - 39.00 -> 75% of titanic passengers were in the age <= 39.00 years and 25% of passengers were in thhe age >= 39.00 

ageHist <- hist(workingData$age,
     main="Histogram of titanic passengers' age",
     xlab="Age",
     ylab="Number of passengers",
     col="red",
     freq=TRUE
)
text(ageHist$mids,ageHist$counts,labels=ageHist$counts, adj=c(0.5, -0.5))

#4 - descriptive statistics for fare
summary(workingData$fare)

#not applying imputations -> 1 value missing
#min - 0.000 -> We can quess -> some people were traveling for free (invitation, reward etc.)
#max - 512.329 -> Value of most expensive ticket was 512.32 in unspecified currency
#avg - 33.295 -> The average of ticket prices paid by the passengers was 33.29 in uspecified currency
#med - 14.454 -> The median value of ticket price paid by the passengers was 14.45 in unspecified currency
#Q1 - 7.896 -> 25% of titanic passengers had to pay a price <= 7.89 and 75% of them had to pay >= 7.89
#Q3 - 31.275 -> 75% of titanic passengers had to pay a price <= 31.27  and 25% of them had to pay >= 31.27 

priceHist <- hist(workingData$fare,
     main="Histogram of titanic ticket prices",
     xlab="Fare value",
     yLab="Number of tickets in specific price",
     col="red",
     freq=TRUE,
     xlim=c(0,520)
)
text(priceHist$mids,priceHist$counts,labels=priceHist$counts, adj=c(0.5, -0.5))

#5 Scatterplot for age & price

attach(mtcars)
plot(workingData$age, workingData$fare, main="Age vs Price", 
     xlab="Age ", ylab="Price for ticket", pch=19, col='blue')
abline(lm(workingData$fare~workingData$age), col="red") 

cor.test(workingData$age, workingData$fare, method="pearson")
#H0: correlation = 0
#H1: correlation != 0
#p-val -> <0.05
#=> H1 -> cor - 0.1787394 -> weak positive correlation. 
#Indicates a weak linear relationship between the variables -> the age does not define the price of the ticket

#6 spliting for training and test set with seed 
#ommiting rows with NA values in dataset
fullDataset <- na.omit(workingData)
require(caTools)

set.seed(123) 
sample = sample.split(fullDataset,SplitRatio = 0.70) 
trainingSet =subset(fullDataset,sample ==TRUE) #180 observations
testSet=subset(fullDataset, sample==FALSE) #90 observations

#7 
#SURVIVED FOR TEST SET

#freq
testSetSurvivedStruct <- count(testSet, 'survived')

testSetSurvivedPercentages <- c()
for (row in 1:length(c(testSetSurvivedStruct$survived))) {
  testSetSurvivedPercentages <- append(testSetSurvivedPercentages,round((testSetSurvivedStruct$freq[row]/sum(testSetSurvivedStruct$freq))*100,digits=2))
}

testSetSurvivedStruct$percentages <- as.data.frame(testSetSurvivedPercentages)
#0 - not survived - 35 people | 38,89%
#1 - survived - 55 people | 61,11%


#SURVIVED FOR TRAINING SET

#freq
trainingSetSurvivedStruct <- count(trainingSet, 'survived')
trainingSetSurvivedPercentages <- c()
for (row in 1:length(c(trainingSetSurvivedStruct$survived))) {
  trainingSetSurvivedPercentages <- append(trainingSetSurvivedPercentages,round((trainingSetSurvivedStruct$freq[row]/sum(trainingSetSurvivedStruct$freq))*100,digits=2))
}

trainingSetSurvivedStruct$percentages <- as.data.frame(trainingSetSurvivedPercentages)
#0 - not survived - 55 people | 30,56%
#1 - survived - 125 people | 69,44%

#BACK TO 6 -> spliting without ommitin NA rows
set.seed(123) 
sampleProper = sample.split(workingData,SplitRatio = 0.70) 
trainingSetProper =subset(workingData,sampleProper ==TRUE) #873 observations
testSetProper =subset(workingData, sampleProper==FALSE) #436 observations

#8 Replace missing values in Embarked with "S" for both sets
trainingSetProper$embarked[is.na(trainingSetProper$embarked)] <- "S"
testSetProper$embarked[is.na(testSetProper$embarked)] <- "S"

#9 -> before replacing duplicating datasets for A and B points
trainingSetProperA <- trainingSetProper
trainingSetProperB <- trainingSetProper
testSetProperA <- testSetProper
testSetProperB <- testSetProper

#9+10 A 
#mean for age for full dataset
summary(workingData$age) #-> 29.881

#simple replace with mean for training and test set for Age values
trainingSetProperA$age[is.na(trainingSetProperA$age)] <- 29.8810
testSetProperA$age[is.na(testSetProperA$age)] <- 29.8810


#9+10 B
#USING HMISC IMPUTATION METHOD
install.packages("Hmisc")
install.packages("survival")
library(Hmisc)
library(survival)

#TRAINING SET
#preview for some NA values
head(trainingSetProperB$age, n=30)

#add new column with imputed values
trainingSetProperB$imputed_age <- with(trainingSetProperB, impute(age,median))
#check where it was imputed compare original with imputed in dataset
head(trainingSetProperB$imputed_age, n=30)

#TEST SET
#preview for some NA values
head(testSetProperB$age, n=30)

#add new column with imputed values
testSetProperB$imputed_age <- with(testSetProperB, impute(age,median))
#check where it was imputed compare original with imputed in dataset
head(testSetProperB$imputed_age, n=30)

#11 Setting Generalized Linear Model for Logit Regression with LOGIT link function
install.packages("tidyverse")
library(tidyverse)

selectedDatasetA <- trainingSetProperA %>% select('age','sex','pclass','embarked','survived')
selectedTestDatasetA <- testSetProperA %>% select('age','sex','pclass','embarked','survived')

#replacing pclass for factors 
selectedDatasetA$pclass[selectedDatasetA$pclass == 1] <- "first"
selectedDatasetA$pclass[selectedDatasetA$pclass == 2] <- "second"
selectedDatasetA$pclass[selectedDatasetA$pclass == 3] <- "third"

selectedTestDatasetA$pclass[selectedTestDatasetA$pclass == 1] <- "first"
selectedTestDatasetA$pclass[selectedTestDatasetA$pclass == 2] <- "second"
selectedTestDatasetA$pclass[selectedTestDatasetA$pclass == 3] <- "third"

write.csv(selectedDatasetA, "trainingDatasetA", )

#simple linear regression using general linear model
modelGLM <- glm(formula = survived ~ .,family=binomial,data=selectedDatasetA)
#details of logistic regression
summary(modelGLM)

#significant factors - sex, pclass, age, embarked is not significant -> not taking into the account during further analysis
#AIC for comparing models - 844.1 
#model formula
#hasSurvived = 3.496 - 0.03(age) - 2.44(the patient is male) - 1.0596(travelingBy2ndClass) - 1.879115(travelingBy3rdClass) - 0.595(GotOnBoarnInPortQ) -0.4122(GotOnBoardInPortS)
#Along with increase of age by 1 year the chances of survival were decreasing by 3% ceteris paribus
#Changes of survival for men were decreased by 244% than in case of female ceteris paribus
#Chances of survival for passengers traveling in the second class were lower by 105.9% than for passengers traveling in first class ceteris paribus
#Chances of survival for pasengers traveling in third class were lower by 187,9% than for passengers traveling first class ceteris paribus

#predicted probability, response - gives probabilities in one vector
pred_probability <- predict(modelGLM, type = 'response', newdata = selectedTestDatasetA[1:4])
#set of all probabilities for each record in test dataset
pred_probability
#vector of predicted values for chance of survival 1- yes 0 - no
predY_A <- ifelse(pred_probability > 0.5, 1, 0)
predY_A

#add column to testSet to visualy verify succesfull prdictions by comparing records from the dataset
selectedTestDatasetA$predictedGLM <- predY_A

#confusion matrix
confMatrixGLM <- table(selectedTestDatasetA$survived,predY_A)
confMatrixGLM

#accuracy => (TP + TN) / (TP + TN + FP + FN) => 0.793578 ~ 79,4% success in predicting survived cases
ACC_GLM = (110 + 236) / (110 + 236 + 44 + 46)
ACC_GLM
#sensitivity => TP/(TP+FN) => 0.7051282 ~ 70,5% success in predicting the real survived cases -> Quite ok
SENS_GLM <- 110/(110+46)
SENS_GLM
#specificity => TN/(FP+TN) => 0.8428571 ~ 84,2% -> success in predicting real not survived cases => Quite good
SPEC_GLM <- 236/(44+236)
SPEC_GLM

library(pROC)
#AUC + ROC
gGLM <- roc(survived ~ pred_probability, data = selectedTestDatasetA[1:5])
gGLM
#AUC => 0.8462
plot(gGLM, print.auc = TRUE, xlab="False Positive Percentage", ylab="True Positive Percentage", print.)

#F1 score => 
#precision = TP / (TP + FP) 

#recall = TP / (TP + FN) => SENSITIVITY

precisionGLM <- 110/(110+44)
recallGLM <- SENS_GLM
#F1 = 2((precision x recall)/(precision + recall))
F1ScoreGLM <- 2*((precisionGLM*recallGLM)/(precisionGLM+recallGLM))
F1ScoreGLM

#F1 score = 0.7096774 => 70,9 % sucessful prediction score

#visualisation
install.packages('ElemStatLearn')
library(ElemStatLearn)

install.packages("pscl")
library(pscl)

#12

#counting pseudo R2 to validate model fit
pR2(modelGLM) #McFadden pR2 > 0.3231 -> ~ 32% - very low 
library(ROCR) 
#measuring accuracy with test set
fitted.results <- predict(modelGLM,selectedTestDatasetA)
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != selectedTestDatasetA$survived)
print(paste('Accuracy',1-misClasificError))

"Accuracy 0.793577" # => 0.79355 => 79.35% success prediction for testSet 


#13 & 14 Random forrest
selectedDatasetRandomForest <- trainingSetProperA %>% select('age','sex','pclass','embarked','survived')
selectedTestDatasetRandomForest <- testSetProperA %>% select('age','sex','pclass','embarked','survived')

#make dependent variable categorical
selectedDatasetRandomForest$survived <- as.factor(selectedDatasetRandomForest$survived)

#random forest classification

library(randomForest)
rForest <- rf <- randomForest(
  survived ~ .,
  data=selectedDatasetRandomForest
)

rForest

#results
#OOB estimate of  error rate: 20.62%
#Confusion matrix:
#  0   1 class.error
#0 482  45  0.08538899
#1 135 211  0.39017341

#error misclasified to total observations - 19.72%
#accuracy = 1-OOB => 80,28% => it's quite ok
#sensitivity => TP/(TP+FN) => 0.7811 ~ 78% success in predicting the real survived cases -> Quite good
SENS_RandomForest <- 482/(482+135)
SENS_RandomForest
#specificity => TN/(FP+TN) => 0.82421 ~ 82% -> success in predicting real not survived cases => Quite good
SPEC_RandomForest <- 211/(45+211)
SPEC_RandomForest


predRandomForest <- predict(rForest,type = "prob")
install.packages("ROCR")
library(ROCR)
performanceRandomForest <- prediction(predRandomForest[,2], selectedDatasetRandomForest$survived)
aucRandomForest <- performance(performanceRandomForest, "auc")

#True Positive and Negative Rate
predRandomForest2 = performance(performanceRandomForest, "tpr","fpr")

plot(predRandomForest2,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")



