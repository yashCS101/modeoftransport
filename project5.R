setwd('C:/Users/USER/Desktop/R Data Sets')

### Importing data
data  =  read.csv("Cars_edited.csv")

### Libraries 


### Data understanding
dim(data)

str(data)

summary(data)

# Checking for NA
sapply(data,function(x) sum(is.na(x)))

#Removing NA
data = na.omit(data)

# Checking # of unique values in each column
sapply(data,function(x) length(unique(x)))

# Converting Transport values to 0 or 1 
data$Transport = ifelse(data$Transport == 'Car', 1,0)
# Male = 1 , Female = 0
data$Gender = ifelse(data$Gender == 'Male', 1,0)
table(data$Transport)

sum(data$Transport == 1)/nrow(data)

# Binary variables needs to be converted into factor variables
data$Gender = as.factor(data$Gender)
data$Engineer = as.factor(data$Engineer)
data$MBA = as.factor(data$MBA)
data$license = as.factor(data$license)
data$Transport = as.factor(data$Transport)

#Co-relation check 
data$Gender = as.numeric(data$Gender)
data$Engineer = as.numeric(data$Engineer)
data$MBA = as.numeric(data$MBA)

data$Transport = as.numeric(data$Transport)
library(corrplot)
correlations = cor(data[,-9]) 
summary(correlations)
corrplot(correlations, type="lower", method = 'number', diag = FALSE)

#PCA

eigendata = eigen(correlations)
eigendata
eigenvalues = eigendata$values
eigenvalues
Factor = c(1,2,3,4,5,6,7,8)
Scree = data.frame(Factor,eigenvalues)
plot(Scree, main = "Scree Plot", col = "Blue", ylim = c(0,4))
lines(Scree,col="Red")
library(psych)
pcadatar = principal(data[,-9], nfactors = 3, rotate = "varimax")
pcadatar

pcadata = data.frame(cbind(pcadatar$scores,data$Transport))

pca_train = subset(pcadata, split == TRUE)
pca_test = subset(pcadata, split == FALSE)
str(pca_train)
pca_train$V4 = as.factor(pca_train$V4)
pca_test$V4 = as.factor(pca_test$V4)
pca_logistic = glm(V4~., data=pca_train, family=binomial(link="logit"))

summary(pca_logistic)

pca_test$log.pred = predict(pca_logistic, pca_test[1:3], type="response")

table(pca_test$V4,pca_test$log.pred>0.5)
install.packages("confusionMatrix")
library(caret)
pca_fac = ifelse(pca_test$log.pred>0.5,1,0)
pca_fac=as.factor(pca_fac)
pca_test$V4=as.factor(pca_test$V4)
confusionMatrix(pca_fac,pca_test$V4)

# Univariate Analysis 
boxplot(data)

# Bivariate Analysis Continious variables 
attach(data)
library(ggplot2)

boxplot(Age~Transport)

ggplot(data, aes(x = Age, y = Transport)) + 
  geom_density(aes(fill = Transport), alpha = 0.3) + 
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) + 
  scale_fill_manual(values = c("darkturquoise", "lightcoral")) + xlim(-30,250)

ggplot(data, aes(x = Transport, y = Gender, fill = Transport)) + geom_boxplot(alpha=0.7)

#Gender vs Transport ???
ggplot(data, aes(x= Gender))  + theme_bw()+ 
  facet_wrap(~Transport ) + geom_bar()+ 
  labs(y= "No. Of People", title = "Gender vs Transport")

# Cars vs gender:
prop.table(table(data$Gender))
ggplot(data, aes(x=Work.Exp)) + geom_histogram(binwidth = 1)
ggplot(data, aes(x=Transport)) + geom_bar() + theme_bw()
ggplot(data, aes(x=Gender, fill= Transport)) + geom_bar()

#Age & Transport as car:
ggplot(data, aes(x=Transport))
ggplot(data, aes(x=Age)) + geom_histogram(binwidth = 5) 

ggplot(data, aes(x=Age)) + geom_histogram(binwidth = 1) +
  facet_wrap(~Transport)

#license & Transport as car:
ggplot(data, aes(x=license)) + geom_bar()
nrow(data)
ggplot(data, aes(x=license)) + geom_bar()  + facet_wrap(~Transport)
table(data$Transport)



#Bi variate Analysis Factor Variables 
barplot(table(Transport, Transport), col = c("green","red"), main = 'Transport')
barplot(table(Transport, Gender) , col = c("green","red"), main = 'Gender')
barplot(table(Transport, Engineer), col = c("green","red"), main = 'Engineer')
barplot(table(Transport, MBA), col = c("green","red"), main = 'MBA')
barplot(table(Transport, license), col = c("green","red"), main = 'License')

#Defining Split 
set.seed(001)
library(caTools) #for sample.split
split = sample.split(data$Transport, SplitRatio = 0.75)

#Logisticregression(without SMOTE)
logis_train = subset(data[,-5], split == TRUE)
logis_test = subset(data[,-5], split == FALSE)

table(logis_train$Transport)

table(logis_test$Transport)

nosmote_logistic = glm(Transport~., data=logis_train, family=binomial(link="logit"))

summary(nosmote_logistic)

# Check for multicollinearity
library(car)
vif(nosmote_logistic)

logis_test$log.pred = predict(nosmote_logistic, logis_test[1:8], type="response")

table(logis_test$Transport,logis_test$log.pred>0.5)
## Without smote we correctly predicted 9 but missed on 6

#SMOTE 
library(DMwR)

smote.train = subset(data[,-5], split == TRUE)
smote.test = subset(data[,-5], split == FALSE)

str(smote.train$Transport)
smote.train$Transport = as.factor(smote.train$Transport)
smote.train$Transport = as.factor(smote.train$Transport)

balanced.gd = SMOTE(Transport ~., smote.train, perc.over = 4800, k = 5, perc.under = 200)

table(balanced.gd$Transport)

#Logistic Regression(with SMOTE)
smote_logistic = glm(Transport~., data=balanced.gd, family=binomial(link="logit"))

summary(smote_logistic)

smote.test$log.pred = predict(smote_logistic, smote.test[1:8], type="response")

table(smote.test$Transport,smote.test$log.pred>0.5)

### Smote with PCA Data
smote.train.pca = subset(pcadata, split == TRUE)
smote.test.pca = subset(pcadata, split == FALSE)

str(data$Transport)
smote.train.pca$V4 = as.factor(smote.train.pca$V4)

balanced.gd.pca = SMOTE(V4 ~., smote.train.pca, perc.over = 4800, k = 5, perc.under = 200)

table(balanced.gd.pca$V4)
sum(balanced.gd.pca==1)/nrow(balanced.gd.pca)

#Logistic Regression(with SMOTE PCA Data)
smote_logistic_pca = glm(V4~., data=balanced.gd.pca, family=binomial(link="logit"))

summary(smote_logistic_pca)

smote.test.pca$log.pred = predict(smote_logistic_pca, smote.test.pca[1:3], type="response")

table(smote.test.pca$V4 ,smote.test.pca$log.pred>0.5)

smote_pca_fac = ifelse(smote.test.pca$log.pred>0.5,1,0)
smote_pca_fac=as.factor(smote_pca_fac)
smote.test.pca$V4=as.factor(smote.test.pca$V4)
confusionMatrix(smote_pca_fac,smote.test.pca$V4)
