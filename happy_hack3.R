setwd("/Users/chidam/Downloads/AV hack3")
getwd()
library(sqldf)
train<-read.csv("happy_train.csv",stringsAsFactors=FALSE)
alcohol<-read.csv("happy_alcohol_cons.csv",stringsAsFactors=FALSE)
DF<-sqldf('select a.*,b.Alcohol_Consumption from train a join alcohol b on a.ID=b.ID')
train<-DF
summary(train)
str(train)
#table(train$Residence_Region)
train$Var1=as.factor(train$Var1)
train$WorkStatus=as.factor(train$WorkStatus)
train$Divorce=as.factor(train$Divorce)
train$Widowed=as.factor(train$Widowed)
train$Residence_Region=as.factor(train$Residence_Region)
train$income=as.factor(train$income)
train$Engagement_Religion=as.factor(train$Engagement_Religion)
train$Var2=as.factor(train$Var2)
train$Gender=as.factor(train$Gender)
train$Unemployed10=as.factor(train$Unemployed10)
train$Happy=as.factor(train$Happy)
train$Alcohol_Consumption=as.factor(train$Alcohol_Consumption)

# Special handling
summary(train$Var1)
train$Divorce[is.na(train$Divorce)]=as.factor("no")
train$TVhours[is.na(train$TVhours)]=median(train$TVhours,na.rm=TRUE)
#train$Unemployed10[is.na(train$Unemployed10[!is.na(train$income)])]=0

#ignore1
plot(train$income,as.numeric(train$Score))
summary(train$income[is.na(train$Score)])
tapply(train$Score,train$income,median,na.rm=TRUE)
## 40 , 42, 46
#not_happy=train$Happy[as.character(train$Happy)=="Not Happy"]
#levels(not_happy)=c(1,2,3)
train$Score[is.na(train$Score) & as.character(train$income)=="$1000 to 2999"] = 100
train$Score[is.na(train$Score) & as.character(train$income)=="$10000 - 14999"] = 761
train$Score[is.na(train$Score) & as.character(train$income)=="$15000 - 19999"] = 671
train$Score[is.na(train$Score) & as.character(train$income)=="$20000 - 24999"] = 780
train$Score[is.na(train$Score) & as.character(train$income)=="$25000 or more"] = 5896
train$Score[is.na(train$Score) & as.character(train$income)=="$3000 to 3999"] = 88
train$Score[is.na(train$Score) & as.character(train$income)=="$4000 to 4999"] = 95
train$Score[is.na(train$Score) & as.character(train$income)=="$5000 to 5999"] = 132
train$Score[is.na(train$Score) & as.character(train$income)=="$6000 to 6999"] = 123
train$Score[is.na(train$Score) & as.character(train$income)=="$7000 to 7999"] = 133
train$Score[is.na(train$Score) & as.character(train$income)=="$8000 to 9999"] = 243
train$Score[is.na(train$Score) & as.character(train$income)=="lt $1000"] = 111
 

## ignore1 end
## ignore2
plot(train$Happy,as.numeric(train$Score))
summary(train$Happy[is.na(train$Score)])
tapply(train$Score,train$Happy,median,na.rm=TRUE)
## 41 , 43, 46
#not_happy=train$Happy[as.character(train$Happy)=="Not Happy"]
#levels(not_happy)=c(1,2,3)
train$Score[is.na(train$Score) & as.character(train$Happy)=="Not Happy"] = 41
train$Score[is.na(train$Score) & as.character(train$Happy)=="Pretty Happy"] = 43
train$Score[is.na(train$Score) & as.character(train$Happy)=="Very Happy"] = 46
## ignore2 end
#train$Unemployed10=as.factor(train$Unemployed10)
#levels(train$Unemployed)=c("0","1","-1")

train$Unemployed10=as.numeric(train$Unemployed10)
train$Unemployed10[is.na(train$Unemployed10)]=-1
train$Unemployed10=as.character(train$Unemployed10)
train$Unemployed10=as.factor(train$Unemployed10)
levels(train$Unemployed10)=c("0","1","-1")
levels(train$Unemployed10)
str(train)
summary(train)

#lapply(train,function(example) example[is.na(example)]=-1)
train1 <- na.omit(train)
table(train1$Happy)

library(caTools)
set.seed(88)
split=sample.split(train1$Happy,SplitRatio=0.75)
train2=subset(train1,split==TRUE)
test2=subset(train1,split==FALSE)
nrow(train2)
nrow(test2)
#tapply(predictTrain,train1$Happy,mean)

# CART
library(rpart)
library(rpart.plot)
Happy_Tree=rpart(Happy~Var1+WorkStatus+Score+Divorce+Widowed+Education+
                  Residence_Region+babies+preteen+teens+income+
                  Engagement_Religion+Var2+TVhours+Gender+Unemployed10+Alcohol_Consumption,
                  data=train2,method="class",control=rpart.control(minbucket=25))
#summary(Happy_Tree)
prp(Happy_Tree)
predictCART=predict(Happy_Tree,newdata=test2,type="class")
table(test2$Happy,predictCART)
#accuracy = 0.5059524
#(411+75)/(411+75+92+92+223) accuracy = 0.5442329
#(468+44)/(468+44+265+41+95+5) = 0.5577342
#(679+77)/(679+77+134+12+84+389) = 0.5498182
#(107+656+74)/(107+656+74+35+4+22+85+16+376) 0.6087273
## Handling test data
test3<-read.csv("happy_test.csv",stringsAsFactors=FALSE)
table(test3$Unemployed10)
DF1<-sqldf('select a.*,b.Alcohol_Consumption from test3 a join alcohol b on a.ID=b.ID')
test3<-DF1
test3$Var1=as.factor(test3$Var1)
test3$WorkStatus=as.factor(test3$WorkStatus)
test3$Divorce=as.factor(test3$Divorce)
test3$Widowed=as.factor(test3$Widowed)
test3$Residence_Region=as.factor(test3$Residence_Region)
test3$income=as.factor(test3$income)
test3$Engagement_Religion=as.factor(test3$Engagement_Religion)
test3$Var2=as.factor(test3$Var2)
test3$Gender=as.factor(test3$Gender)
#test3$Unemployed10=as.factor(test3$Unemployed10)

test3$Alcohol_Consumption=as.factor(test3$Alcohol_Consumption)

#levels(train$Unemployed)=c("0","1","-1")
str(test3)
test3$Unemployed10=as.numeric(test3$Unemployed10)
test3$Unemployed10[is.na(test3$Unemployed10)]=-1
test3$Unemployed10=as.character(test3$Unemployed10)
test3$Unemployed10=as.factor(test3$Unemployed10)
levels(test3$Unemployed10)=c("0","1","-1")
levels(test3$Unemployed10)

#summary(test3$Unemployed10)
predictCART3=predict(Happy_Tree,newdata=test3,type="class")
test3$Happy=predictCART3
table(test3$Happy,predictCART3)
nrow(test3)
table(test3$Happy)

# Random Forest
library(randomForest)
train2$Happy=as.factor(train2$Happy)

Random_Tree=randomForest(as.factor(Happy)~Var1+WorkStatus+Score+Divorce+Widowed+Education+
                           Residence_Region+babies+preteen+teens+income+
                           Engagement_Religion+Var2+TVhours+Gender+Unemployed10+Alcohol_Consumption,
                         data=train2,nodesize=25,ntree=200,na.action = na.roughfix,type="prob")
summary(Random_Tree)
#rf_fit<-randomForest(Project_Valuation~.,data=corr_final_DF[train,],ntree=50)
#rf_predictions<-predict(rf_fit,newdata=corr_final_DF[test,])

PredictForest=predict(Random_Tree,newdata=test2)
#PredictForest<-predict(rf_fit,newdata=corr_final_DF[test,])
table(test2$Happy,PredictForest)
#Accuracy 0.6094545
#(106+677+58)/(106+677+58+34+6+22+64+16+392)
test3=test3[1:18]
str(test3)
lapply(test3,is.factor)
levels(test3$Var1)=levels(train2$Var1)
levels(test3$WorkStatus)=levels(train2$WorkStatus)
levels(test3$Divorce)=levels(train2$Divorce)
levels(test3$Widowed)=levels(train2$Widowed)
levels(test3$Residence_Region)=levels(train2$Residence_Region)
levels(test3$income)=levels(train2$income)
levels(test3$Engagement_Religion)=levels(train2$Engagement_Religion)
levels(test3$var2)=levels(train2$var2)
levels(test3$Gender)=levels(train2$Gender)
levels(test3$Unemployed10)=levels(train2$Unemployed10)
levels(test3$Alcohol_Consumption)=levels(train2$Alcohol_Consumption)
#lapply(train2,levels)
summary(test3)
predictRF=predict(Random_Tree,newdata=test3)
head(predictRF)
summary(predictRF)
#attributes(PredictForest)
test3$Happy=predictRF
#table(test3$Happy,predictRF)
levels(test3$Happy)=levels(train2$Happy)

## SVM
library(e1071)
test3$Happy=NULL
svm_fit<-svm(Happy~Var1+WorkStatus+Score+Divorce+Widowed+Education+
               Residence_Region+babies+preteen+teens+income+
               Engagement_Religion+Var2+TVhours+Gender+Unemployed10+Alcohol_Consumption,
             data=train2,na.action=na.omit)
svm_predictions<-predict(svm_fit,newdata=test3)
#error<-sqrt((sum((corr_final_DF[-train,]$shares-svm_predictions)^2))/nrow(corr_final_DF[-train]))
#error

#test1=corr_final_DF_test
#svm_predictions<-predict(svm_fit,newdata=test3)
test3$Happy=svm_predictions


submit<-data.frame(ID=test3$ID,Happy=test3$Happy)
write.csv(submit,file="happy_submit1.csv",row.names=FALSE)

