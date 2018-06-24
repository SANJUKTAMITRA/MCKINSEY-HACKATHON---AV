#install and load the required libraries
install.packages("ranndomForest")

library(randomForest)
library(ggplot2)
library(car)
library(MASS)
library(caret)
library(ROCR)
#load the input files 
stroketrain <- read.csv("train_ajEneEa.csv",stringsAsFactors = FALSE)
stroketest <- read.csv("test_v2akXPA.csv",stringsAsFactors = FALSE)

dim(stroketrain)
dim(stroketest)
head(stroketest)
head(stroketrain)
str(stroketrain)
str(stroketest)
#CHECK FOR DATA ANAMOLIES
sum(is.na(stroketrain))
sum(is.na(stroketest))

sum(duplicated(stroketrain))
sum(duplicated(stroketest))

#no duplicate found but missing value found

sapply(stroketrain[,c(1:12)],function(x) sum(is.na(x)))
sapply(stroketest[,c(1:11)],function(x) sum(is.na(x)))
#convert char to factors
str(stroketrain)

stroketrain$gender <- as.factor(stroketrain$gender)
stroketrain$ever_married <- as.factor(stroketrain$ever_married)

stroketrain$work_type <- as.factor(stroketrain$work_type)
stroketrain$Residence_type <- as.factor(stroketrain$Residence_type)
stroketrain$smoking_status <- as.factor(stroketrain$smoking_status)

stroketest$gender <- as.factor(stroketest$gender)
stroketest$ever_married <- as.factor(stroketest$ever_married)

stroketest$work_type <- as.factor(stroketest$work_type)
stroketest$Residence_type <- as.factor(stroketest$Residence_type)
stroketest$smoking_status <- as.factor(stroketest$smoking_status)

#in both the cases bmi is missing
#lets us create decision trees to predict the missing value of bmi`

bmitrain <- na.omit(stroketrain)
bmitest <- na.omit(stroketest)

#copy the missing bmi records in one file
bmitrain1 <- stroketrain[which(is.na(stroketrain$bmi)),]
bmitest1 <- stroketest[which(is.na(stroketest$bmi)),]


rm(bmim1)
library(rpart.plot)
library(rpart)
bmim1 <- rpart(bmi~.-id,data = bmitrain)
#bmim1 <- randomForest(bmi~.-id,data = bmitrain[c(1:10000),],proximity=FALSE,
#                      ntree=200, mtry=6, do.trace=2, na.action=na.omit)


prp(bmim1)

rm(bmim2)
bmim2 <- rpart(bmi~.-id,data = bmitest)
#bmim2 <- randomForest(bmi~.-id,data = bmitest,proximity=FALSE,
#                      ntree=200, mtry=6, do.trace=2, na.action=na.omit)


prp(bmim2)
#prediction of bmi for train data set

bmipred1 <- predict(bmim1,bmitrain1)
bmipred2 <- predict(bmim2,bmitest1)

bmitrain1$bmipred <- round(bmipred1,1)
bmitest1$bmipred <- round(bmipred2,1)
#impute the missing bmi value in stroketest and train
stroketrain[which(is.na(stroketrain$bmi)),10] <- bmipred1
stroketest[which(is.na(stroketest$bmi)),10] <- bmipred2

#some of the patient smoking status is unknown
#convert to chr first
stroketrain$smoking_status <- as.character(stroketrain$smoking_status)
stroketrain[which(stroketrain$smoking_status==""),11] <- "Unknown"
stroketrain$smoking_status <- as.factor(stroketrain$smoking_status)
stroketest$smoking_status <- as.character(stroketest$smoking_status)
stroketest[which(stroketest$smoking_status==""),11] <- "Unknown"
stroketest$smoking_status <- as.factor(stroketest$smoking_status)

#convert character variable to numeric
str(stroketrain)
levels(stroketrain$gender)

#create a dummy variable for stroketrain

dummy1 <- data.frame(model.matrix(~gender,data = stroketrain))
#remove the first col
colnames(dummy1)[1] <- "Female"
dummy2 <- data.frame(model.matrix(~work_type,data = stroketrain))
colnames(dummy2)[1] <- "children"
dummy3 <- data.frame(model.matrix(~smoking_status,data = stroketrain))
colnames(dummy3)[1] <- "formerly smoked"

levels(stroketrain$ever_married)
#change No to 0 and Yes to 1
levels(stroketrain$ever_married) <- c(0,1)
levels(stroketrain$Residence_type)
#change rural to 0 and urban to 1
levels(stroketrain$Residence_type) <- c(0,1)

#create a dummy variable for stroketest

dummy4 <- data.frame(model.matrix(~gender,data = stroketest))
#remove the first col
colnames(dummy4)[1] <- "Female"
dummy5 <- data.frame(model.matrix(~work_type,data = stroketest))
colnames(dummy5)[1] <- "children"
dummy6 <- data.frame(model.matrix(~smoking_status,data = stroketest))
colnames(dummy6)[1] <- "formerly smoked"

levels(stroketest$ever_married)
#change No to 0 and Yes to 1
levels(stroketest$ever_married) <- c(0,1)
levels(stroketest$Residence_type)
#change rural to 0 and urban to 1
levels(stroketest$Residence_type) <- c(0,1)

#create new dataframe to be used for modeelling
stroketrain1 <- cbind(stroketrain[,c(1,3:6,8:10,12)],dummy1,dummy2,dummy3)
stroketest1 <- cbind(stroketest[,c(1,3:6,8:10)],dummy4,dummy5,dummy6)

#DATA VISULIZATION
ggplot(stroketrain,aes(x=factor(stroke),fill=smoking_status)) + geom_bar(position = "dodge")
#among people who never had stroke are the people who either dont smoke or unknown category
ggplot(stroketrain,aes(x=factor(stroke),fill=smoking_status)) + geom_bar(position = "dodge") + ylim(0,150)
#people who smoke has a very high chances of stroke,says the plot
ggplot(stroketrain,aes(x=factor(stroke),fill=Residence_type)) + geom_bar(position = "dodge")
#residence type do not have much impact on stroke
ggplot(stroketrain,aes(x=factor(stroke),fill=work_type)) + geom_bar(position = "dodge")
#among people who never had stroke has higher probability of having work_type as private or self-employed
#this might be beacuse private and self employed work type are having more salary and less tension

ggplot(stroketrain,aes(x=factor(stroke),fill=work_type)) + geom_bar(position = "dodge") + ylim(0,100)
#govt job people have maximum chances of having stroke and children has least probability of having stroke
ggplot(stroketrain,aes(x=factor(stroke),y=heart_disease)) + stat_summary(fun="mean",geom="bar")
#so on a average level people who have heart disease has very higher chances of having stroke

ggplot(stroketrain,aes(x=factor(stroke),fill=ever_married)) + geom_bar()
ggplot(stroketrain,aes(x=factor(stroke),y=ever_married)) + stat_summary(fun="mean",geom="bar")
#maritial status do not have any impact on stroke
ggplot(stroketrain,aes(x=factor(stroke),y=hypertension)) + stat_summary(fun="mean",geom="bar")
#people having hypertension has very higher chances of having stroke
ggplot(stroketrain,aes(x=factor(stroke),y=bmi)) + stat_summary(fun="mean",geom="col",fill="maroon")
ggplot(stroketrain,aes(x=factor(stroke),y=avg_glucose_level)) + stat_summary(fun="mean",geom="col",fill="green")
#while there is not much difference in the bmi value of people not having stroke and having stoke;
#average glucose level is 110 for people not having stoke and 130 for people had stoke atleast once

#MODEL BUILDING
#before model building check the number of records with stroke as 0 and 1
length(which(stroketrain$stroke==0))
#42617
length(which(stroketrain$stroke==1))
#783
#lets take a 2000 records with stroke as 0
set.seed(123)
a <- sample(nrow(stroketrain1[which(stroketrain$stroke==0),]),0.05*nrow(stroketrain1[which(stroketrain$stroke==0),]))
stroketrain2 <- stroketrain1[a,]
stroketrain2 <- rbind(stroketrain2,stroketrain1[which(stroketrain$stroke==1),])
stroketrain2 <- stroketrain2[sample(nrow(stroketrain2)),]
set.seed(1234)
a1 <- sample(nrow(stroketrain2),0.7*nrow(stroketrain2))
train <- stroketrain2[a1,]
test <- stroketrain2[-a1,]

#logistic regression
model1 <- glm(stroke~.-id,data = train,family = "binomial")
summary(model1)

#stepaic
model2 <- stepAIC(model1,direction = "both")
#remove work_typePrivate,work_typeGovt_job,genderMale,ever_married ,,work_typeNever_worked,bmi
model3 <- glm(formula = stroke ~ work_typeSelf.employed + smoking_statusnever.smoked+Residence_type +
              smoking_statusUnknown+smoking_statussmokes+ hypertension+avg_glucose_level + heart_disease+ age,
              data = train,family = "binomial")
summary(model3)
vif(model3)
#all the variables have low vif
#based on p value remove work_typeSelf.employed 

model4 <- glm(formula = stroke ~  smoking_statusnever.smoked+Residence_type +
                smoking_statusUnknown+smoking_statussmokes+ hypertension+avg_glucose_level + heart_disease+ age,
              data = train,family = "binomial")
summary(model4)
vif(model4)
#all the variables have low vif
#based on p value remove smoking_statusnever.smoked 

model5 <- glm(formula = stroke ~  Residence_type +
                smoking_statusUnknown+smoking_statussmokes+ hypertension+avg_glucose_level + heart_disease+ age,
              data = train,family = "binomial")
summary(model5)
vif(model5)
#smoking_statusUnknown
model6 <- glm(formula = stroke ~  Residence_type +smoking_statussmokes+ hypertension+avg_glucose_level + heart_disease+ age,
              data = train,family = "binomial")
summary(model6)
vif(model6)
#smoking_statussmokes
model7 <- glm(formula = stroke ~  Residence_type + hypertension+avg_glucose_level + heart_disease+ age,
              data = train,family = "binomial")
summary(model7)
vif(model7)

#finalmodel; all variables have p value less than 0.05
#Coefficients:
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -5.863428   0.298902 -19.617  < 2e-16 ***
#  Residence_type1    0.260535   0.121171   2.150   0.0315 *  
##  hypertension       0.368147   0.152648   2.412   0.0159 *  
#  avg_glucose_level  0.004619   0.001120   4.125 3.71e-05 ***
#  heart_disease      0.860766   0.183774   4.684 2.82e-06 ***
#  age                0.070852   0.004177  16.961  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 2406.8  on 2038  degrees of freedom
#Residual deviance: 1684.7  on 2033  degrees of freedom
#AIC: 1696.7

#final model shows the stroke depends on residence type urban,hypertension,avg glocose level,
# and age

final_model <- model7
##============ Model Evaluation - Test Data ========

## predicted probabilities of stroke for test data
test_pred <- predict(final_model,type = "response",newdata = test[,-1])

#lets check summary 
summary(test_pred)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.004259 0.044235 0.195381 0.279015 0.494526 0.890260

test$probability <- test_pred
View(test)

## Use the probability cutoff of 50%.
test_pred_stroke <- factor(ifelse(test$probability  >= 0.50, "Yes", "No"))
test_actual_stroke <- factor(ifelse(test$stroke==1,"Yes","No"))

table(test_pred_stroke,test_actual_stroke)

#test_actual_stroke
#test_pred_stroke  No Yes
#No  543 116
#Yes  71 144


## Use the probability cutoff of 60%.
test_pred_stroke1 <- factor(ifelse(test$probability  >= 0.60, "Yes", "No"))
test_actual_stroke1 <- factor(ifelse(test$stroke==1,"Yes","No"))
table(test_pred_stroke1,test_actual_stroke1)

#              test_actual_stroke1
#test_pred_stroke1  No Yes
#No  572 161
#Yes  42  99
##============ Choose the cutoff value ======== 

## Find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_stroke <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_stroke,test_actual_stroke, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}
## to check the above function for cutoff value from 30% to highest value of probability 0.8959000 i.e 89.5%
s = seq(0.1,0.91,length=100)
#creating a output matrix of size 70
OUT1 <- matrix(0,100,3)


for(i in 1:99){
  OUT1[i,] <- perform_fn(s[i])
}
OUT1
#putting cut off value,sensitivity,Specificity and Accuracy in a single dataframe
#plotting them to check the cut off value 
OUT <- data.frame(s,OUT1[,1],OUT1[,2],OUT1[,3])
colnames(OUT)[1] <- "cutoff"
colnames(OUT)[2] <- "sensitivity"
colnames(OUT)[3] <- "specificity"
colnames(OUT)[4] <- "accuracy"
plot(OUT$cutoff,OUT$sensitivity,xlab = "CUTOFF",cex.axis=1.5,lwd=2,type = "l",col="red")
lines(OUT$cutoff,OUT$specificity,col="green",cex.axis=1.5,lwd=2)
lines(OUT$cutoff,OUT$accuracy,col="pink",cex.axis=1.5,lwd=2)
legend(0.7,0.2,col=c("red","green","pink"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
locator()
cutoff=0.3104
#locator shows the cut off value in the plot is at x = 0.31 i.e probaibility 0.31
# in this case we want to choose the people having stroke so sensitivity must be very high
# hence a low cut off value like 0.31 will do

test_cutoff_stroke <- factor(ifelse(test$probability >= cutoff, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_stroke,test_actual_stroke,positive = "Yes")

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc
sens
spec
#accuracy as 78.2%
#sensitivity as 78.08%
#Specificity as 78.3%



##============ XXXXKS -statistic - Test Data ######

test_cutoff_stroke <- ifelse(test_cutoff_stroke=="Yes",1,0)
test_actual_stroke <- ifelse(test_actual_stroke=="Yes",1,0)

#library(ROCR)
#library(caret)
#on testing  data
str(test_cutoff_stroke)
pred_object_test<- prediction(test_cutoff_stroke,test_actual_stroke)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
#KS STATISTICS VALUE IS 56% which shows it is a good model

##============ XXXXLift & Gain Chart ======

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

stroke_decile = lift(test_cutoff_stroke,test_actual_stroke, groups = 10)


#at 3rd decile lift is 2% 
#########################################################################
#repeat the above process with cut off value less than 0.31
locator()

x=0.233

cutoff1=0.233
#locator shows the cut off value in the plot is at x = 0.31 i.e probaibility 0.31
# in this case we want to choose the people having stroke so sensitivity must be very high
# hence a low cut off value like 0.31 will do

test_cutoff_stroke1 <- factor(ifelse(test$probability >= cutoff1, "Yes", "No"))
test_actual_stroke <- factor(ifelse(test$stroke ==1, "Yes", "No"))
conf_final1 <- confusionMatrix(test_cutoff_stroke1,test_actual_stroke,positive = "Yes")

acc1 <- conf_final$overall[1]
sens1 <- conf_final$byClass[1]
spec1 <- conf_final$byClass[2]

acc
sens
spec
#accuracy as 78.2%
#sensitivity as 78.08%
#Specificity as 78.3%



##============ XXXXKS -statistic - Test Data ######

test_cutoff_stroke1 <- ifelse(test_cutoff_stroke1=="Yes",1,0)
test_actual_stroke <- ifelse(test_actual_stroke=="Yes",1,0)

#library(ROCR)
#library(caret)
#on testing  data
str(test_cutoff_stroke1)
pred_object_test<- prediction(test_cutoff_stroke1,test_actual_stroke)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
#KS STATISTICS VALUE IS 57% which shows it is a good model

##============ XXXXLift & Gain Chart ======

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

stroke_decile1 = lift(test_cutoff_stroke1,test_actual_stroke, groups = 10)


#at 3rd decile lift is 2% 
#repeat the above process with cut off value less than 0.31

#predict stroke for test data 

predictstroke1 <- predict(final_model,type = "response",newdata = stroketest[,-1])

#predict stroke with cutoff value 0.3104
stroketest$predictstroke1 <- predictstroke1
stroketest$stroke_03104 <- factor(ifelse(stroketest$predictstroke1 >= 0.3104,1,0))
getwd()
write.csv(stroketest[,c(1,12,13)],"stroketest3104.csv")

#predict stroke with cutoff value 0.233
#predictstroke1 <- predict(final_model,type = "response",newdata = stroketest[,-1])
#stroketest$predictstroke1 <- predictstroke1
stroketest$stroke_0233 <- factor(ifelse(stroketest$predictstroke1 >= 0.233,1,0))
getwd()
write.csv(stroketest[,c(1,12,14)],"stroketest233.csv")

#cutoff value 0.28
test_cutoff_stroke3 <- factor(ifelse(test$probability >= 0.28, "Yes", "No"))
test_actual_stroke <- factor(ifelse(test$stroke ==1, "Yes", "No"))
conf_final3 <- confusionMatrix(test_cutoff_stroke3,test_actual_stroke,positive = "Yes")

acc3 <- conf_final3$overall[1]
sens3 <- conf_final3$byClass[1]
spec3 <- conf_final3$byClass[2]

acc
sens
spec
#accuracy as 77%
#sensitivity as 81.15%
#Specificity as 75%

stroketest$stroke_028 <- factor(ifelse(stroketest$predictstroke1 >= 0.28,1,0))
getwd()
write.csv(stroketest[,c(1,12,15)],"stroketest28.csv")

stroketest$stroke_016 <- factor(ifelse(stroketest$predictstroke1 >= 0.222,1,0))
getwd()
write.csv(stroketest[,c(1,12,16)],"stroketest22.csv")
