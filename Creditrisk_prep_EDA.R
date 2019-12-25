library(tidyverse)
library(ggthemes)
library(corrplot)
library(GGally)
library(DT)
library(caret)
library(dplyr)

# Set the blank spaces to NA's
setwd("D:/IMC/Project")
getwd()
loan = read_csv("loan.csv" , na = "")
colnames(loan)
nrow(loan)


j = c()
s = c()
for(i in seq(from = 1 , to = 145 , by = 1)){
  j[i] = names(loan[i])
  s[i] = formatC(sum(as.numeric(is.na(loan[i])))/nrow(loan)*100, digits = 2, format = "f")
}
percentage_of_na = data.frame(j,s) ; percentage_of_na

percentage_of_na$s = as.numeric(trimws(percentage_of_na$s))
str(percentage_of_na)

percentage_of_na
missinglessthan30percent = filter(percentage_of_na, s < 30)
missinglessthan30percent

str(missinglessthan30percent)
thiscolumn = data.frame(as.character(missinglessthan30percent$j))
thiscolumn

new_loan30 = loan[,FALSE]
for(i in seq(from = 1  , to = 87 , by = 1)){
  a <- loan[,which(colnames(loan)== thiscolumn[i,1] )]
  new_loan30 = cbind(new_loan30,a)
}
head(new_loan30)

# loan_outcome -> 1 if loan_status = ‘Charged Off’ or ‘Default’ loan_outcome -> 0 if loan_status = ‘Fully Paid’
new_loan30_2 = new_loan30 %>%
  mutate(loan_outcome = ifelse(loan_status %in% c('Charged Off' ,'Default'), 1, 
                               ifelse(loan_status == 'Fully Paid' , 0, 'No info')
  ))

# Create the new dataset by filtering 0's and 1's in the loan_outcome column and remove loan_status column for the modelling (So we focus only on loan stutus which contain 'Charge off', 'Default' and 'Fully Paid')
loan2 = new_loan30_2  %>%  filter(loan_outcome %in% c(0 , 1))
nrow(loan2)

table(unlist(lapply(loan2, class)))
'''
loan4 = loan2 %>%
  select(loan_outcome , loan_amnt , int_rate , grade , emp_length , home_ownership , annual_inc , term, debt_settlement_flag, sub_grade, verification_status,purpose, total_pymnt, dti, emp_title, tot_cur_bal,avg_cur_bal)'''

loanSelect = loan2 %>%
  select(loan_outcome , loan_amnt , int_rate , grade , emp_length , home_ownership , annual_inc , term, sub_grade, purpose, total_pymnt, dti, emp_title, tot_cur_bal, avg_cur_bal)


# home_ownership prep

table(loanSelect$home_ownership)
ggplot(loanSelect , aes(x = home_ownership , y = ..count.. , fill = factor(loan_outcome , c(1 , 0) , c('Default or Charged off' , 'Fully Paid')))) + 
  geom_bar() + 
  theme(legend.title = element_blank())

loanSelect1 = loanSelect %>%
  mutate(home_ownership2 = ifelse(home_ownership %in% c('ANY' ,'NONE', 'OTHER', 'RENT','MORTGAGE'), 'Not OWN',ifelse(home_ownership == 'OWN' , 'OWN', 'No info')
  ))
table(loanSelect1$home_ownership2)
ggplot(loanSelect1 , aes(x = home_ownership2 , y = ..count.. , fill = factor(loan_outcome , c(1 , 0) , c('Default or Charged off' , 'Fully Paid')))) + 
  geom_bar() + 
  theme(legend.title = element_blank())


# purpose prep
ggplot(loanSelect1 , aes(x = purpose , y = ..count.. , fill = factor(loan_outcome , c(1 , 0) , c('Default or Charged off' , 'Fully Paid')))) + 
  geom_bar() + 
  theme(legend.title = element_blank())

table(loanSelect1$purpose,loanSelect1$loan_outcome)

loanSelect2 = loanSelect1 %>%
  mutate(purpose2 = ifelse(purpose %in% c('debt_consolidation' ,'credit_card'), 'debt_con', 'other')
  )
ggplot(loanSelect2 , aes(x = purpose2 , y = ..count.. , fill = factor(loan_outcome , c(1 , 0) , c('Default or Charged off' , 'Fully Paid')))) + 
  geom_bar() + 
  theme(legend.title = element_blank())

table(loanSelect2$purpose2,loanSelect2$loan_outcome)


# Screen for NA
j = c()
percent_NA = c()
num_NA = c()
for(i in seq(from = 1 , to = dim(loanSelect2)[2] , by = 1)){
  j[i] = names(loanSelect2[i])
  percent_NA[i] = formatC(sum(as.numeric(is.na(loanSelect2[i])))/nrow(loanSelect2)*100, digits = 4, format = "f")
  num_NA[i] = sum(as.numeric(is.na(loanSelect2[i])))
}
percentage_of_na1 = data.frame(j,percent_NA,num_NA) ; percentage_of_na1
str(loanSelect2)
summary(loanSelect2)

#find outliers
boxplot(loanSelect2$loan_amnt)$out
outliers <- boxplot(loanSelect2$loan_amnt, plot=FALSE)$out
loanSelect3 <- loanSelect2[-which(loanSelect2$loan_amnt %in% outliers),]
boxplot(loanSelect3$loan_amnt)
nrow(loanSelect3)
str(loanSelect3)
summary(loanSelect3$loan_amnt)

boxplot(loanSelect3$annual_inc)$out
outliers <- boxplot(loanSelect3$annual_inc, plot=FALSE)$out
loanSelect4 <- loanSelect3[-which(loanSelect3$annual_inc %in% outliers),]
boxplot(loanSelect4$annual_inc)$out
nrow(loanSelect4)
outliers <- boxplot(loanSelect4$annual_inc, plot=FALSE)$out
loanSelect4 <- loanSelect4[-which(loanSelect4$annual_inc %in% outliers),]
boxplot(loanSelect4$annual_inc)$out
nrow(loanSelect4)
str(loanSelect4)

boxplot(loanSelect4$dti)$out
outliers <- boxplot(loanSelect4$dti, plot=FALSE)$out
loanSelect4 <- loanSelect4[-which(loanSelect4$dti %in% outliers),]
boxplot(loanSelect4$dti)$out
nrow(loanSelect4)
boxplot(loanSelect4$dti)$out
outliers <- boxplot(loanSelect4$dti, plot=FALSE)$out
loanSelect4 <- loanSelect4[-which(loanSelect4$dti %in% outliers),]
boxplot(loanSelect4$dti)$out
nrow(loanSelect4)
str(loanSelect4)

boxplot(loanSelect4$tot_cur_bal)$out
outliers <- boxplot(loanSelect4$tot_cur_bal, plot=FALSE)$out
loanSelect4 <- loanSelect4[-which(loanSelect4$tot_cur_bal %in% outliers),]
boxplot(loanSelect4$tot_cur_bal)$out
nrow(loanSelect4)
outliers <- boxplot(loanSelect4$tot_cur_bal, plot=FALSE)$out
loanSelect4 <- loanSelect4[-which(loanSelect4$tot_cur_bal %in% outliers),]
boxplot(loanSelect4$tot_cur_bal)$out
nrow(loanSelect4)
str(loanSelect4)

boxplot(loanSelect4$avg_cur_bal)$out
outliers <- boxplot(loanSelect4$avg_cur_bal, plot=FALSE)$out
loanSelect4 <- loanSelect4[-which(loanSelect4$avg_cur_bal %in% outliers),]
boxplot(loanSelect4$avg_cur_bal)$out
nrow(loanSelect4)
outliers <- boxplot(loanSelect4$avg_cur_bal, plot=FALSE)$out
loanSelect4 <- loanSelect4[-which(loanSelect4$avg_cur_bal %in% outliers),]
boxplot(loanSelect4$avg_cur_bal)$out
nrow(loanSelect4)
outliers <- boxplot(loanSelect4$avg_cur_bal, plot=FALSE)$out
loanSelect4 <- loanSelect4[-which(loanSelect4$avg_cur_bal %in% outliers),]
boxplot(loanSelect4$avg_cur_bal)$out
nrow(loanSelect4)
str(loanSelect4)


loanSelect5 = loanSelect4 %>% select(-total_pymnt)

str(loanSelect5)
summary(loanSelect5)
# for numeric data
# Quick code to replace missing values with the mean
loanSelect6 <- loanSelect5
loanSelect6[sapply(loanSelect6, is.numeric)] <- data.frame(
  sapply(
    loanSelect5[sapply(loanSelect5, is.numeric)],
    function(x) ifelse(is.na(x),
                       mean(x, na.rm = TRUE),
                       x)))



nrow(loanSelect6)

summary(loanSelect6)
str(loanSelect6)






table(unlist(lapply(loanSelect6, class)))
table(unlist(lapply(loanSelect6, typeof)))

loanSelect6[sapply(loanSelect6, is.character)] <- lapply(loanSelect6[sapply(loanSelect6, is.character)], as.factor)
summary(loanSelect6)
str(loanSelect6)

loanSelect7 = loanSelect6 %>%
  filter(!is.na(emp_title))
summary(loanSelect7)
str(loanSelect7)

loanSelect7 = loanSelect7 %>%
  filter(dti >= 0)

write.csv(loanSelect7, file = "loan_select7.csv")

rm(a,loan,loan2,loanSelect,loanSelect1,loanSelect2,loanSelect3,loanSelect4,loanSelect5)
rm(new_loan30,new_loan30_2,loanSelect6)

data = loanSelect7 %>%
  select(loan_outcome) %>%
  select(home_ownership2) %>%
  select(annual_inc) %>%
  select(term) %>%
  select(loan_amnt)%>%
  select(grade) 
  

data = loanSelect7[,c('loan_outcome','home_ownership2','annual_inc','term','loan_amnt','grade','int_rate',"purpose2",'avg_cur_bal','dti')]
str(data)
summary(data)
# Split the data

set.seed(123)
sample <- sample.int(n = nrow(data),size = floor(0.7*nrow(data)),replace = F)
train <- data[sample,]
test <- data[-sample,]

library(caret)
param = preProcess(train[,-1],method = c("center","scale"))
train_scale <- predict(param,train)
test_scale <- predict(param,test)

# Logistic Regression
library(MASS)
library(e1071)
#train.control <- trainControl(method = "cv",number = 5) #, sampling = "up"
set.seed(123)
glm.fit<-train(loan_outcome ~., data = train_scale, method="glm", family="binomial") #,trControl = train.control
glm.fit
summary(glm.fit)
#¶éÒ¨Ð predict ¢éÍÁÙÅãËÁè¡çãÊè x ãËÁèã¹ prob ¹ÕéáËÅÐ ¨Ð¡Õè row ¡çä´é
prob<-predict(glm.fit, test_scale, type='prob')
y_pred<-as.numeric(prob[, "1"] > 0.5)  #à»ÅÕèÂ¹¤èÒ·Õè Predict ÍÍ¡ÁÒ¨Ò¡ true false à»ç¹ 1,0
predlist = data.frame(y_pred, test$loan_outcome)

library(MLmetrics)
k = 0
accuracy = c()
sensitivity = c()
specificity = c()
F1score = c()
recall = c()
for(i in seq(from = 0.1 , to = 0.6 , by = 0.05)){
  k = k + 1
  preds_binomial = as.numeric(prob[, "1"] > i)
  confmat = table( preds_binomial,test_scale$loan_outcome)
  accuracy[k] = sum(diag(confmat)) / sum(confmat)
  sensitivity[k] = confmat[2 , 2] / (confmat[2,2]+confmat[2,1])
  specificity[k] = confmat[1 , 1] / (confmat[1 , 1]+confmat[2 , 1])
  recall[k] = confmat[2 , 2] / (confmat[2,2]+confmat[1,2])
  F1score[k] = F1_Score(preds_binomial ,test_scale$loan_outcome)
}
#, positive = '1'
threshold = seq(from = 0.1 , to = 0.6 , by = 0.05)

data1 = data.frame(threshold , accuracy , sensitivity , specificity,recall, F1score)
data1

# Gather accuracy , sensitivity and specificity in one column
ggplot(gather(data1 , key = 'Metric' , value = 'Value' , 2:6) , 
       aes(x = threshold , y = Value , color = Metric)) + 
  geom_line(size = 1.5)



y_pred<-as.numeric(prob[, "1"] > 0.2)

conf_mat<-table(y_pred, ytrue = test$loan_outcome); conf_mat



library(pROC)
# Area Under Curve
auc(roc(test$loan_outcome , y_pred))
# Plot ROC curve
plot.roc(test$loan_outcome , y_pred , main = "Confidence interval of a threshold" , percent = TRUE , ci = TRUE , of = "thresholds" , thresholds = "best" , print.thres = "best" , col = 'blue')
zx