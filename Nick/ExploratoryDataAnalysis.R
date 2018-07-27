#Exploratory dat analysis for home credit default risk
application_train <- read.csv("C:/Users/Nick Burnett/Desktop/HomeCredit/application_train.csv", header=TRUE)
bureau <- read.csv("C:/Users/Nick Burnett/Desktop/HomeCredit/bureau.csv")
#installments_payments <- read.csv("C:/Users/Nick Burnett/Desktop/HomeCredit/installments_payments.csv")
#previous_application <- read.csv("C:/Users/Nick Burnett/Desktop/HomeCredit/previous_application.csv")
#POS_CASH_balance <- read.csv("C:/Users/Nick Burnett/Desktop/HomeCredit/POS_CASH_balance.csv")
HomeCredit_columns_description <- read.csv("C:/Users/Nick Burnett/Desktop/HomeCredit/HomeCredit_columns_description.csv")
credit_card_balance <- read.csv("C:/Users/Nick Burnett/Desktop/HomeCredit/credit_card_balance.csv")




   #Match loan ID number between tables
   matchedIndex = match(application_train$SK_ID_CURR,credit_card_balance$SK_ID_CURR)  
   
   
   withKids = as.numeric(application_train$CNT_CHILDREN>=4)
   ageDays = as.numeric(abs(application_train$DAYS_BIRTH) < 11000)
   Home = as.numeric(application_train$NAME_HOUSING_TYPE=="Rented apartment")
   Edu = as.numeric(application_train$NAME_EDUCATION_TYPE=="Higher education")
   Job = as.numeric(application_train$OCCUPATION_TYPE=="Low-skill Laborers")
   Income = as.numeric(application_train$AMT_INCOME_TOTAL<=50000)
   Working = as.numeric(application_train$NAME_INCOME_TYPE=="Working")
   credBalance = as.numeric(is.na(credit_card_balance$AMT_BALANCE[matchedIndex]))
   credLim = as.numeric(credit_card_balance$AMT_CREDIT_LIMIT_ACTUAL[matchedIndex]>=200000)
   
   #pairwise EDA
   df = data.frame(application_train$TARGET,application_train$AMT_INCOME_TOTAL,application_train$AMT_ANNUITY,application_train$AMT_GOODS_PRICE,withKids,ageDays,Home,Edu,Job,Income,Working,credBalance,credLim)
   #df = subset(df,df[,2]< 1.17e+08)
   
   #pairs(df)
   tapply(df[,1],as.factor(df$withKids),function(x){sum(x)/NROW(x)})
    #8.1% of people with fewer than 6 kids defaulted on their loans, 21.4% of people with 6 or more kids defaulted on their loans
   
   tapply(df[,1],as.factor(df$credLim),function(x){sum(x)/NROW(x)})
   
   
   tapply(df[,1],as.factor(paste(df$Job,df$credLim)),function(x){sum(x)/NROW(x)})
   
   

#   setwd("C:/Users/Nick Burnett/Desktop/HomeCredit")
#   write.csv(df,"CompiledData.csv")   
   
   sjp.corr(df)
