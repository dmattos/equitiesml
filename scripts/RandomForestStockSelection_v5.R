library(quantmod)
library(ggplot2)
library(randomForest)
library(e1071)
library(xgboost)
library(Matrix)
library(glmnet)
library(openxlsx)
library(stringr)
library(xts)
library(zoo)
library(plyr)
library(Hmisc)
library(caret)
library(PerformanceAnalytics)
library(TTR)

# Working directory
setwd("C:/Users/Daniel Mattos/Documents/FGV/Tese/Program/R/scripts")

# Library
source("../lib/Functions_v6.r")

# Database
filepath="../database/DataMaster_v11.xlsm"

filing_dates = announcementDates(filepath,sheet="ANNOUNCEMENT_DT",startRow=2)
stocks = rawFactors(filepath=filepath,startRow = 2,filing_dates=filing_dates)
stocks_A = modFactors(stocks)
tt = splitTrainTest(stocks_A[[13]],500,100,"2004-01-01")

res=testRegression(tt[[1]][[1]],tt[[2]][[1]],upper=0.1,lower=-0.2)
for(i in 2:length(tt[[2]])){
  pred=testRegression(tt[[1]][[i]],tt[[2]][[i]],upper=0.1,lower=-0.2)
  res = rbind(res,pred)
  
}
res[,"Cum_Ret_Buy_Hold"]=cumsum(res[,"Daily_Ret"])
res[,"Cum_Ret_Linreg"]=cumsum(res[,"Linreg_Ret"])
plot.xts(res[,c("Cum_Ret_Buy_Hold","Cum_Ret_Linreg")])

set.seed(10)
res=testLasso(tt[[1]][[1]],tt[[2]][[1]],upper=0.1,lower=-0.2)
for(i in 2:length(tt[[2]])){
  pred=testLasso(tt[[1]][[i]],tt[[2]][[i]],upper=0.1,lower=-0.2)
  res = rbind(res,pred)
  
}
res[,"Cum_Ret_Buy_Hold"]=cumsum(res[,"Daily_Ret"])
res[,"Cum_Ret_Lasso"]=cumsum(res[,"Lasso_Ret"])
plot.xts(res[,c("Cum_Ret_Buy_Hold","Cum_Ret_Lasso")])

set.seed(10)
res=testRandomForest(tt[[1]][[1]],tt[[2]][[1]],upper=0.1,lower=-0.2)
for(i in 2:length(tt[[2]])){
  pred=testRandomForest(tt[[1]][[i]],tt[[2]][[i]],upper=0.1,lower=-0.2)
  res = rbind(res,pred)
  
}
res[,"Cum_Ret_Buy_Hold"]=cumsum(res[,"Daily_Ret"])
res[,"Cum_Ret_Rf"]=cumsum(res[,"Rf_Ret"])
plot.xts(res[,c("Cum_Ret_Buy_Hold","Cum_Ret_Rf")])

set.seed(10)

res=testXGBoost(tt[[1]][[1]],tt[[2]][[1]],upper=0.1,lower=-0.2)
for(i in 2:length(tt[[2]])){
  pred=testXGBoost(tt[[1]][[i]],tt[[2]][[i]],upper=0.1,lower=-0.2)
  res = rbind(res,pred)
  
}
res[,"Cum_Ret_Buy_Hold"]=cumsum(res[,"Daily_Ret"])
res[,"Cum_Ret_Xgb"]=cumsum(res[,"Xgb_Ret"])
plot.xts(res[,c("Cum_Ret_Buy_Hold","Cum_Ret_Xgb")])


# Walk Forward Analysis #

start_time <- Sys.time()

walkForward(stocks_A[["ABEV3"]],train_len = 19*40,
            test_len = 19*10,starting_date = "2006-01-01")

end_time <- Sys.time()
end_time - start_time


