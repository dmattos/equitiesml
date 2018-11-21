library(quantmod)
library(ggplot2)
library(randomForest)
library(e1071)
library(xgboost)
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
source("../lib/Functions_v4.r")

# Database
filepath="../database/DataMaster_v9.xlsm"

filing_dates = announcementDates(filepath,sheet="ANNOUNCEMENT_DT",startRow=2)
stocks = rawFactors(filepath=filepath,startRow = 2,filing_dates=filing_dates)
stocks_A = modFactors(stocks,0.3,-0.3,short = FALSE)

set.seed(1110)

# Walk Forward Analysis #

start_time <- Sys.time()

walkForward(stocks_A[["ABEV3"]],train_len = 19*40,
            test_len = 19*10,starting_date = "2006-01-01")

end_time <- Sys.time()
end_time - start_time


