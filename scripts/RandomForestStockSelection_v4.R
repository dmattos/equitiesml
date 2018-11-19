library(quantmod)
library(ggplot2)
library(randomForest)
library(openxlsx)
library(stringr)
library(xts)
library(zoo)
library(plyr)
library(Hmisc)
library(caret)
library(PerformanceAnalytics)

# Working directory
setwd("C:/Users/User/Documents/Trabalho_Final_Met_Quant/R/scripts")

# Library
source("../lib/Functions_v4.r")

# Database
filepath="../database/DataMaster_v9.xlsm"

filing_dates = announcementDates(filepath,sheet="ANNOUNCEMENT_DT",startRow=2)
stocks = rawFactors(filepath=filepath,startRow = 2,filing_dates=filing_dates)
stocks_A = modFactors(stocks,0.4,-0.1,short = FALSE)

set.seed(111)

# Walk Forward Analysis #

walkForward(stocks_A[["ABEV3"]],train_len = 19*40,
            test_len = 19*10,starting_date = "2006-01-01")

walkForward(stocks_A[["BRFS3"]],train_len = 19*40,
            test_len = 19*10,starting_date = "2006-01-01")

walkForward(stocks_A[["CCRO3"]],train_len = 19*40,
            test_len = 19*10,starting_date = "2006-01-01")

walkForward(stocks_A[["CSAN3"]],train_len = 19*40,
            test_len = 19*10,starting_date = "2006-01-01")

walkForward(stocks_A[["CSNA3"]],train_len = 19*40,
            test_len = 19*10,starting_date = "2006-01-01")

walkForward(stocks_A[["FIBR3"]],train_len = 19*40,
            test_len = 19*10,starting_date = "2006-01-01")

walkForward(stocks_A[["EMBR3"]],train_len = 19*40,
            test_len = 19*10,starting_date = "2006-01-01")

walkForward(stocks_A[["EQTL3"]],train_len = 19*40,
            test_len = 19*10,starting_date = "2006-01-01")

walkForward(stocks_A[["GGBR4"]],train_len = 19*40,
            test_len = 19*10,starting_date = "2006-01-01")

walkForward(stocks_A[["HYPE3"]],train_len = 19*40,
            test_len = 19*10,starting_date = "2006-01-01")

walkForward(stocks_A[["JBSS3"]],train_len = 19*40,
            test_len = 19*10,starting_date = "2006-01-01")

walkForward(stocks_A[["LREN3"]],train_len = 19*40,
            test_len = 19*10,starting_date = "2006-01-01")

walkForward(stocks_A[["RADL3"]],train_len = 19*40,
            test_len = 19*10,starting_date = "2006-01-01")

walkForward(stocks_A[["VALE3"]],train_len = 19*40,
            test_len = 19*10,starting_date = "2006-01-01")





