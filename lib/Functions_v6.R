cleanSymbolClosePrice = function(Symbol){
  newSeries = na.omit(na.locf(Ad(Symbol)))
  colnames(newSeries)="AdjClose"
  return(newSeries)
}

drawdown <- function(pnl) {
  cum.pnl  <- c(0, cumsum(pnl))
  drawdown <- cum.pnl - cummax(cum.pnl)
  return(tail(drawdown, -1))
}

maxdrawdown <- function(pnl)min(drawdown(pnl))

returnOverPeriod = function(Symbol,period){
  return(log(Symbol)-log(Lag(Symbol,period)))
}

announcementDates = function(filepath,sheet="ANNOUNCEMENT_DT",startRow=2){
  dates = list()
  data=read.xlsx(filepath,sheet=sheet,startRow = startRow,
                 detectDates = TRUE,skipEmptyCols=FALSE)
  k=1
  for(i in seq(from=3,to=ncol(data),by=2)){
    ticker = strsplit(colnames(data)[i], '[.]')[[1]][1]
    clean_data = na.omit(data[,i:(i+1)])
    dates[[k]]=data.frame(as.Date(as.character(clean_data[,2]),"%Y%m%d"),
                          row.names = as.Date(clean_data[,1]))
    colnames(dates[[k]])="Date"
    names(dates)[k]=ticker
    k=k+1
  }
  return(dates)
}

rawFactors = function(filepath,startRow=2,filing_dates){
  # Get historical prices first
  stocks = list()
  sheet = "PX_LAST"
  data=read.xlsx(filepath,sheet=sheet,startRow = startRow,
                 detectDates = TRUE,skipEmptyCols=FALSE)
  k=1
  for(i in seq(from=3,to=ncol(data),by=2)){
    ticker = strsplit(colnames(data)[i], '[.]')[[1]][1]
    clean_data = na.omit(data[,i:(i+1)])
    stocks[[k]]=data.frame(clean_data[,1:2])
    colnames(stocks[[k]])=c("Date",sheet)
    names(stocks)[k]=ticker
    k=k+1
  }
  sheets = c('CUR_MKT_CAP',
             'PX_LAST',
             'ANNOUNCEMENT_DT',
             'NET_DEBT',
             'TRAIL_12M_EBITDA', #MODIFY
             'MINORITY_INTEREST', #MODIFY
             'RETURN_ON_INV_CAPITAL', #USE
             'PE_RATIO', #USE INVERSE
             'BOOK_VAL_PER_SH', #MODIFY
             'EQY_DVD_YLD_12M', #USE
             'TRAIL_12M_FREE_CASH_FLOW_PER_SH', #MODIFY
             'TRAIL_12M_SALES_PER_SH',
             'TRAIL_12M_OPER_INC', #MODIFY
             'TRAIL_12M_PROF_MARGIN', #USE
             'RETURN_TOT_EQY', #USE
             'SALES_GROWTH', #USE
             'EBITDA_GROWTH') #USE
  for(i in sheets){
    data=read.xlsx(filepath,sheet=i,startRow = startRow,
                   detectDates = TRUE,skipEmptyCols=FALSE)
    k=1
    for(j in seq(from=3,to=ncol(data),by=2)){
      ticker = strsplit(colnames(data)[j], '[.]')[[1]][1]
      clean_data = na.omit(data[,j:(j+1)])
      if(!(i %in% c('EQY_SH_OUT','CUR_MKT_CAP','EQY_REC_CONS'))){
        clean_data[,1]=filing_dates[[k]][as.character.Date(clean_data[,1]),1]
      }
      to_merge = data.frame(clean_data[,1:2])
      colnames(to_merge)=c("Date",i)
      stocks[[ticker]]=join(stocks[[ticker]],
                            to_merge,by="Date",
                            type="full",match="first")
      k=k+1
    }
  }
  index_data = read.xlsx(filepath,sheet="INDEX",startRow = startRow,
                         detectDates = TRUE,skipEmptyCols=FALSE)
  ticker = strsplit(colnames(index_data)[j], '[.]')[[1]][1]
  index = na.omit(index_data[,3:4])
  colnames(index)=c("Date","IBOV")
  for(i in 1:length(stocks)){
    stocks[[i]]=join(stocks[[i]],
                     index,by="Date",
                     type="full",match="first")
    stocks[[i]]=stocks[[i]][!is.na(stocks[[i]]$Date),]
    stocks[[i]]=stocks[[i]][order(stocks[[i]]$Date),]
    stocks[[i]][,-2]=na.locf(stocks[[i]][,-2],na.rm=FALSE)
    stocks[[i]]=stocks[[i]][!is.na(stocks[[i]]$PX_LAST),]
    stocks[[i]]=data.frame(stocks[[i]][,-1],row.names = stocks[[i]][,1])
    for(j in 1:ncol(stocks[[i]])){
      stocks[[i]][,j]=as.numeric(stocks[[i]][,j])
    }
  }
  return(stocks)
}

# Add or adjust factors that will differ from pre-loaded ones from Bloomberg
modFactors = function(stocks){
  
  for(i in 1:length(stocks)){
    #Momentum
    stocks[[i]]$r.21 = returnOverPeriod(stocks[[i]]$PX_LAST,21)
    stocks[[i]]$r.63 = returnOverPeriod(stocks[[i]]$PX_LAST,63)
    stocks[[i]]$r.126 = returnOverPeriod(stocks[[i]]$PX_LAST,126)
    stocks[[i]]$r.252 = returnOverPeriod(stocks[[i]]$PX_LAST,252)
    
    #Market returns
    stocks[[i]]$ind.r.21 = returnOverPeriod(stocks[[i]]$IBOV,21)
    stocks[[i]]$ind.r.63 = returnOverPeriod(stocks[[i]]$IBOV,63)
    stocks[[i]]$ind.r.126 = returnOverPeriod(stocks[[i]]$IBOV,126)
    stocks[[i]]$ind.r.252 = returnOverPeriod(stocks[[i]]$IBOV,252)
    
    #Fundamentals - EBITDA Yield
    tryCatch(
      {
        stocks[[i]]$EBITDA.EV = 1/((stocks[[i]]$CUR_MKT_CAP+stocks[[i]]$NET_DEBT+
                                 stocks[[i]]$MINORITY_INTEREST)/stocks[[i]]$TRAIL_12M_EBITDA)
      },
      error=function(cond) {
        print(paste(names(stocks)[i]," ",message(cond),"\n",sep=""))
        return(NA)
      }
    )
    
    #Fundamentals - Earnings Yield
    tryCatch(
      {  
        stocks[[i]]$E.P = 1/stocks[[i]]$PE_RATIO
        
      },
      error=function(cond) {
        print(paste(names(stocks)[i]," ",message(cond),"\n",sep=""))
        return(NA)
      }
    )
    
    #Fundamentals - FCF Per Share / Price
    tryCatch(
      {  
        stocks[[i]]$FCF.P = 1/(stocks[[i]]$TRAIL_12M_FREE_CASH_FLOW_PER_SH/stocks[[i]]$PX_LAST)
        
      },
      error=function(cond) {
        print(paste(names(stocks)[i]," ",message(cond),"\n",sep=""))
        return(NA)
      }
    )
    
    #Fundamentals - Price to Book
    tryCatch(
      {  
        stocks[[i]]$P.BV = stocks[[i]]$BOOK_VAL_PER_SH/stocks[[i]]$PX_LAST
        
      },
      error=function(cond) {
        print(paste(names(stocks)[i]," ",message(cond),"\n",sep=""))
        return(NA)
      }
    )
    
    #Fundamentals - Net Debt to EBITDA
    tryCatch(
      {  
        stocks[[i]]$ND.EBITDA = stocks[[i]]$NET_DEBT/stocks[[i]]$TRAIL_12M_EBITDA
        
      },
      error=function(cond) {
        print(paste(names(stocks)[i]," ",message(cond),"\n",sep=""))
        return(NA)
      }
    )
    
    # One-month forward return for stock
    stocks[[i]]$rf.21 = -returnOverPeriod(stocks[[i]]$PX_LAST,-21)
    
    # One day returns
    stocks[[i]]$r.1 = returnOverPeriod(stocks[[i]]$PX_LAST,1)
    
    #Remove full NA columns
    stocks[[i]] <- stocks[[i]][,colSums(is.na(stocks[[i]]))<nrow(stocks[[i]])]
  }
  return(stocks)
}

# Splits dataset into train and test according to desired lengths
splitTrainTest = function(stock,train_len,test_len,starting_date){
  data = na.omit(stock)
  starting_index=which(as.Date(rownames(data))>as.Date(starting_date))[1]
  i=1
  train=list()
  test=list()
  while(nrow(data)>(starting_index+train_len+i*(test_len))){
    train[[i]] = data[(starting_index+(i-1)*test_len):(starting_index+train_len+(i-1)*test_len),]
    test[[i]] = data[(starting_index+train_len+(i-1)*test_len+1):(starting_index+train_len+test_len*i),]
    i=i+1
  }
  test[[i]] = data[(nrow(data)-test_len):nrow(data),]
  train[[i]] = data[(nrow(data)-test_len-train_len):(nrow(data)-test_len-1),]
  return(list(train,test))
}

sig = function(pred,upper,lower){
  res = ifelse(pred>upper,1,ifelse(pred<lower,-1,0))
  return(res)
}

pos = function(sig){
  pos = as.numeric(rep(0,length(sig)))
  for(i in 2:length(sig)){
    pos[i]=ifelse(pos[i-1]+sig[i-1]>1,1,ifelse(pos[i-1]+sig[i-1]<(-1),-1,pos[i-1]+sig[i-1]))
  }
  return(pos)
}

pos2 = function(sig,short=FALSE){
  pos = as.numeric(pmax(c(0,sig[1:(length(sig)-1)]),0))
  return(pos)
}

# Model 1 - Regression
testRegression = function(train,test,upper,lower){
  # Train model
  linReg = lm(rf.21 ~ 
                RETURN_ON_INV_CAPITAL+
                E.P+
                EQY_DVD_YLD_12M+
                EBITDA.EV+
                FCF.P+
                SALES_GROWTH+
                EBITDA_GROWTH+
                RETURN_TOT_EQY+
                TRAIL_12M_PROF_MARGIN+
                r.21+
                r.63+
                r.126+
                r.252+
                ind.r.21+
                ind.r.63+
                ind.r.126+
                ind.r.252,
              data = train[1:(nrow(train)-21),])
  
  # Summary of model
  summary(linReg)
  pred = predict(linReg,test)
  signal = sig(pred=pred,upper,lower)
  position=pos2(signal)
  ret_linreg=test[,"r.1"]*position
  cum_ret_linreg=cumsum(ret_linreg)
  cum_ret_buyhold=cumsum(test[,"r.1"])
  result = xts(cbind(pred,signal,position,test[,"r.1"],
                     ret_linreg,cum_ret_buyhold,cum_ret_linreg),
               order.by=as.Date.character(rownames(test)))
  colnames(result)=c("Predicted","Signal","Position","Daily_Ret",
                     "Linreg_Ret","Cum_Ret_Buy_Hold","Cum_Ret_Linreg")
  return(result)
}

testLasso = function(train,test,upper,lower){
  # Train model
  lambda <- 10^seq(5, -5, length = 100)
  lasso.mod <- glmnet(as.matrix(train[1:(nrow(train)-21),c("RETURN_ON_INV_CAPITAL",
                                                           "E.P",
                                                           "EQY_DVD_YLD_12M",
                                                           "EBITDA.EV",
                                                           "FCF.P",
                                                           "SALES_GROWTH",
                                                           "EBITDA_GROWTH",
                                                           "RETURN_TOT_EQY",
                                                           "TRAIL_12M_PROF_MARGIN",
                                                           "r.21",
                                                           "r.63",
                                                           "r.126",
                                                           "r.252",
                                                           "ind.r.21",
                                                           "ind.r.63",
                                                           "ind.r.126",
                                                           "ind.r.252")]),
                      as.matrix(train[1:(nrow(train)-21),"rf.21"]),
                      alpha = 1, lambda = lambda)
  cv.out <- cv.glmnet(as.matrix(train[1:(nrow(train)-21),c("RETURN_ON_INV_CAPITAL",
                                                           "E.P",
                                                           "EQY_DVD_YLD_12M",
                                                           "EBITDA.EV",
                                                           "FCF.P",
                                                           "SALES_GROWTH",
                                                           "EBITDA_GROWTH",
                                                           "RETURN_TOT_EQY",
                                                           "TRAIL_12M_PROF_MARGIN",
                                                           "r.21",
                                                           "r.63",
                                                           "r.126",
                                                           "r.252",
                                                           "ind.r.21",
                                                           "ind.r.63",
                                                           "ind.r.126",
                                                           "ind.r.252")]),
                      as.matrix(train[1:(nrow(train)-21),"rf.21"]), 
                      alpha = 1, nfolds = 10)
  bestlam <- cv.out$lambda.min
  lasso <- glmnet(as.matrix(train[1:(nrow(train)-21),c("RETURN_ON_INV_CAPITAL",
                                                       "E.P",
                                                       "EQY_DVD_YLD_12M",
                                                       "EBITDA.EV",
                                                       "FCF.P",
                                                       "SALES_GROWTH",
                                                       "EBITDA_GROWTH",
                                                       "RETURN_TOT_EQY",
                                                       "TRAIL_12M_PROF_MARGIN",
                                                       "r.21",
                                                       "r.63",
                                                       "r.126",
                                                       "r.252",
                                                       "ind.r.21",
                                                       "ind.r.63",
                                                       "ind.r.126",
                                                       "ind.r.252")]),
                  as.matrix(train[1:(nrow(train)-21),"rf.21"]), 
                  alpha = 1, lambda = bestlam)
  
  # Summary of model
  summary(lasso)
  pred = predict(lasso,as.matrix(test[,c("RETURN_ON_INV_CAPITAL",
                                         "E.P",
                                         "EQY_DVD_YLD_12M",
                                         "EBITDA.EV",
                                         "FCF.P",
                                         "SALES_GROWTH",
                                         "EBITDA_GROWTH",
                                         "RETURN_TOT_EQY",
                                         "TRAIL_12M_PROF_MARGIN",
                                         "r.21",
                                         "r.63",
                                         "r.126",
                                         "r.252",
                                         "ind.r.21",
                                         "ind.r.63",
                                         "ind.r.126",
                                         "ind.r.252")]))
  signal = sig(pred=pred,upper,lower)
  position=pos2(signal)
  ret_lasso=test[,"r.1"]*position
  cum_ret_lasso=cumsum(ret_lasso)
  cum_ret_buyhold=cumsum(test[,"r.1"])
  result = xts(cbind(pred,signal,position,test[,"r.1"],
                     ret_lasso,cum_ret_buyhold,cum_ret_lasso),
               order.by=as.Date.character(rownames(test)))
  colnames(result)=c("Predicted","Signal","Position","Daily_Ret",
                     "Lasso_Ret","Cum_Ret_Buy_Hold","Cum_Ret_Lasso")
  return(result)
}

# Model 3 - Random Forest
testRandomForest = function(train,test,upper,lower){
  # Train model
  rfReg = randomForest(rf.21 ~ 
                         RETURN_ON_INV_CAPITAL+
                         E.P+
                         EQY_DVD_YLD_12M+
                         EBITDA.EV+
                         FCF.P+
                         SALES_GROWTH+
                         EBITDA_GROWTH+
                         RETURN_TOT_EQY+
                         r.21+
                         r.63+
                         r.126+
                         r.252+
                         ind.r.21+
                         ind.r.63+
                         ind.r.126+
                         ind.r.252,
              data = train[1:(nrow(train)-21),],
              ntree=200)
  
  # Summary of model
  summary(rfReg)
  pred = predict(rfReg,test)
  signal = sig(pred=pred,upper,lower)
  position=pos2(signal)
  ret_rf=test[,"r.1"]*position
  cum_ret_rf=cumsum(ret_rf)
  cum_ret_buyhold=cumsum(test[,"r.1"])
  result = xts(cbind(pred,signal,position,test[,"r.1"],
                     ret_rf,cum_ret_buyhold,cum_ret_rf),
               order.by=as.Date.character(rownames(test)))
  colnames(result)=c("Predicted","Signal","Position","Daily_Ret",
                     "Rf_Ret","Cum_Ret_Buy_Hold","Cum_Ret_Rf")
  return(result)
}

# Model 4 - XGBoost
testXGBoost = function(train,test,upper,lower){
  data= Matrix(as.matrix(train[,c("RETURN_ON_INV_CAPITAL",
                                  "E.P",
                                  "EQY_DVD_YLD_12M",
                                  "EBITDA.EV",
                                  "FCF.P",
                                  "SALES_GROWTH",
                                  "EBITDA_GROWTH",
                                  "RETURN_TOT_EQY",
                                  "r.21",
                                  "r.63",
                                  "r.126",
                                  "r.252",
                                  "ind.r.21",
                                  "ind.r.63",
                                  "ind.r.126",
                                  "ind.r.252")]),sparse = TRUE)
  label = as.numeric(train[,"rf.21"])
  # Train model
  xgbReg = xgboost(data = data, label = label, booster = "gblinear", max_depth = 10, nthread = 2, nrounds = 40)
  
  # Summary of model
  summary(xgbReg)
  pred = predict(xgbReg,Matrix(as.matrix(test[,c("RETURN_ON_INV_CAPITAL",
                                                 "E.P",
                                                 "EQY_DVD_YLD_12M",
                                                 "EBITDA.EV",
                                                 "FCF.P",
                                                 "SALES_GROWTH",
                                                 "EBITDA_GROWTH",
                                                 "RETURN_TOT_EQY",
                                                 "r.21",
                                                 "r.63",
                                                 "r.126",
                                                 "r.252",
                                                 "ind.r.21",
                                                 "ind.r.63",
                                                 "ind.r.126",
                                                 "ind.r.252")]),sparse = TRUE))
  signal = sig(pred=pred,upper,lower)
  position=pos2(signal)
  ret_xgb=test[,"r.1"]*position
  cum_ret_xgb=cumsum(ret_xgb)
  cum_ret_buyhold=cumsum(test[,"r.1"])
  result = xts(cbind(pred,signal,position,test[,"r.1"],
                     ret_xgb,cum_ret_buyhold,cum_ret_xgb),
               order.by=as.Date.character(rownames(test)))
  colnames(result)=c("Predicted","Signal","Position","Daily_Ret",
                     "Xgb_Ret","Cum_Ret_Buy_Hold","Cum_Ret_Xgb")
  return(result)
}