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
  sheets = c('ANNOUNCEMENT_DT',
             'NET_DEBT',
             'TRAIL_12M_EBITDA',
             'MINORITY_INTEREST',
             'EARN_FOR_COMMON',
             'BOOK_VAL_PER_SH',
             'OPERATING_ROIC',
             'RETURN_ON_INV_CAPITAL',
             'ROIC_WACC_RATIO',
             'TRAIL_12M_NET_INC',
             'EQY_SH_OUT',
             'CUR_MKT_CAP',
             'PX_LAST',
             'EQY_REC_CONS',
             'SALES_GROWTH',
             'EBITDA_GROWTH',
             'EPS_GROWTH',
             'CASH_FLOW_GROWTH')
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
modFactors = function(stocks,yr_upside,yr_downside,short=c(TRUE,FALSE)){
  
  for(i in 1:length(stocks)){
    #Momentum - RSI
    stocks[[i]]$RSI = RSI(stocks[[i]]$PX_LAST,n=21,maType="SMA") # 1 month RSI
    stocks[[i]]$RSI.Sig = -(pmax(stocks[[i]]$RSI-70,0)+pmin(stocks[[i]]$RSI-30,0))
    
    #Moving average
    stocks[[i]]$SMA.21 = SMA(stocks[[i]]$PX_LAST,n=21) # 1 month SMA
    stocks[[i]]$SMA.63 = SMA(stocks[[i]]$PX_LAST,n=63) # 3 month SMA
    stocks[[i]]$SMA.Sig = c(diff(sign(stocks[[i]]$SMA.21-stocks[[i]]$SMA.63)),NA)
    
    #Analyst consensus
    stocks[[i]]$EQY_REC_CONS.Sig = c(diff(stocks[[i]]$EQY_REC_CONS),NA)
    
    #Fundamentals - EV/EBITDA
    tryCatch(
      {
        stocks[[i]]$EV.EBITDA = (stocks[[i]]$CUR_MKT_CAP+stocks[[i]]$NET_DEBT+
                                 stocks[[i]]$MINORITY_INTEREST)/stocks[[i]]$TRAIL_12M_EBITDA
        stocks[[i]]$EV.EBITDA.Sig = -(stocks[[i]]$EV.EBITDA-SMA(stocks[[i]]$EV.EBITDA,n=500))
      },
      error=function(cond) {
        print(paste(names(stocks)[i]," ",message(cond),"\n",sep=""))
        return(NA)
      }
    )
    
    tryCatch(
      {  
        #Fundamentals - P/E
        stocks[[i]]$P.E = stocks[[i]]$CUR_MKT_CAP/stocks[[i]]$TRAIL_12M_NET_INC
        stocks[[i]]$P.E.Sig = -(stocks[[i]]$P.E-SMA(stocks[[i]]$P.E,n=500))
        
        #Fundamentals - Change in ROIC
        stocks[[i]]$ROIC.Sig = stocks[[i]]$RETURN_ON_INV_CAPITAL-SMA(stocks[[i]]$RETURN_ON_INV_CAPITAL,n=500)
      },
      error=function(cond) {
        print(paste(names(stocks)[i]," ",message(cond),"\n",sep=""))
        return(NA)
      }
    )
    
    
    # One-month forward return for stock
    stocks[[i]]$rf.21 = -returnOverPeriod(stocks[[i]]$PX_LAST,-21)
    
    # Three-month forward return for stock
    stocks[[i]]$rf.63 = -returnOverPeriod(stocks[[i]]$PX_LAST,-63)
    
    # One day historical return
    stocks[[i]]$r.1 = returnOverPeriod(stocks[[i]]$PX_LAST,1)
    
    stocks[[i]]$pos.21 = ifelse(stocks[[i]]$rf.21>yr_upside/12,1,
                                 ifelse(stocks[[i]]$rf.21<(yr_downside/12),-1,0))
    stocks[[i]]$pos.63 = ifelse(stocks[[i]]$rf.63>yr_upside/4,1,
                                ifelse(stocks[[i]]$rf.21<(yr_downside/4),-1,0))
    if(short==FALSE){
      stocks[[i]]$pos.21[stocks[[i]]$pos.21==-1]=0
      stocks[[i]]$pos.63[stocks[[i]]$pos.63==-1]=0
    }
    
    #Remove columns that will not be used
    stocks[[i]][,c("ANNOUNCEMENT_DT",
                   "NET_DEBT",
                   "MINORITY_INTEREST",
                   "EARN_FOR_COMMON",
                   "BOOK_VAL_PER_SH",
                   "OPERATING_ROIC",
                   "EQY_SH_OUT",
                   "CUR_MKT_CAP")]=NULL
    
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

# To be deprecated
walkForward = function(stock_A,train_len,test_len,starting_date){
  data = na.omit(stock_A)
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
  
  # Random Forest
  for(i in 1:length(train)){
    # Train #
    my_forest = randomForest(as.factor(pos.63) ~ 
                               RSI.Sig+
                               SMA.Sig+
                               EQY_REC_CONS.Sig+
                               SALES_GROWTH+
                               EBITDA_GROWTH+
                               EV.EBITDA.Sig+
                               P.E.Sig+
                               ROIC.Sig,
                             data = train[[i]],
                             importance = TRUE,
                             ntree = 200)
    
    # Test #
    test[[i]]$rf.63 = NULL
    my_prediction = predict(my_forest,test[[i]])
    print(table(my_prediction,test[[i]]$pos.63))
    position = xts(x=as.numeric(as.character(my_prediction)), order.by=as.Date(rownames(test[[i]])))
    colnames(position)="pos"
    test[[i]]$pos.63 = lag(position,1)
    
  }
  walkFwd = do.call(rbind,test[-length(test)])
  stockPerformance = xts(x=na.omit(walkFwd)[,c("PX_LAST","pos.63")],order.by=as.Date(rownames(na.omit(walkFwd))))
  stockPerformance$Ret = diff(log(stockPerformance$PX_LAST),na.rm=FALSE)
  stockPerformance = na.omit(stockPerformance)
  
  # Long only strategy
  stockPerformance$LongOnlyDaily = pmax(stockPerformance$pos.63,0)*stockPerformance$Ret
  stockPerformance$LongOnly = cumsum(stockPerformance$LongOnlyDaily)
  stockPerformance$BuyHold = cumsum(stockPerformance$Ret)
  LongOnly.MaxDD = maxdrawdown(stockPerformance$LongOnlyDaily)
  BuyHold.MaxDD = maxdrawdown(stockPerformance$Ret)
  txt = sprintf("Long Only Maximum Pct. Drawdown: %.2f",LongOnly.MaxDD*100)
  print(txt)
  print(SharpeRatio(R=stockPerformance$LongOnlyDaily,FUN="StdDev",annualize = TRUE))
  txt = sprintf("Buy and Hold Maximum Pct. Drawdown: %.2f",BuyHold.MaxDD*100)
  print(txt)
  print(SharpeRatio(R=stockPerformance$Ret,FUN="StdDev",annualize = TRUE))
  autoplot(stockPerformance[,c("LongOnly","BuyHold")])
  
  # Long and short
  stockPerformance$LongShortDaily = stockPerformance$pos*stockPerformance$Ret
  stockPerformance$LongShort = cumsum(stockPerformance$LongShortDaily)
  stockPerformance$BuyHold = cumsum(stockPerformance$Ret)
  LongShort.MaxDD = maxdrawdown(stockPerformance$LongShortDaily)
  BuyHold.MaxDD = maxdrawdown(stockPerformance$Ret)
  txt = sprintf("Long Short Maximum Pct. Drawdown: %.2f",LongShort.MaxDD*100)
  print(txt)
  print(SharpeRatio(R=stockPerformance$LongShortDaily,FUN="StdDev",annualize = TRUE))
  txt = sprintf("Buy and Hold Maximum Pct. Drawdown: %.2f",BuyHold.MaxDD*100)
  print(txt)
  print(SharpeRatio(R=stockPerformance$Ret,FUN="StdDev",annualize = TRUE))
  autoplot(stockPerformance[,c("LongShort","BuyHold")])
}

