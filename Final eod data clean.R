#importing all non standard libraries
library(pracma)
library(zoo)

#importing eod file
eod=read.csv("eod_fin_mid_cap.csv")
sapply(eod, class) #checking Class of Columns

#Renaming Columns
colnames(eod)<-c("Ticker", "Date", "UnAdjOpen", "UnAdjHigh",
                 "UnAdjLow", "UnAdjClose", "UnAdjVol", "Dividends",
                 "Splits", "AdjOpen", "AdjHigh", "AdjLow", "AdjClose",
                 "AdjVol")
View(eod)

#Sorting dataframe by ticker then by date
eod<-eod[order(eod$Ticker,eod$Date),]

unique_tickers<-unique(eod$Ticker)  #Unique set of Tickers
eod_entries<-dim(eod)[1]            #Number of Observations

#####################################################
# Returns
#####################################################

returns<-vector()
prev_ticker=""
for(i in 1:eod_entries){
  if(eod$Ticker[i]!=prev_ticker){
    returns=c(returns, NA)
  }
  else{
    returns[i]<-(eod$AdjClose[i]/eod$AdjClose[i-1])
  }
  prev_ticker=eod$Ticker[i]
}

eod$returns_adj_close<-returns

#####################################################
#ADX
#####################################################

#calculating upmove and downmove
upmove<-vector()
downmove<-vector()
prev_ticker=""
for(i in 1:eod_entries){
  if(eod$Ticker[i]!=prev_ticker){
    upmove[i]=NA
    downmove[i]=NA
  }
  else{
    upmove[i]<-eod$AdjHigh[i]-eod$AdjHigh[i-1]
    downmove[i]<-eod$AdjLow[i-1]-eod$AdjLow[i]
  }
  prev_ticker=eod$Ticker[i]
}
eod$upmove<-upmove
eod$downmove<-downmove

#calulating +DM and -DM
pos.DM<-vector()
neg.DM<-vector()
prev_ticker=""
for(i in 1:eod_entries){
  if(eod$Ticker[i]!=prev_ticker){
    pos.DM[i]<-NA
    neg.DM[i]<-NA
  }
  else{

    if(eod$upmove[i]>eod$downmove[i] & eod$upmove[i]>0){
      pos.DM[i]<-eod$upmove[i]
      neg.DM[i]<-0
    }
    else if(eod$downmove[i]>eod$upmove[i] & eod$downmove[i]>0){
      neg.DM[i]<-eod$downmove[i]
      pos.DM[i]<-0
    }
    else{
      pos.DM[i]<-0
      neg.DM[i]<-0
    }  
  }
  prev_ticker<-eod$Ticker[i]
}
eod$pos.DM<-pos.DM
eod$neg.DM<-neg.DM

#Calculating true range
tr<-vector()
prev_ticker=""
for(i in 1:eod_entries){
  if(eod$Ticker[i]!=prev_ticker){
    tr[i]<-NA
  }
  else{
    tr[i]<-max((eod$AdjHigh[i]-eod$AdjLow[i]),
               abs(eod$AdjHigh[i]-eod$AdjClose[i-1]),
               abs(eod$AdjLow[i]-eod$AdjClose[i-1]))
  }
  prev_ticker<-eod$Ticker[i]
}
eod$tr<-tr


#calculating Directional Indicators i.e. pos.DI and neg.DI

#14day
pos.DI=vector()
neg.DI=vector()

for(i in 1:length(unique_tickers)){
  temp.df=eod[eod$Ticker==unique_tickers[i],c("pos.DM","neg.DM","tr")]
  pos.DI<-c(pos.DI, (100*movavg(temp.df$pos.DM, 14, type="s"))/movavg(temp.df$tr, 14, type="s"))
  neg.DI<-c(neg.DI, (100*movavg(temp.df$neg.DM, 14, type="s"))/movavg(temp.df$tr, 14, type="s"))
}
eod$pos.DI<-pos.DI
eod$neg.DI<-neg.DI

#ADX
adx.5=vector()
adx.14=vector()
adx.30=vector()
adx.60=vector()

for(i in 1:length(unique_tickers)){
  temp.df=eod[eod$Ticker==unique_tickers[i],c("pos.DI","neg.DI")]
  #5 day
  adx.5<-c(adx.5, (100*movavg(abs(temp.df$pos.DI-temp.df$neg.DI),5,type="s"))/(temp.df$pos.DI+temp.df$neg.DI))
  #14 day
  adx.14<-c(adx.14, (100*movavg(abs(temp.df$pos.DI-temp.df$neg.DI),14,type="s"))/(temp.df$pos.DI+temp.df$neg.DI))
  #30 day
  adx.30<-c(adx.30, (100*movavg(abs(temp.df$pos.DI-temp.df$neg.DI),30,type="s"))/(temp.df$pos.DI+temp.df$neg.DI))
  #60 day
  adx.60<-c(adx.60, (100*movavg(abs(temp.df$pos.DI-temp.df$neg.DI),60,type="s"))/(temp.df$pos.DI+temp.df$neg.DI))
}

eod$adx.5<-adx.5
eod$adx.14<-adx.14
eod$adx.30<-adx.30
eod$adx.60<-adx.60


#####################################################
#RSI
#####################################################

# Gain & Loss
gain<-vector()
loss<-vector()
prev_ticker<-""
for(i in 1:eod_entries){
  if(eod$Ticker[i]!=prev_ticker){
    gain[i]=NA
    loss[i]=NA
  }
  else{
    if(eod$AdjClose[i]-eod$AdjClose[i-1]>0){
      gain[i]=eod$AdjClose[i]-eod$AdjClose[i-1]
      loss[i]=0
    }
    else{
      gain[i]=0
      loss[i]=eod$AdjClose[i-1]-eod$AdjClose[i]
      
    }
  }
  prev_ticker=eod$Ticker[i]
}
eod$gain<-gain
eod$loss<-loss


#Average Gain and Loss
avg.gain=vector()
avg.loss=vector()

for(i in 1:length(unique_tickers)){  
  tic_df = eod[eod$Ticker==unique_tickers[i],c("gain","loss")]
  avg.gain=c(avg.gain, movavg(tic_df$gain,14,type="s"))
  avg.loss=c(avg.loss, movavg(tic_df$loss,14,type="s"))
}
eod$Avg.Gain<-avg.gain
eod$Avg.Loss<-avg.loss

#RS 
eod$rs<-eod$Avg.Gain/eod$Avg.Loss

#Calculating Moving Average RS
ma.rs<-vector()
for(i in 1:length(unique_tickers)){  
  rs = eod[eod$Ticker==unique_tickers[i],c("rs")]
  ma.rs<-c(ma.rs, movavg(rs,14,type="s"))
}
eod$ma.rs<-ma.rs

#RSI
eod$rsi<-(100-100/(1+eod$ma.rs))


#####################################################
#MACD
#####################################################

ma.5<-vector()
ma.10<-vector()
ma.20<-vector()
ma.40<-vector()
ma.80<-vector()

for(i in 1:length(unique_tickers)){  
  AdjClose = eod[eod$Ticker==unique_tickers[i],c("AdjClose")]
  ma.5<-c(ma.5, movavg(AdjClose,5,type="e"))
  ma.10<-c(ma.10, movavg(AdjClose,10,type="e"))
  ma.20<-c(ma.20, movavg(AdjClose,20,type="e"))
  ma.40<-c(ma.40, movavg(AdjClose,40,type="e"))
  ma.80<-c(ma.80, movavg(AdjClose,80,type="e"))
}

eod$ma.5<-ma.5
eod$ma.10<-ma.10
eod$ma.20<-ma.20
eod$ma.40<-ma.40
eod$ma.80<-ma.80


eod$macd.10v5<-eod$ma.10-eod$ma.5
eod$macd.20v10<-eod$ma.20-eod$ma.10
eod$macd.40v20<-eod$ma.40-eod$ma.20
eod$macd.80v40<-eod$ma.80-eod$ma.40


#####################################################
# Stochastic Oscillators
#####################################################


percent.K.5<-vector()
percent.K.14<-vector()
percent.K.30<-vector()
percent.K.60<-vector()

for(i in 1:length(unique_tickers)){  
  temp.df = eod[eod$Ticker==unique_tickers[i],c("AdjClose","AdjHigh","AdjLow")]
  #5 day
  K.5<-(temp.df$AdjClose-rollapply(temp.df$AdjLow, width=5, FUN=min, fill=NA, align="right"))/
    (rollapply(temp.df$AdjHigh, width=5, FUN=max, fill=NA, align="right")-
     rollapply(temp.df$AdjLow, width=5, FUN=min, fill=NA, align="right"))
  percent.K.5<-c(percent.K.5, k)
  
  #14 day
  K.14<-(temp.df$AdjClose-rollapply(temp.df$AdjLow, width=14, FUN=min, fill=NA, align="right"))/
    (rollapply(temp.df$AdjHigh, width=14, FUN=max, fill=NA, align="right")-
     rollapply(temp.df$AdjLow, width=14, FUN=min, fill=NA, align="right"))
  percent.K.14<-c(percent.K.14, K.14)
  
  #30 day
  K.30<-(temp.df$AdjClose-rollapply(temp.df$AdjLow, width=30, FUN=min, fill=NA, align="right"))/
    (rollapply(temp.df$AdjHigh, width=30, FUN=max, fill=NA, align="right")-
     rollapply(temp.df$AdjLow, width=30, FUN=min, fill=NA, align="right"))
  percent.K.30<-c(percent.K.30, K.30) 
  
  #60 day
  K.60<-(temp.df$AdjClose-rollapply(temp.df$AdjLow, width=60, FUN=min, fill=NA, align="right"))/
    (rollapply(temp.df$AdjHigh, width=60, FUN=max, fill=NA, align="right")-
     rollapply(temp.df$AdjLow, width=60, FUN=min, fill=NA, align="right"))
  percent.K.60<-c(percent.K.60, K.60) 


}

eod$percent.K.5<-percent.K.5
eod$percent.K.14<-percent.K.14
eod$percent.K.30<-percent.K.30
eod$percent.K.60<-percent.K.60



#Dropping UnAdj Pricing Data
eod<-eod[,-c(3:9)]
View(eod)

#Calculating 40 day Maximum Absolute Value
max.vol<-vector()
max.close<-vector()
max.macd.10v5<-vector()
max.macd.20v10<-vector()
max.macd.40v20<-vector()
max.macd.80v40<-vector()
max.rsi<-vector
max.percent.K.5<-vector()
max.percent.K.14<-vector()
max.percent.K.30<-vector()
max.percent.K.60<-vector()
max.adx.5<-vector()
max.adx.14<-vector()
max.adx.30<-vector()
max.adx.60<-vector()


for(i in 1:length(unique_tickers)){
  temp.df=eod[eod$Ticker==unique_tickers[i],]
  max.vol<-c(max.vol, rollapply(abs(temp.df$AdjVol),width=40,FUN=max,fill=NA, align="right"))
  max.close<-c(max.close, rollapply(abs(temp.df$AdjClose),width=40,FUN=max,fill=NA, align="right"))
  max.macd.10v5<-c(max.macd.10v5, rollapply(abs(temp.df$macd.10v5),width=40,FUN=max,fill=NA, align="right"))
  max.macd.20v10<-c(max.macd.20v10, rollapply(abs(temp.df$macd.20v10),width=40,FUN=max,fill=NA, align="right"))
  max.macd.40v20<-c(max.macd.40v20, rollapply(abs(temp.df$macd.40v20),width=40,FUN=max,fill=NA, align="right"))
  max.macd.80v40<-c(max.macd.80v40, rollapply(abs(temp.df$macd.80v40),width=40,FUN=max,fill=NA, align="right"))
  max.rsi<-c(max.rsi, rollapply(abs(temp.df$rsi),width=40,FUN=max,fill=NA, align="right"))
  max.percent.K.5<-c(max.percent.K.5, rollapply(abs(temp.df$percent.K.5),width=40,FUN=max,fill=NA, align="right"))
  max.percent.K.14<-c(max.percent.K.14, rollapply(abs(temp.df$percent.K.14),width=40,FUN=max,fill=NA, align="right"))
  max.percent.K.30<-c(max.percent.K.30, rollapply(abs(temp.df$percent.K.30),width=40,FUN=max,fill=NA, align="right"))
  max.percent.K.60<-c(max.percent.K.60, rollapply(abs(temp.df$percent.K.60),width=40,FUN=max,fill=NA, align="right"))
  max.adx.5<-c(max.adx.5, rollapply(abs(temp.df$adx.5),width=40,FUN=max,fill=NA, align="right"))
  max.adx.14<-c(max.adx.14, rollapply(abs(temp.df$adx.14),width=40,FUN=max,fill=NA, align="right"))
  max.adx.30<-c(max.adx.30, rollapply(abs(temp.df$adx.30),width=40,FUN=max,fill=NA, align="right"))
  max.adx.60<-c(max.adx.60, rollapply(abs(temp.df$adx.60),width=40,FUN=max,fill=NA, align="right"))
  
}



#Normalizing Data
eod$AdjVol<-eod$AdjVol/max.vol
eod$AdjClose<-eod$AdjClose/max.close
eod$AdjOpen<-eod$AdjOpen/max.close
eod$AdjHigh<-eod$AdjHigh/max.close
eod$AdjLow<-eod$AdjLow/max.close
eod$macd.10v5<-eod$macd.10v5/max.macd.10v5
eod$macd.20v10<-eod$macd.20v10/max.macd.20v10
eod$macd.40v20<-eod$macd.40v20/max.macd.40v20
eod$macd.80v40<-eod$macd.80v40/max.macd.80v40
eod$rsi<-eod$rsi/max.rsi
eod$percent.K.5<-eod$percent.K.5/max.percent.K.5
eod$percent.K.14<-eod$percent.K.14/max.percent.K.14
eod$percent.K.30<-eod$percent.K.30/max.percent.K.30
eod$percent.K.60<-eod$percent.K.60/max.percent.K.60
eod$adx.5<-eod$adx.5/max.adx.5
eod$adx.14<-eod$adx.14/max.adx.14
eod$adx.30<-eod$adx.30/max.adx.30
eod$adx.60<-eod$adx.60/max.adx.60

#Omitting na from dataframe and saving dataframe as myeod.csv
eod1=na.omit(eod)
write.csv(eod1,file = "myeod.csv", row.names = FALSE)
