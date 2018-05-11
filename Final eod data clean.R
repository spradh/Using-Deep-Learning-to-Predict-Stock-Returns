#importing eod file
eod=read.csv("eod_fin_mid_cap.csv")

library(pracma)
View(eod)
sapply(eod, class)
colnames(eod)<-c("Ticker", "Date", "UnAdjOpen", "UnAdjHigh",
                 "UnAdjLow", "UnAdjClose", "UnAdjVol", "Dividends",
                 "Splits", "AdjOpen", "AdjHigh", "AdjLow", "AdjClose",
                 "AdjVol")
View(eod)
eod<-eod[order(eod$Ticker,eod$Date),]

unique_tickers<-unique(eod$Ticker)

eod_entries<-dim(eod)[1]


#calculating log return
log_return<-vector()
prev_ticker=""
for(i in 1:eod_entries){
  if(eod$Ticker[i]!=prev_ticker){
    log_return=c(log_return, NA)
  }
  else{
    log_return[i]<-log(eod$AdjClose[i]/eod$AdjClose[i-1])
  }
  prev_ticker=eod$Ticker[i]
}

eod$log_returns_adj_close<-log_return



#calculating upmove
upmove<-vector()
prev_ticker=""
for(i in 1:eod_entries){
  if(eod$Ticker[i]!=prev_ticker){
    upmove[i]=NA
  }
  else{
    upmove[i]<-eod$AdjHigh[i]-eod$AdjHigh[i-1]
  }
  prev_ticker=eod$Ticker[i]
}
eod$upmove<-upmove

#calculating downmove
downmove<-vector()
prev_ticker=""
for(i in 1:eod_entries){
  if(eod$Ticker[i]!=prev_ticker){
    downmove[i]=NA
  }
  else{
    downmove[i]<-eod$AdjLow[i-1]-eod$AdjLow[i]
  }
  prev_ticker=eod$Ticker[i]
}
eod$downmove<-downmove

#calulating +DM
pos.DM<-vector()
prev_ticker=""
for(i in 1:eod_entries){
  if(eod$Ticker[i]!=prev_ticker){
    pos.DM[i]<-NA
  }
  else{
    if(eod$upmove[i]>eod$downmove[i] & eod$upmove[i]>0){
      pos.DM[i]<-eod$upmove[i]
    }
    else{
      pos.DM[i]<-0
    }
  }
  prev_ticker<-eod$Ticker[i]
}
eod$pos.DM<-pos.DM

#calulating -DM
neg.DM<-vector()
prev_ticker=""
for(i in 1:eod_entries){
  if(eod$Ticker[i]!=prev_ticker){
    neg.DM[i]<-NA
  }
  else{
    if(eod$downmove[i]>eod$upmove[i] & eod$downmove[i]>0){
      neg.DM[i]<-eod$downmove[i]
    }
    else{
      neg.DM[i]<-0
    }
  }
  prev_ticker<-eod$Ticker[i]
}
eod$neg.DM<-neg.DM


#Average true range
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
  tic_df=eod[eod$Ticker==unique_tickers[i],c("pos.DM","tr")]
  pos.DI<-c(pos.DI, (100*movavg(tic_df$pos.DM, 14, type="s"))/movavg(tic_df$tr, 14, type="s"))
  neg.DI<-c(neg.DI, (100*movavg(tic_df$neg.DM, 14, type="s"))/movavg(tic_df$tr, 14, type="s"))
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

 

#Calculating RSI

# Gain & Loss
gain<-vector()
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


#MACD
ma.5<-vector()
ma.10<-vector()
ma.20<-vector()
ma.40<-vector()
ma.80<-vector()

for(i in 1:length(unique_tickers)){  
  AdjClose = eod[eod$Ticker==unique_tickers[i],c("AdjClose")]
  cat(unique_tickers[i],length(AdjClose),"\n")
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


#Stochastic Indicator
#SI5
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

#Calculating 40 day Moving Standard Deviation and Moving Averages of Closing Price and Volume
sd.40.vol<-vector()
sd.40.close<-vector()
ma.40.vol<-vector()

prev_ticker=""
for(i in 1:eod_entries){
  temp.df=eod[eod$Ticker==unique_tickers[i],c("AdjClose","AdjVol")]
  sd.close<-rollapply(temp.df$AdjClose,width=40,FUN=sd,fill=NA, align="right")
  sd.vol<-rollapply(temp.df$AdjVol,width=40,FUN=sd,fill=NA, align="right")
  
  #appending it to respective vectors
  sd.40.vol<-c(sd.40.vol, sd.vol)
  sd.40.close<-c(sd.40.close, sd.close)
  ma.40.vol<-c(vol.ma.40, movavg(AdjVol,40,type="s"))
}

eod$sd.40.vol<-sd.40.vol
eod$sd.40.close<-sd.40.close
eod$ma.40.vol<-vol.ma.40

#Normalizing Pricing Data
eod$AdjOpen<-(eod$AdjOpen-eod$ma.40)/eod$sd.40
eod$AdjClose<-(eod$AdjClose-eod$ma.40)/eod$sd.40
eod$AdjHigh<-(eod$AdjHigh-eod$ma.40)/eod$sd.40
eod$AdjLow<-(eod$AdjLow-eod$ma.40)/eod$sd.40

#normalizing Volume Data
eod$AdjVol<-(eod$AdjVol-eod$vol.ma.40)/eod$vol.sd.40


#Omitting na from dataframe and saving dataframe as myeod.csv
eod1=na.omit(eod)
write.csv(eod1,file = "myeod.csv", row.names = FALSE)
