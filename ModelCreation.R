#First Cut model
summary(model <- lm(IOC.NS~PFC.NS_LAG_8 + PEL.NS_LAG_7 + BAJFINANCE.NS_LAG_4 ,data = data))

#New Data
tckk1 <-c('ABB.NS', 'APOLLOHOSP.NS', 'ASHOKLEY.NS', 'BAJFINANCE.NS', 'BAJAJFINSV.NS', 'BHARATFORG.NS', 'BRITANNIA.NS', 'CADILAHC.NS', 'CASTROLIND.NS', 'COLPAL.NS', 'CONCOR.NS', 'CUMMINSIND.NS', 'DLF.NS', 'DABUR.NS', 'DIVISLAB.NS', 'EMAMILTD.NS', 'GSKCONS.NS', 'GLAXO.NS', 'GLENMARK.NS', 'GODREJCP.NS', 'HAVELLS.NS', 'HINDPETRO.NS', 'HINDZINC.NS', 'IBULHSGFIN.NS', 'IOC.NS', 'INDIGO.NS', 'JSWSTEEL.NS', 'LICHSGFIN.NS', 'MARICO.NS', 'MOTHERSUMI.NS', 'NHPC.NS', 'NMDC.NS', 'OIL.NS', 'OFSS.NS', 'PIDILITIND.NS', 'PEL.NS', 'PFC.NS', 'PGHH.NS', 'PNB.NS', 'SHREECEM.NS', 'SRTRANSFIN.NS', 'SIEMENS.NS', 'SAIL.NS', 'TITAN.NS', 'TORNTPHARM.NS', 'UPL.NS', 'UBL.NS', 'VEDL.NS')# ticker names defined  
numtk1 <-length(tckk1);  

ustart1 <-"2017-01-02";uend1 <-"2017-03-24" #start and end date  

test1 <- xxx<-get.hist.quote(instrument = tckk1[1], start=ustart1, end=uend1,quote = c("Close"),provider = "yahoo", compression = "d")
test1 = do.call(rbind, lapply(test1, data.frame, stringsAsFactors=FALSE))

all_data1 <- data.frame(matrix(nrow = nrow(test1),ncol = numtk1))
for(i in 1:numtk1)  
{
  all_dat<-list()#empty list to fill in the data 
  data <- data.frame()
  all_dat[[1]]<-xxx<-get.hist.quote(instrument = tckk1[i], start=ustart1, end=uend1,quote = c("Close"),provider = "yahoo", compression = "d")  
  data = do.call(rbind, lapply(all_dat, data.frame, stringsAsFactors=FALSE))
  all_data1[i] <- NA
  all_data1[i] <- data[1]
  colnames(all_data1)[i] <- tckk1[i] 
}   

rownames(all_data1) <- row.names(data)
rm(test1,data)

month_return1 <- data.frame(matrix(nrow = nrow(monthlyReturn(all_data1)), ncol = numtk1))
weekly_return1 <- data.frame(matrix(nrow = nrow(weeklyReturn(all_data1)), ncol = numtk1 ))

for(i in 1:numtk1){
  mdata1 <- as.data.frame(monthlyReturn(all_data1[i]))
  wdata1 <- as.data.frame(weeklyReturn(all_data1[i]))
  month_return1[i] <- mdata1[1]
  weekly_return1[i] <- wdata1[1]
  colnames(month_return1)[i] <- tckk1[i]
  colnames(weekly_return1)[i] <- tckk1[i]
}

rm(mdata1,wdata1)

create_lag <- function(df,vars,lag){
  
  for(i in 1:nrow(df)){
    df$Row[i] <- i
  }
  
  df_1 <- df
  df_1 <- as.data.frame(df_1[,vars])
  for(i in 1:nrow(df_1)){
    df_1$Row[i] <- i
  }
  df_1$Row <- df_1$Row + lag
  names(df_1)[1:(ncol(df_1)-1)] <- paste(vars,"LAG",lag,sep = "_")
  
  return(df_1)
  
}

modeldata1 <- weekly_return1

for(i in 1:nrow(modeldata1)){
  modeldata1$Row[i] <- i
}

lag1 <- create_lag(modeldata1,tckk,1)
lag2 <- create_lag(modeldata1,tckk,2)
lag3 <- create_lag(modeldata1,tckk,3)
lag4 <- create_lag(modeldata1,tckk,4)
lag5 <- create_lag(modeldata1,tckk,5)
lag6 <- create_lag(modeldata1,tckk,6)
lag7 <- create_lag(modeldata1,tckk,7)
lag8 <- create_lag(modeldata1,tckk,8)

modeldataLag1 <- merge(modeldata1,lag1,by="Row", all.x = T)
modeldataLag1 <- merge(modeldataLag1,lag2,by="Row", all.x = T)
modeldataLag1 <- merge(modeldataLag1,lag3,by="Row", all.x = T)
modeldataLag1 <- merge(modeldataLag1,lag4,by="Row", all.x = T)
modeldataLag1 <- merge(modeldataLag1,lag5,by="Row", all.x = T)
modeldataLag1 <- merge(modeldataLag1,lag6,by="Row", all.x = T)
modeldataLag1 <- merge(modeldataLag1,lag7,by="Row", all.x = T)
modeldataLag1 <- merge(modeldataLag1,lag8,by="Row", all.x = T)

rm(modeldata1,lag1,lag2,lag3,lag4,lag5,lag6,lag7,lag8)
modeldataLag1$Row <- NULL

a <- predict(model, modeldataLag1)
b <- modeldataLag1$IOC.NS
c <- as.data.frame(cbind(a,b))
