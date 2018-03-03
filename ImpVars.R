rm(list = ls())

setwd("C:\\Users\\rpankajdave\\Desktop\\Stock Market")

library(data.table)
library(lubridate)
library(quantmod)
library(corrplot)
library(tseries)  
library(forecast)
library(randomForest)
library(reshape)

tckk <-c('ABB.NS', 'APOLLOHOSP.NS', 'ASHOKLEY.NS', 'BAJFINANCE.NS', 'BAJAJFINSV.NS', 'BHARATFORG.NS', 'BRITANNIA.NS', 'CADILAHC.NS', 'CASTROLIND.NS', 'COLPAL.NS', 'CONCOR.NS', 'CUMMINSIND.NS', 'DLF.NS', 'DABUR.NS', 'DIVISLAB.NS', 'EMAMILTD.NS', 'GSKCONS.NS', 'GLAXO.NS', 'GLENMARK.NS', 'GODREJCP.NS', 'HAVELLS.NS', 'HINDPETRO.NS', 'HINDZINC.NS', 'IBULHSGFIN.NS', 'IOC.NS', 'INDIGO.NS', 'JSWSTEEL.NS', 'LICHSGFIN.NS', 'MARICO.NS', 'MOTHERSUMI.NS', 'NHPC.NS', 'NMDC.NS', 'OIL.NS', 'OFSS.NS', 'PIDILITIND.NS', 'PEL.NS', 'PFC.NS', 'PGHH.NS', 'PNB.NS', 'SHREECEM.NS', 'SRTRANSFIN.NS', 'SIEMENS.NS', 'SAIL.NS', 'TITAN.NS', 'TORNTPHARM.NS', 'UPL.NS', 'UBL.NS', 'VEDL.NS')# ticker names defined  
numtk <-length(tckk);  

ustart <-"2016-01-04";uend <-"2017-01-10" #start and end date  

test <- xxx<-get.hist.quote(instrument = tckk[1], start=ustart, end=uend,quote = c("Close"),provider = "yahoo", compression = "d")
test = do.call(rbind, lapply(test, data.frame, stringsAsFactors=FALSE))

all_data <- data.frame(matrix(nrow = nrow(test),ncol = numtk))
for(i in 1:numtk)  
{
  all_dat<-list()#empty list to fill in the data 
  data <- data.frame()
  all_dat[[1]]<-xxx<-get.hist.quote(instrument = tckk[i], start=ustart, end=uend,quote = c("Close"),provider = "yahoo", compression = "d")  
  data = do.call(rbind, lapply(all_dat, data.frame, stringsAsFactors=FALSE))
  all_data[i] <- NA
  all_data[i] <- data[1]
  colnames(all_data)[i] <- tckk[i] 
}   

rownames(all_data) <- row.names(data)
rm(test,data)

month_return <- data.frame(matrix(nrow = nrow(monthlyReturn(all_data)), ncol = numtk))
weekly_return <- data.frame(matrix(nrow = nrow(weeklyReturn(all_data)), ncol = numtk ))

for(i in 1:numtk){
  mdata <- as.data.frame(monthlyReturn(all_data[i]))
  wdata <- as.data.frame(weeklyReturn(all_data[i]))
  month_return[i] <- mdata[1]
  weekly_return[i] <- wdata[1]
  colnames(month_return)[i] <- tckk[i]
  colnames(weekly_return)[i] <- tckk[i]
}

rm(mdata,wdata)

#correlation <- cor(all_data)
#corrplot(correlation, method = "circle")

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

modeldata <- weekly_return

for(i in 1:nrow(modeldata)){
  modeldata$Row[i] <- i
}

lag1 <- create_lag(modeldata,tckk,1)
lag2 <- create_lag(modeldata,tckk,2)
lag3 <- create_lag(modeldata,tckk,3)
lag4 <- create_lag(modeldata,tckk,4)
lag5 <- create_lag(modeldata,tckk,5)
lag6 <- create_lag(modeldata,tckk,6)
lag7 <- create_lag(modeldata,tckk,7)
lag8 <- create_lag(modeldata,tckk,8)

modeldataLag <- merge(modeldata,lag1,by="Row", all.x = T)
modeldataLag <- merge(modeldataLag,lag2,by="Row", all.x = T)
modeldataLag <- merge(modeldataLag,lag3,by="Row", all.x = T)
modeldataLag <- merge(modeldataLag,lag4,by="Row", all.x = T)
modeldataLag <- merge(modeldataLag,lag5,by="Row", all.x = T)
modeldataLag <- merge(modeldataLag,lag6,by="Row", all.x = T)
modeldataLag <- merge(modeldataLag,lag7,by="Row", all.x = T)
modeldataLag <- merge(modeldataLag,lag8,by="Row", all.x = T)

rm(modeldata,lag1,lag2,lag3,lag4,lag5,lag6,lag7,lag8)
modeldataLag$Row <- NULL
modeldataLag <- modeldataLag[-(1:9),]
#write.csv(modeldataLag,"modeldataLag.csv", row.names = F)
modeldataLag = read.csv("modeldataLag.csv")
test <- modeldataLag[(49:58),]
colnames(test) <- paste("Y",colnames(test),sep = "")

all_corr <- cor(modeldataLag)
all_corr[abs(all_corr) < 0.33] <- NA
all_corr <- all_corr[(1:numtk),-(1:numtk)]
all_corr <- melt(all_corr)
all_corr <- all_corr[!is.na(all_corr$value),]
all_corr <- all_corr[all_corr$value<1,]


#Model Creation

data = modeldataLag[1:48,]
test = tdata
colnames(test) <- paste("Y",colnames(test),sep = "")

options(warn = -1)
model_creation <- function(stock){
  vars = as.character(all_corr$X2[all_corr$X1==stock])
  data1 = data[,vars]
  data2 = data[,stock]
  X = as.matrix(data2)
  Y = as.matrix(data1)
  print(stock)
  model <- lm(X~Y)
  print(summary(model)$r.squared)
  test = test[,vars]
  colnames(test) = names(data1)
  prediction <- predict(model,test)
  actual <- modeldataLag[,stock][49:58]
  compare <- as.data.frame(cbind(actual,prediction))
  compare$value <- ifelse(sign(compare$actual)==sign(compare$prediction),TRUE,FALSE)
  print(table(compare$value))
}

for(i in 1:numtk){
  print(i)
  model_creation(tckk[i])
}

#Stocks to keep in mind : 1,2,5,6,7,11,12,15,16,24,27,29,31,41,43,44,46,48
shortlist_1 = c(1,2,5,6,7,11,12,15,16,24,27,29,31,41,43,44,46,48)

data = modeldataLag
options(warn = -1)
significant_vars <- function(stock){
  vars = as.character(all_corr$X2[all_corr$X1==stock])
  data1 = data[,vars]
  data2 = data[stock]
  X = as.matrix(data2)
  Y = as.matrix(data1)
  print(stock)
  model <- lm(X~Y)
  print(paste("The Company is",stock,"and the number of coefficients below .5% threshold are",length(summary(model)$coef[summary(model)$coef[,4] <= .005, 4]),sep = " "))
  print(summary(model)$coef[summary(model)$coef[,4] <= .05, 4])
}

for(i in 1:length(shortlist_1)){
  print(shortlist_1[i])
  significant_vars(tckk[shortlist_1[i]])
}

shortlist_2 = c(1,2,7,11,16,29,41,43,48)
  
for(i in 1:length(shortlist_2)){
  print(tckk[shortlist_2[i]])
}


#Creating new dataset

tdata <- modeldataLag

varnames <- colnames(tdata)
varname1 <- varnames[(numtk+1):(2*numtk)]
varname2 <- varnames[(numtk+1):length(varnames)]
nchar(varname2)
varname3 <- substr(varname2,1,nchar(varname2)-1)
last_char <- as.integer(substr(varname2,nchar(varname2),nchar(varname2)))
last_char <- last_char + 1
varname3 <- paste(varname3,last_char,sep = "")
varname4 <- c(varname1,varname3)
rm(varname3,varname2,varname1,varnames)

colnames(tdata) <- varname4

tdata <- tdata[nrow(tdata),]

#Prediction for Next Week

data = modeldataLag
test = tdata
colnames(test) <- paste("Y",colnames(test),sep = "")

model_creation <- function(stock){
  vars = as.character(all_corr$X2[all_corr$X1==stock])
  data1 = data[,vars]
  data2 = data[stock]
  data3 = cbind(data1,data2)
#  X = as.matrix(data2)
#  Y = as.matrix(data1)
  print(stock)
  Y = data3
  X = data3[,vars]
  model <- lm(Y ~ X)
  print(summary(model))
  model$coefficients
  test = test[,vars]
  colnames(test) = names(data1)
  colnames(test) <- paste("Y",colnames(test),sep = "")
  prediction <- predict(object = model, Y = test)
  actual <- modeldataLag[,stock][49:58]
  compare <- as.data.frame(cbind(actual,prediction))
  compare$value <- ifelse(sign(compare$actual)==sign(compare$prediction),TRUE,FALSE)
  print(table(compare$value))
}

options(warn = 1)
