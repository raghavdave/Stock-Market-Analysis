rm(list = ls())

install.packages("quantmod")
install.packages("plyr")
install.packages("forecast")
install.packages("lubridate")
library(data.table)
library(lubridate)
library(quantmod)
library(plyr)
library(tseries)  
library(forecast)

tckk <-c("TCOR.NS")# ticker names defined  
numtk <-length(tckk);  
ustart <-"2016-12-25";uend <-"2017-05-02" #start and end date  
all_dat<-list()#empty list to fill in the data  
for(i in 1:numtk)  
{  
  all_dat[[i]]<-xxx<-get.hist.quote(instrument = tckk[i], start=ustart, end=uend,quote = c("Open", "High", "Low", "Close"),provider = "yahoo", compression = "d")  
}   

data = do.call(rbind, lapply(all_dat, data.frame, stringsAsFactors=FALSE))

#summary report
yearly_return1 <- yearlyReturn(data)
yearly_return <- yearlyReturn(data,leading = FALSE)
monthly_return <- monthlyReturn(data, leading = FALSE)
#Weekly return is the %change in Monday closing prices.
weekly_return <- weeklyReturn(data, leading = FALSE)
dailyReturn(data)
#build model
q.model = specifyModel(Next(Cl(INFY.BO)) ~ Lag(OHL(INFY.BO),0:3))
model <- buildModel(q.model,method = "glm",training.per = c('2016-12-30','2017-03-24'))

#creation of variables
Opc
  a <- ncol(data)
  for (i in 1:(a-1)){
    for (j in (i+1):a){
      name <- paste(colnames(data)[j],colnames(data)[i],"Ratio",sep = "_")
      data[name] <- data[j]/data[i]
      name1 <- paste(colnames(data)[j],colnames(data)[i],"Diff",sep = "_")
      data[name1] <- data[j] - data[i]
    }
  }
data$PercentChange <- (data$Close-data$Open)/data$Open*100
data$High_Close <- ifelse(abs((data$High-data$Close)/data$Close) <0.005,"True","False")
data$Low_Close <- ifelse(abs((data$Low-data$Close)/data$Close) <0.005,"True","False")

setDT(data, keep.rownames = TRUE)[]
data$rn <- as.Date(data$rn)
yearlyReturn(data)
weeklyReturn(data)
