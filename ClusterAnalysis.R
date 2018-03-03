

for(i in 1:numtk){
  remove_vars <- grep(tckk[i],colnames(modeldataLag))[-1]
  data <- modeldataLag[]
  X <- data[-(1:8),i]
  data <- data[-(1:8),]
  model_rf <- randomForest(X~.,data=data,importance = TRUE)
  imp <- as.data.frame(model_rf$importance)
  imp <- order(-model_rf$importance[,2])
  imp <- imp[1:10]
  data <- data[,imp]  
  model <- lm(X~. ,data = data)
  summary(model)
}

null <- lm(X~1,data = data)
full <- lm(X~.,data = data)
step(null,scope = list(lower = null, upper = full),direction = "forward")

model_new <- lm(formula = X ~ SHREECEM.NS + OIL.NS_LAG_4 + CUMMINSIND.NS_LAG_6 + 
     INDIGO.NS + PEL.NS + CASTROLIND.NS_LAG_2 + PEL.NS_LAG_5 + 
     ABB.NS_LAG_6 + LICHSGFIN.NS + MARICO.NS_LAG_6 + VEDL.NS + 
     SIEMENS.NS + NHPC.NS + SRTRANSFIN.NS_LAG_4 + PFC.NS_LAG_8 + 
     VEDL.NS_LAG_1 + BHARATFORG.NS_LAG_7 + SIEMENS.NS_LAG_4 + 
     BRITANNIA.NS_LAG_4 + GODREJCP.NS_LAG_2 + PGHH.NS_LAG_4 + 
     LICHSGFIN.NS_LAG_4 + HINDPETRO.NS_LAG_3 + PGHH.NS_LAG_7 + 
     INDIGO.NS_LAG_1 + NMDC.NS_LAG_8 + CUMMINSIND.NS_LAG_4 + JSWSTEEL.NS_LAG_3 + 
     MARICO.NS_LAG_1 + SAIL.NS_LAG_4 + TITAN.NS + TORNTPHARM.NS + 
     MOTHERSUMI.NS_LAG_8 + HINDPETRO.NS_LAG_2 + UPL.NS_LAG_5 + 
     GLENMARK.NS + OIL.NS_LAG_3 + HINDZINC.NS + ABB.NS_LAG_5 + 
     BAJFINANCE.NS + PNB.NS + LICHSGFIN.NS_LAG_7 + MOTHERSUMI.NS_LAG_2 + 
     APOLLOHOSP.NS, data = data)
summary(model_new)

correlation <- cor(modeldataLag[-(1:8),])
write.csv(correlation,"correlation.csv")

text = "ABB.NS"
for(i in 2:ncol(data)){
  text = paste(text,colnames(data)[i],sep = " + ")
}


summary(model <- lm(IOC.NS~PFC.NS_LAG_8 + PEL.NS_LAG_7 + BAJFINANCE.NS_LAG_4 ,data = data))
