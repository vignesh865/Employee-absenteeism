Data_arima = as.data.frame(read_excel("Absenteeism_at_work_Project.xls",  sheet = "Absenteeism_at_work"))

#Arima
Data_arima['Year'] = 0
Data_arima[0:114,'Year'] = 2008
Data_arima[114:359,'Year'] = 2009
Data_arima[359:571,'Year'] = 2010
Data_arima[571:740,'Year'] = 2011


index = 1
for (col in colnames(Data_arima)){
  new_name=NULL
  name_splitted = trimws(unlist(strsplit(col, " ")))
  for (i in name_splitted){
    new_name=  paste(new_name, i, sep = "_")
  }
  print(new_name)  
  colnames(Data_arima)[index] = c(substring(new_name, 2))
  index = index+1
}


#Error correction
Data_arima[570, 'Month_of_absence'] = 2
Data_arima[738:740, 'Month_of_absence'] = 1
Data_arima[738:740, 'Year'] = 2011



RawData = aggregate(. ~ Month_of_absence + Year, Data_arima, FUN = aggg)
decompose(RawData)

fnames=colnames(Data_arima[, sapply(Data_arima, is.factor)])
cnames=colnames(Data_arima[, sapply(Data_arima, is.numeric)])
count = 38
i = "Reason_for_absence"
for (i in fnames){
  RawData[1:count,paste(i,'agg')] = 0
  for (row in 1:count){
    RawData[row,paste(i,'agg')] = as.numeric(Mode(unlist(RawData[row,i])))
  }
  RawData[,i] = as.factor(RawData[,paste(i,'agg')])  
  RawData[,paste(i,'agg')] = NULL
}

for (i in cnames){
  for (row in 1:count){
    RawData[row,i] =as.numeric(mean(unlist(RawData[row,i])))
  }
  RawData[,i] = as.numeric(RawData[,i])  
}

RawData['ID'] = NULL

aggg = function(x){
  return(x)
}

Data_dum = RawData;
Data_dum$`Reason_for_absence` = NULL
Data_dum$Month_of_absence = NULL
Data_dum$Day_of_the_week = NULL
Data_dum$Seasons = NULL
#Data_dum$Disciplinary_failure = NULL
#Data_dum$Education = NULL
Data_dum$Son = NULL
Data_dum$Social_drinker = NULL
#Data_dum$Social_smoker = NULL
Data_dum$Pet = NULL
#dummy_data = as.data.frame(model.matrix(~ Reason_for_absence + Month_of_absence + Day_of_the_week + Seasons + Disciplinary_failure + Education + Son + Social_drinker + Social_smoker + Pet + 0, data = RawData))
dummy_data = as.data.frame(model.matrix(~ Reason_for_absence + Month_of_absence + Day_of_the_week + Seasons + Son + Social_drinker + Pet + 0, data = RawData))
df = model.matrix(~ Reason_for_absence + Month_of_absence + Day_of_the_week + Seasons + Son + Social_drinker + Pet + 0, data = RawData)
Data_dum = cbind(Data_dum, dummy_data)





train = Data_dum[0:30, ]
test =  Data_dum[31:37, ]
tsData = ts(train$Absenteeism_time_in_hours, start = c(2007,7), frequency = 12)

plot.ts(tsData)
components = decompose(tsData)
plot(components)

library("fUnitRoots")
urkpssTest(tsData, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(tsData, differences=1)
plot(tsstationary)
library("forecast")
ndiffs(tsData, alpha = 0.1, test = "kpss")

#autocorrelation
acf(tsData,lag.max=34)

#Removing seasonal
timeseriesseasonallyadjusted <- tsData- components$seasonal
tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)

acf(tsstationary, lag.max=34)
pacf(tsstationary, lag.max=34)


arim =arima(tsData, order = c(0,0,1))
library("lmtest")
coeftest(arim) 
confint(arim)

arim

tsVariables = as.matrix(sapply(train[,c(-9,-14, -19, -20, -48)], as.numeric))
tsVariables = as.matrix(RawData)
sum(is.na(ass))

ass = model.matrix(~ Reason_for_absence + Month_of_absence + Day_of_the_week + Seasons + Son + Social_drinker + Pet + 0, data = RawData)
arim = auto.arima(Data_arima$Absenteeism_time_in_hours, trace=TRUE, xreg = ass)

#pred = as.data.frame(predict(arim, n.ahead = 7)$pred)

fcast_cons <- forecast(arim, h = 7)
autoplot(fcast_cons)

forecast::accuracy(test$Absenteeism_time_in_hours, fcast_cons$mean)


fcast_cons <-forecast(arim, xreg = test$Reason_for_absence, h = 7)
autoplot(fcast_cons)

forecast::accuracy(test$Absenteeism_time_in_hours, fcast_cons$mean)



mase(test$Absenteeism_time_in_hours, pred$x)
MAPE(test$Absenteeism_time_in_hours, pred$x)
mae(test$Absenteeism_time_in_hours, pred$x)
rmse(test$Absenteeism_time_in_hours, pred$x)
