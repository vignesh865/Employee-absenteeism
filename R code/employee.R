
rm(list=ls())

setwd("G://ml//edwiser//employee absentesm/")

x=c("ggplot2","corrgram","DMwR","caret","randomForest","unbalanced","C50","dummies","e1071","MASS","rpart","gbm","ROSE","scales", "readxl", "car", "Metrics", "usdm", "ggbiplot", "Metrics", "pls")


lapply(x,require,character.only=TRUE)

rm(x)
source("UtilityFunctions.R")


Data = as.data.frame(read_excel("Absenteeism_at_work_Project.xls",  sheet = "Absenteeism_at_work"))
Data['Year'] = 0
Data[0:114,'Year'] = 2008
Data[114:359,'Year'] = 2009
Data[359:571,'Year'] = 2010
Data[571:740,'Year'] = 2011
Data[, 'Month_for_aggregation'] = Data[, 'Month of absence']

#Change column name - Fill spaces with underscore
index = 1
for (col in colnames(Data)){
  new_name=NULL
  name_splitted = trimws(unlist(strsplit(col, " ")))
  for (i in name_splitted){
    new_name=  paste(new_name, i, sep = "_")
  }
  print(new_name)  
  colnames(Data)[index] = c(substring(new_name, 2))
  index = index+1
}

colnames(Data)[10] = 'Work_load_Average_day'
str(Data)

#Change to appropriate category
Data$`Reason_for_absence` = as.factor(Data$Reason_for_absence)
Data$Month_of_absence = as.factor(Data$Month_of_absence)
Data$Day_of_the_week = as.factor(Data$Day_of_the_week)
Data$Seasons = as.factor(Data$Seasons)
Data$Disciplinary_failure = as.factor(Data$Disciplinary_failure)
Data$Education = as.factor(Data$Education)
Data$Son = as.factor(Data$Son)
Data$Social_drinker = as.factor(Data$Social_drinker)
Data$Social_smoker = as.factor(Data$Social_smoker)
Data$Pet = as.factor(Data$Pet)

summary(Data)



#Missing Value
missingValues=data.frame(apply(Data, 2,function(x){sum(is.na(x))}))
colnames(missingValues) = "Missing value"

#Missing value treatment
Data[67, "Month_of_absence"] = 10
Data[67, "Month_for_aggregation"] = 10

Data = imputeMissingValues(Data, cnames)

train = Data[0:570,]
test = Data[571:737,] 

#Outlier analysis
cnames=colnames(Data[, sapply(Data, is.numeric)])
#Outlier analysis - by column

generateOutlierImage(length(Data), cnames, Data, "With outlier.png", "G://ml//edwiser//employee absentesm//with_outlier","Absenteeism_time_in_hours" )

train = medianImputation(train, cnames)
generateOutlierImage(length(Data), cnames, Data, "Without outlier.jpg", "G://ml//edwiser//employee absentesm//without_outlier","Absenteeism_time_in_hours")

#Outlier analysis - by observation


model = lm(Absenteeism_time_in_hours ~ ., data = train)
cooksd = cooks.distance(model)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line

outlers = (outlierTest(model = model))

train = train[c(-309,-399,-490,-385,-566),]


#Feature Selection
corrgram(train[,cnames],order = F,upper.panel = panel.pie, text.panel = panel.txt,  main = "Coorelation plot")
train = removeFeatures(train, c("Service_time", "Age", "Weight", "Body_mass_index"))
test = removeFeatures(test, c("Service_time", "Age", "Weight", "Body_mass_index"))



RandomF_Model = randomForest(Absenteeism_time_in_hours ~., data = train, importance = TRUE,ntree = 100)

imp = as.data.frame(varImp(RandomF_Model))
imp['Variables'] = rownames(imp)
imp=imp[order(-imp[,1]),]

vif_imp = as.data.frame(vif(train[,-17]))
vif_imp = vif_imp[order(-vif_imp[,2]),]

fnames=colnames(Data[, sapply(Data, is.factor)])
ls = c()
count = 1
for (i in fnames){
  print(i)
  ls[count]=anovaScores( train$Absenteeism_time_in_hours, train[,i])
  count = count+1
}
anovaa = as.data.frame(ls, row.names = fnames)
anovaa = as.data.frame(anovaa[order(anovaa[, 1]),], row.names = fnames)

train = removeFeatures(train, c("Son", "Social_drinker", "Social_smoker", "Pet"))
test = removeFeatures(test, c("Son", "Social_drinker", "Social_smoker", "Pet"))


Data_dum = rbind(train,test);
Data_dum$`Reason_for_absence` = NULL
Data_dum$Month_of_absence = NULL
Data_dum$Day_of_the_week = NULL
Data_dum$Seasons = NULL
Data_dum$Disciplinary_failure = NULL
Data_dum$Education = NULL

dummy_data = as.data.frame(model.matrix(~ Reason_for_absence + Month_of_absence + Day_of_the_week + Seasons + Disciplinary_failure + Education + 0, data = rbind(train,test)))
Data_dum = cbind(Data_dum, dummy_data)

backup = Data_dum


pca = prcomp(Data_dum[, -7], center = TRUE, scale. = TRUE)
str(pca)
pca_summary = (summary(pca))
pca_summary  = as.data.frame(pca_summary$sdev)
colnames(pca_summary)[1] = "Variance"
pca_summary["pca"] = as.numeric(rownames(pca_summary))


ggplot(pca_summary, aes_string(y="Variance", x="pca")) +
  geom_line() + theme_bw() + 
  ylab("Variance")+xlab("pca")+
  scale_x_continuous(breaks = pretty_breaks(n=10))+
  ggtitle("pca")+
  theme(text = element_text(size = 15))

str(pca_summary)

#Feature Scaling

train = Data_dum[0:565,c(-8, -9)]
test = Data_dum[566:732,c(-8, -9)] 
train = standardizeData(train, c('ID', 'Transportation_expense', 'Distance_from_Residence_to_Work', 'Work_load_Average_day', 'Hit_target', 'Height', 'Absenteeism_time_in_hours'))
test = standardizeData(test, c('ID', 'Transportation_expense', 'Distance_from_Residence_to_Work', 'Work_load_Average_day', 'Hit_target', 'Height', 'Absenteeism_time_in_hours'))

#run  regression model
base = lm(Absenteeism_time_in_hours ~., data = train)

optimized_fitt <- step(base)
pred_lr = predict(optimized_fitt, test[,-7])


summary(optimized_fitt)
summary(base)


#Regression using PCA
fit = pcr(Absenteeism_time_in_hours ~., data = train, validation="CV")
pred_lrp = predict(fit, test[,-7], ncomp = 50)
summary(fit)


#Ranfom forest regression
fit = rpart(Absenteeism_time_in_hours ~ ., data = train, method = "anova")
pred_rf = predict(fit, test[,-7])

summary(fit)


RMSE(test[,7], pred_rf)
MAE(test[,7], pred_rf)

train = Data_dum[0:565,c(7,8,9)]
test = Data_dum[566:732,c(7,8,9)]

train$Absenteeism_time_in_hours = standardizeVector(train$Absenteeism_time_in_hours) 
test$Absenteeism_time_in_hours = standardizeVector(test$Absenteeism_time_in_hours) 

test_lr = Data_dum[566:732,c(7,8,9)] 
test_lrp = Data_dum[566:732,c(7,8,9)] 
test_rf = Data_dum[566:732,c(7,8,9)] 

test_lr$Absenteeism_time_in_hours = pred_lr
test_lrp$Absenteeism_time_in_hours = pred_lrp
test_rf$Absenteeism_time_in_hours = pred_rf

trainAggregate = aggregate(. ~ Month_for_aggregation + Year, train, FUN = mean)
testAggregate = aggregate(. ~ Month_for_aggregation + Year, test, FUN = mean)

testAggregate_lr = aggregate(. ~ Month_for_aggregation + Year, test_lr, FUN = mean)
testAggregate_lrp = aggregate(. ~ Month_for_aggregation + Year, test_lrp, FUN = mean)
testAggregate_rf = aggregate(. ~ Month_for_aggregation + Year, test_rf, FUN = mean)



trainAggregateTsData = ts(trainAggregate$Absenteeism_time_in_hours, start = c(2008,7), frequency = 12)
testAggregateTsData = ts(testAggregate$Absenteeism_time_in_hours, start = c(2011,1), frequency = 12)

testAggregateLrTsData = ts(testAggregate_lr$Absenteeism_time_in_hours, start = c(2011,1), frequency = 12)
testAggregateLrpTsData = ts(testAggregate_lrp$Absenteeism_time_in_hours, start = c(2011,1), frequency = 12)
testAggregateRfTsData = ts(testAggregate_rf$Absenteeism_time_in_hours, start = c(2011,1), frequency = 12)

ts.plot(trainAggregateTsData, testAggregateTsData, testAggregateLrTsData,testAggregateLrpTsData, 
        testAggregateRfTsData,gpars = list(col = c("black","green", "red", "blue", "yellow")))


#Arima

plot.ts(testAggregateTsData)
plot.ts(testAggregateLrTsData)
plot.ts(testAggregateLrpTsData)
plot.ts(testAggregateRfTsData)

comp = decompose(trainAggregateTsData)
plot.ts(comp$trend)



arim = auto.arima(trainAggregateTsData, trace=TRUE)
fcast_cons <- forecast(arim, h = 12)
autoplot(fcast_cons)


as = model.matrix(~ ID + Transportation_expense + Height + Disciplinary_failure1, data = Data_dum[0:565,])

rg = model.matrix(~ ID + Transportation_expense + Height + Disciplinary_failure1, data = Data_dum[566:732,])

arim = auto.arima(train$Absenteeism_time_in_hours, trace=TRUE, xreg = as)
fcast_cons <- forecast(arim, h = 16, xreg = rg)
autoplot(fcast_cons)

forecast::accuracy(test$Absenteeism_time_in_hours, fcast_cons$mean)

for (i in 1:51){
  if (sum(dummy_data[0:565,i] == 0)==0){
    print(i)
  }
}
sum(dummy_data[0:565,i] == 0)!=0
