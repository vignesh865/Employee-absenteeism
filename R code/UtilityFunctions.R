#Generates histogram and boxplot and saves in the given path 
generateOutlierImage = function(length, cnames, data, nameSuffix, imagePath, x_axis ){
  
  setwd(imagePath)
  
  for(index in 1:length){
    print(cnames[index])
    assign(paste0("plot",index), ggplot(aes_string(y=(cnames[index]), x=x_axis),data = data)
           +stat_boxplot(geom = "errorbar",width = 0.5)
           +geom_boxplot(outlier.color = "red", fill="grey", outlier.shape = 18, outlier.size = 1, notch = F)
           +theme(legend.position = "bottom")+labs(y=cnames[index], x=x_axis)
    );
    
    assign(paste0("histogram",index), 
           ggplot(data, aes_string(x=cnames[index]))+
             geom_histogram(fill="DarkSlateBlue",colour = "black") + geom_density() + theme_bw()+ 
             xlab(cnames[index])+ylab("Freq")+
             scale_x_continuous(breaks = pretty_breaks(n=6))+
             scale_y_continuous(breaks = pretty_breaks(n=10)) +
             theme(text = element_text(size = 15))
    );
    ggsave(paste0(cnames[index],nameSuffix),plot = gridExtra::grid.arrange(get(paste0("histogram",index)),get(paste0("plot",index)), ncol=2), device = NULL, height=9,width=12,dpi=72)  
  }
}
#This method assign levels to the factors
assignLevels = function(data){
  for(i in 1:ncol(data)){
    if(class(data[,i])=="factor"){
      data[,i]=factor(data[,i],labels = (1:length(levels(data[,i]))))
    }
  }
  
  return(data)
}

#Replace NA values with median value
medianImputation = function(data, cnames){
  for (nnames in cnames){
    data = internalImputation(data, nnames, median(data[, nnames]))
  }
  return(data)
}

#Replace NA values with mean value
meanImputation = function(data, cnames){
  for (nnames in cnames){
   data = internalImputation(data, nnames, mean(data[, nnames]))
  }
  return(data)
}

#Helper method for mean and median imputation
internalImputation = function(data,nnames, replacementValue){
    val = boxplot.stats(data[, nnames])$out
    data[data[,nnames] %in% val,nnames]= replacementValue
    return(data)
}

#Remove given features from data frame
removeFeatures = function(data, varaibleList){
  for (variable in varaibleList) {
    data[variable] = NULL
  }
  
  return(data)
}

#Feature scaling standardization
standardizeData = function(data, numericNames){
  #numericNames = sapply(data, is.numeric)
  numeric_data = data[, numericNames]
  for (colname in colnames(numeric_data)) {
    data[,colname] = (data[,colname] - min(data[,colname]))/(max(data[,colname])-min(data[,colname]))
  }
  return(data)
}

standardizeVector = function(data){
    data = (data - min(data))/(max(data)-min(data))
    return(data)
}

#Feature scaling normalization
normalizeData = function(data, numericNames){
  #numericNames = sapply(data, is.numeric)
  numeric_data = data[, numericNames]
  print(numeric_data)
  print(colnames(numeric_data))
  for (i in colnames(numeric_data)) {
    data[,i]=(data[,i]-mean(data[,i]))/(sd(data[,i]))
  }
  return(data)
}




imputeMissingValues = function(Data, strategy){
  numericNames=colnames(Data[, sapply(Data, is.numeric)])
  print(numericNames)
  for (num_col in numericNames){
    Data[is.na(Data[num_col]), num_col]=median(Data[,num_col],na.rm = T)
  }
  
  factorNames=colnames(Data[, sapply(Data, is.factor)])
  for (fact_col in factorNames){
    Data[is.na(Data[fact_col]), fact_col]=Mode(Data[,fact_col])
  }
  return(Data)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  require(fmsb)
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}
