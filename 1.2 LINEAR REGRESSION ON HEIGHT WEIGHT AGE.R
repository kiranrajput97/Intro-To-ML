
# CALLING LIBRARIES
library(missForest)

# GETTING DATA
{
HEIGHT=c(5,5.11,5.6,5.9,4.8,5.8,5.3,5.8,5.5,5.6)
AGE=c(45,26,30,34,40,36,19,28,23,32)
WEIGHT=c(77,78,55,88,50,78,40,70,45,58)
DATA=data.frame(HEIGHT,AGE,WEIGHT)
}


# DATA VIEWING
str(DATA)
summary(DATA)
is.na(DATA)
(DATA[!complete.cases(DATA),])
cor(DATA)


# CONVERTING TO FACTORS
{
  data=DATA
  count=0
  for(i in 1:ncol(data))
  {
    if((class(data[,i])=='numeric')||(class(data[,i])=='integer'))
    {
      if((min(data[,i],na.rm=T))==quantile(data[,i],0.25,na.rm=T)||quantile(data[,i],0.25,na.rm=T)==quantile(data[,i],0.5,na.rm=T)||quantile(data[,i],0.5,na.rm=T)==quantile(data[,i],0.75,na.rm=T)||quantile(data[,i],0.75,na.rm=T)==quantile(data[,i],1,na.rm=T))
      {
        data[,i]=factor(data[,i])
        print(paste(names(data[i]),": CHANGED TO FACTOR"))
        count=count+1
      }else{
        data[,i]=data[,i]
        print(paste(names(data[i]),": IS REAL NUMBER"))
      }
    }else if((class(data[,i])=='factor')){
      print("ALREADY IN FACTOR")
    }else{
      print("NOT NUMERIC")
    }
  }
  print(paste("**** NO. OF ATTRIBUTES ARE COVERTED TO FACTOR:",count ,"****"))
  DATA=data
}


# DIMENSIONALITY REDUCTION
{
summary(DATA)
DATACLEANED=DATA
}


# IMPUTING OUTLIERS AND FEEDING NAs TO THEM
{
{
  OUTLIERS=NULL
  for (i in 1:ncol(DATACLEANED))
  {
    if(class(DATACLEANED[,i])=='numeric'||class(DATACLEANED[,i])=='integer')
    {
      if (length((boxplot(DATACLEANED[,i])$out))==0)
      {
        print ('NO OUTLIERS')
      }else {
        print ('OUTLIERS')
        OUTLIERS=boxplot(DATACLEANED[,i], plot=FALSE)$out
        DATACLEANED[which(DATACLEANED[,i] %in% OUTLIERS),i]=NA
        OUTLIERS=NULL
      }
    }else{
      print ("NOT NUMERIC")
    }
  }
}
summary(DATACLEANED)
DATANOOUTLIER=DATACLEANED
}


# REMOVING NA
{
{
  if (nrow(DATACLEANED[!complete.cases(DATACLEANED),])==0)
  {
    DATANONA=DATACLEANED
  }else{
    DATAMISSFOREST <- missForest(DATACLEANED)
    DATANONA=as.data.frame(DATAMISSFOREST[[1]])
  }
}
summary(DATANONA)
}


# NORMALIZE DATASET
{
DATANORM=DATANONA
summary(DATANORM)
}


# DATA SUBSETTING AND PREPARATION
set.seed(1234)
ind=sample(2,nrow(DATANORM),replace=T,prob=c(0.7,0.3))
TRAINING=DATANORM[ind==1,]
TESTING=DATANORM[ind==2,]
str(TRAINING)
summary(TRAINING)
str(TESTING)
summary(TESTING)


# CREATING MULTIPLE LINEAR REGRESSION MODEL
{
MODEL=lm(WEIGHT~.,data=TRAINING)
summary(MODEL)
}


# MAKING PLANE FOR PLOTTING
par(mfrow=c(1,2))


# PLOTTING MODEL ON BEHALF OF HEIGHT 
{
MODEL1=lm(WEIGHT~HEIGHT,data=TRAINING)
p=plot(DATA$HEIGHT,DATA$WEIGHT,main='MODEL FOR HEIGHT')
abline(MODEL1,col='red')
}


# PLOTTING MODEL ON BEHALF OF AGE 
{
MODEL2=lm(WEIGHT~AGE,data=TRAINING)
p=plot(DATA$AGE,DATA$WEIGHT,main='MODEL FOR AGE')
abline(MODEL2,col='red')
}


# PREDICTION AND ACCURACY ON BEHALF OF BOTH COLUMNS
{
PREDICTION=(predict(MODEL,TESTING))
DIFFERENCE=abs(TESTING[,3]-PREDICTION)
ERROR_PERCENTAGE=DIFFERENCE/TESTING[,3]*100
ACCURACY_PERCENTAGE=100-ERROR_PERCENTAGE
(cbind(ACTUAL=TESTING[,3],PREDICTED=PREDICTION,ERROR=DIFFERENCE,ERROR_PERCENTAGE,ACCURACY_PERCENTAGE))
(FINAL_ACCURACY=mean(ACCURACY_PERCENTAGE))
}


# PREDICTION AND ACCURACY ON BEHALF OF HEIGHT
{
PREDICTION1=(predict(MODEL1,TESTING))
DIFFERENCE1=abs(TESTING[,3]-PREDICTION1)
ERROR_PERCENTAGE1=DIFFERENCE1/TESTING[,3]*100
ACCURACY_PERCENTAGE1=100-ERROR_PERCENTAGE1
(cbind(ACTUAL=TESTING[,3],PREDICTED=PREDICTION1,ERROR=DIFFERENCE1,ERROR_PERCENTAGE1,ACCURACY_PERCENTAGE1))
(FINAL_ACCURACY1=mean(ACCURACY_PERCENTAGE1))
}


# PREDICTION AND ACCURACY ON BEHALF OF AGE
{
PREDICTION2=(predict(MODEL2,TESTING))
DIFFERENCE2=abs(TESTING[,3]-PREDICTION2)
ERROR_PERCENTAGE2=DIFFERENCE2/TESTING[,3]*100
ACCURACY_PERCENTAGE2=100-ERROR_PERCENTAGE2
(cbind(ACTUAL=TESTING[,3],PREDICTED=PREDICTION2,ERROR=DIFFERENCE2,ERROR_PERCENTAGE2,ACCURACY_PERCENTAGE2))
(FINAL_ACCURACY2=mean(ACCURACY_PERCENTAGE2))
}


ACCURACIES=c(FINAL_ACCURACY,FINAL_ACCURACY1,FINAL_ACCURACY2)
NAMES_MODEL=c('MODEL','MODEL1','MODEL2')
(ACCURACIES_COLUMNS=data.frame(NAMES_MODEL,ACCURACIES))


# PREDICTING THE VALUE
{
CHECK=data.frame(HEIGHT=6.5,AGE=45)
(PREDICTED_VALUE=predict(MODEL2,CHECK))
}

paste("If HEIGHT:",CHECK$HEIGHT,"and AGE:",CHECK$AGE,"then we have",round(FINAL_ACCURACY2),"% chances of getting PREDICTED WEIGHT as:",round(PREDICTED_VALUE))
 
