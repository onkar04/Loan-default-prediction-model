## for numerical variables bin the data 

library("dplyr")

setwd("C:/Users/User/Desktop/R Codes/")
getwd()
data <- read.csv("germancredit.csv")

# Perform Basic eda

str(data)

# Convert datatypes of mismatching columns

data$Default<-as.factor(as.character(data$Default))
data$liable <-as.factor(as.character(data$liable ))
data$cards <-as.factor(as.character(data$cards))
data$residence<-as.factor(as.character(data$residence))
data$installment<-as.factor(as.character(data$installment))

# Missing map 
library(Amelia)
library(mlbench)
missmap(data, col=c("blue", "red"), legend=FALSE)

# Count of missing values
apply(is.na(data),2,sum)

# Check str again
str(data)

# Basic Inferences

prop.table(table(data$Default))


# Function to bin numerical columns into 10 deciles

get_bin<-function(data,column,target)
{
  df <- data.frame(matrix(ncol = 3, nrow = nrow(data)))
  x <- c("var","group","target")
  colnames(df) <- x

  df["var"]<-data[column]
  df["target"]<-data[target]
  
  decile<-c()
  
  for (i in 1:10)
    
  { decile<-append(decile,quantile(data[column],0.1*i,na.rm=TRUE))}

  df[df["var"]<=decile[1],]["group"]<-1
  
  for (i in 2:9)
    
  { try(df[(df["var"]<=decile[i])&(df["var"]>decile[i-1]),]["group"]<-i,silent=T) }
  
  
  df[df["var"]>decile[9],]["group"]<-10
  return(df)
 
}


# Function that return the information value of all predictor variables in a dataframe.

get_iv <-function (df,target)
{
  variable<-c()
  ivvalue<-c()
  
  numerics<-names(select_if(df, is.numeric))
  labels<-names(select_if(df, is.factor))
  
  mod<-c()
  
  for(i in labels)
  {
    if(i!=target)
    {
      mod<-append(mod,i)
    }
  }
  
  labels<-mod
  
  print("numeric :")
  print(numerics)
  
  print("labels :")
  print(labels)
  
  
  for (nums in numerics)
  {
  
    getdf<-get_bin(df,nums,target)

    
    woe <- data.frame(matrix(ncol = 5,nrow = 10))
    x <- c("bin","good", "bad", "woe","iv")
    colnames(woe) <- x
    
    woe["bin"]<-c(1:10)
    
    for (i in 1:10)
      
    {
      g<- ((nrow(getdf[(getdf["group"]==i)&(getdf["target"]==0),]))/sum(getdf["target"]==0))
      b<- ((nrow(getdf[(getdf["group"]==i)&(getdf["target"]==1),]))/sum(getdf["target"]==1))
      woe[i,"good"]<-g
      woe[i,"bad"]<- b
      woe[i,"woe"]<-log(g/b)
      woe[i,"iv"]<-(g-b)*log(g/b)
    }
    

    variable<-append(variable,nums)
    ivvalue<-append(ivvalue,sum(woe["iv"],na.rm = TRUE))
    
  }
  
  
  for (label in labels)
  {
    

    woe <- data.frame(matrix(ncol = 5,nrow = length(levels(as.factor(as.matrix(df[label]))))))
    x <- c("bin","good", "bad", "woe","iv")
    colnames(woe) <- x
    
    woe["bin"]<-levels(as.factor(as.matrix(df[label])))
    
    count<-1
    
    for (i in levels(as.factor(as.matrix(df[label]))))
      
    {
      
      # Non event is taken as good
      # Event is taken as bad
      
      g<- (nrow(df[(df[label]==i)&(df[target]==0),])/sum(df[target]==0))
      b<- (nrow(df[(df[label]==i)&(df[target]==1),])/sum(df[target]==1))
      
      woe[count,"good"]<-g
      woe[count,"bad"]<- b
      woe[count,"woe"]<-log(g/b)
      woe[count,"iv"]<-(g-b)*log(g/b)
      count<-count+1
      
    }
    

    variable<-append(variable,label)
    ivvalue<-append(ivvalue,sum(woe["iv"],na.rm = TRUE))
    
  }
  
  
  idf<-data.frame("variable" = variable, "information.value"=ivvalue)
  
  return(idf)
  
}

woe<-get_iv(data,"Default")

woe[order(-woe$information.value),]

woe$information.value<-as.numeric(woe$information.value)

variable<-c()
variable<-subset(woe,information.value>0.1)["variable"]
variable<-levels(as.factor(as.matrix(variable)))
variable<-append(variable,"Default")


#Train-Test Split (Performing a stratified sampling)
#-------------------------------------------------------------------------
library(caTools)
set.seed(88)
split <- sample.split(data$Default, SplitRatio = 0.75)

#split

#get training and test data
train <- subset(data, split == TRUE)
test  <- subset(data, split == FALSE)

#Checks
prop.table(table(data$Default))*100
prop.table(table(data$Default))*100
prop.table(table(data$Default))*100

# Building a logistic regression model using only those important variables as predictors

logreg <- glm(Default ~ ., data=train[,c(variable)], family="binomial")
summary(logreg)
 
variable[variable != "Default"]

pred = predict(logreg, newdata = test[,c(variable)], type = "response")

pred1 = ifelse(pred > 0.5, 1, 0)

#Confusion matrix
table(test$Default, pred1)

# Plotting ROC Curve
library(InformationValue)

plotroc<-function(pred1)
{
xaxis<-c()
yaxis<-c()

for (i in seq(0,1, by=0.1))
  {
  pred1 = ifelse(pred >= i, 1, 0)
  sensitivity(test$Death, pred1)
  xaxis<-append(xaxis,1-specificity(test$Default, pred1))
  yaxis<-append(yaxis,sensitivity(test$Default, pred1))
  }

plot(xaxis,yaxis,type="p",xlab="1-specificity",ylab="sensitivity")
plot(xaxis,yaxis,type="b")


}

plotroc(pred1)


