  # Multiple Linear Regression on Insurance data

  library(tidyverse)
  dataset=read.csv("heart.data.csv")

  #Explore the dataset
  view(dataset)
  glimpse(dataset)
  length(dataset)
  names(dataset)
  summary(dataset)

  #Missing values
  colSums(is.na(dataset))

  #Filling missing values in biking
  ggplot(data=dataset,
         aes(biking))+
    geom_histogram()

  bike__median=median(dataset$biking, na.rm = TRUE)
  dataset$biking = ifelse(is.na(dataset$biking),
                     bike__median,
                     dataset$biking)
  
  #Filling missing values in smoking
  ggplot(data=dataset,
         aes(smoking))+
    geom_histogram()
  
  smoke_median=median(dataset$smoking, na.rm = TRUE)
  dataset$smoking = ifelse(is.na(dataset$smoking),
                          smoke_median,
                          dataset$smoking)
  
  #Filling missing values in heart.disease
  ggplot(data=dataset,
         aes(heart.disease))+
    geom_histogram()
  
  hd_mean=median(dataset$heart.disease, na.rm = TRUE)
  dataset$heart.disease = ifelse(is.na(dataset$heart.disease),
                          hd_mean,
                          dataset$heart.disease)
  
  colSums(is.na(dataset))

  
  #Splitting the data into training and testing sets
  library(caTools)
  set.seed(100)
  split=sample.split(dataset$heart.disease, SplitRatio = 0.8) #80% training 20% testing
  training_set=subset(dataset,split=TRUE)
  test_set=subset(dataset,split=FALSE)
  
  
  #Multiple linear regression training
  names(dataset)
  MLR=lm(formula=heart.disease~.,
         data=training_set)
  summary(MLR)
  
  # 14.958560 - 0.200119*(biking) + 0.179512*(smoking)
  
  #Biking and smoking are statistically significant variables.
  #If the p value of a variable is >.05, the variable claimed as not statistically significant
  
  #Mean square error
  summ=summary(MLR)
  MSE=(mean(summ$residuals^2))
  paste("Mean squared error", MSE)
  
  #R-square = 97.64% from summary
  #Because the R-square value is so high, we will be able to predict to a high level of 
  # accuracy from this model.
  summary(MLR)
  #How much a variation of a dependent variable is explained by independent variables
  #Higher value means good model
  #This model R square value is 97.64% and that means its a good model
  # A value less than 50% it is not a good model
  # A value less than 30% is is a poor model
  
  
  #Testing set prediction
  y_pred=predict(MLR,newdata = test_set)
  data=data.frame(test_set$heart.disease,y_pred)
  head(data)
  
  #Validation
  new=read.csv("Heart_validation.csv")
  new_x=new[c(1:2)]
  new_x
  data.frame(new[c(3)], predict(MLR,newdata=new_x))
  
  #Through this validation we can see that our model predicted values that are very close to the actual.