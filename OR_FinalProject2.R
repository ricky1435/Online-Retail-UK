library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(plotly)
library(readxl)
library(stats)
library(factoextra)
library(gridExtra)
library(ggpubr)
library(Hmisc)
set.seed(100)
# Function to set the working directory and read the data
read_data <- function()
{
  setwd('/Users/ruthvikpvs/Desktop/OR 568/Final Project')
  raw_data1 <- read_excel('online_retail_II.xlsx',sheet = 'Year 2009-2010')
  raw_data2 <- read_excel('online_retail_II.xlsx',sheet = 'Year 2010-2011')
  combined.raw.data <- rbind(raw_data1,raw_data2)
  return(combined.raw.data)
}
raw_data<-read_data()

# Copying the data into another variable
data <- raw_data

# Function for Checking Missing Values
check.missing <- function(x)
{
  return(colSums(is.na(x)))
  
}
check.missing(data)

data.preprocess <- function(data1)
{
  # Creating another column called TotalPrice
  data1$TotalPrice <- data1$Quantity * data1$Price
  
  # Removing Missing Values
  data1<-na.omit(data1)
  
  # Removing the Invoice rows that containts the letter C. C means this order was cancelled.
  data1<-data1[!grepl("C",data1$Invoice),]
  # Removing transactions with values above the 3rd Quantile 
  
  Q3 <- quantile(data1$TotalPrice, .75)
  IQR <- IQR(data1$TotalPrice)
  data1 <- subset(data1,data1$TotalPrice<= (Q3 + 1.5*IQR))
  return(data1)
}

data<-data.preprocess(data)
# Checking the distribution of Total Price
ggplot(data,aes(x=TotalPrice))+geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+labs(title='Distribution of Total Price')

# Number of customers in our Retail Store
length(unique(data$`Customer ID`))
# 5680 customers

# Total money spent on customer level
money.spent <- data %>%
  group_by(`Customer ID`) %>%
  summarise(Sum=sum(TotalPrice))
money.spent

# Creating a custom date
date1 = as.Date.character("25/12/2011","%d/%m/%Y")
max(data$InvoiceDate)

# Converting the dates in POISxt format to Date format
poix.to.date <- function(x)
{
  options(digits.secs=3)
  return(as.Date(as.POSIXct(x$InvoiceDate, 'GMT')))
}
data$InvoiceDate<-poix.to.date(data)

# Calculating the Recency, Frequency and Monetary Values
rfm.calc <- function(x)
{
  z<-x %>% group_by(`Customer ID`) %>% 
           summarise(Recency = as.numeric(date1- max(InvoiceDate)), 
              Frequency = n(), 
              Monetary = sum(TotalPrice),firstpurchace=min(InvoiceDate))
  return(z)
}

rfm.scores<-rfm.calc(data)
rfm.scores
ggplot(data=rfm.scores,aes(Recency))+geom_histogram()
ggplot(data=rfm.scores,aes(Frequency))+geom_histogram()
ggplot(data=rfm.scores,aes(Monetary))+geom_histogram()

scores <- function(){
      rfm.scores$rscore <- ntile(rfm.scores$Recency,5)
      rfm.scores$fscore <- ntile(rfm.scores$Frequency,5)
      rfm.scores$mscore <- ntile(rfm.scores$Monetary,5)
      rfm.scores <- rfm.scores %>%
                              mutate(scores = 100 *rscore +10 * fscore + mscore)
      # Assigning Clusters
      # https://www.geeksforgeeks.org/how-to-filter-r-dataframe-by-multiple-conditions/
      
      rfm.scores$Segment <- "0"
      rfm.scores$Segment[which(rfm.scores$scores %in% c(555,545,554,455,553,355,552,541,551,552,144,145,135,125,154,354,
                                                        345,255,542,452))] <-"Loyal"
      rfm.scores$Segment[which(rfm.scores$scores %in% c(443,444,453,454,445,513,514,515,541,543,
                                                        531,533,534,535,544,124,134,523,512,153,532,412,421))] <- "Potential Loyal"
      
      rfm.scores$Segment[which(rfm.scores$scores %in% c(344 ,521,345 ,352 ,422 ,423 ,424 ,425, 432 ,433 ,434 ,435, 431,
                                                        442 ,331,441,142,342,343))] <- "Repeat"
      
      
      rfm.scores$Segment[which(rfm.scores$scores %in% c(235 ,321,244,245 ,253, 254 , 325, 335,  353   ,511,522,113,231,411,
                                                        222,121,322,312,311,223))] <- "Non Repeat"
      rfm.scores$Segment[which(rfm.scores$scores %in% c(232,233 ,234 ,242 ,243 ,252, 323 ,324 ,332 ,333 ,334 ,122,221 ,
                                                        144,155,133,213,111,112,211,212,122,131,123,132,141,143))] <- "Churning"
      
      return(rfm.scores)
      
}
rfm.scores<- scores()
rfm.scores

#Classification: 

## Split the data into training (80%) and test sets (20%)
set.seed(100)
inTrain <- createDataPartition(rfm.scores$Segment, p = .8)[[1]]
rfmTrain <- rfm.scores[ inTrain, c(2,3,4,5,10)]
rfmTest  <- rfm.scores[-inTrain, c(2,3,4,5,10)]
rfmTrain
# Fit SVM model
library(kernlab)
#set.seed(200)
#sigmaRangeFull <- sigest(as.matrix(rfmTrain[,-4]))
#svmRGridFull <- expand.grid(sigma =  as.vector(sigmaRangeFull)[1],
#                            C = 2^(-1:30))

set.seed(100)
svmModel = train(Segment ~ Recency+Frequency+Monetary, data = rfmTrain, 
                 method ="svmRadial", 
                 metric ="accuracy",
                 preProc=c("center", "scale"),
                 tuneLength = 30,
                 #tuneGrid = svmRGridFull,
                 trControl = trainControl(method = "cv",number =10))

svmModel$bestTune
# Predict on test data
svmRPred = predict(svmModel, newdata=rfmTest[-4])
svmPR = confusionMatrix(data=svmRPred, reference=factor(rfmTest$Segment)) 
svmPR

# Fit Decision tree
set.seed(100)
rfm.tree = train(Segment ~ Recency+Frequency+Monetary, 
                 data=rfmTrain, 
                 method="rpart", 
                 metric="accuracy",
                 tuneLength = 30,
                 trControl = trainControl(method = "cv", number=10))
rfm.tree

# Predict on test data
Treepred= predict(rfm.tree, newdata=rfmTest[-4])
treePR=confusionMatrix(data=rfm.tree, reference=factor(rfmTest$Segment)) 
treePR



library(partykit)
TreePlot = as.party(rfm.tree$finalModel)
plot(TreePlot)

# Fit Random forest
library(randomForest)
rfmTrain$Segment <- as.factor(rfmTrain$Segment)
tunegrid <- expand.grid(.mtry=c(1:10))
set.seed(100)
rfm.rf <- train(Segment~Recency+Frequency+Monetary, 
                data=rfmTrain,
               method="rf", 
               metric="Accuracy", 
               #tuneLength=15, 
               tuneGrid = tunegrid,
               ntree = 1000,
               trControl=trainControl(method = "cv",number =10, search = "grid"))
rfm.rf

# Predict on test data
Rfpred= predict(rfm.rf, newdata=rfmTest[-4])
RfPR=confusionMatrix(data=rfm.rf, reference=rfmTest$Segment) 
RfPR

# Linear Descriminant Analysis
set.seed(476)
ldaFit <- train(Segment~Recency+Frequency+Monetary, 
                data=rfmTrain, 
                method = "lda",
                preProc = c("center","scale"),
                metric = "Accuracy",
                trControl = trainControl(method = "cv",number =10))
ldaFit

lda_pred= predict(ldaFit, newdata=rfmTest[-4])
ldaFit$pred <- merge(ldaFit$pred,  ldaFit$bestTune)
ldaCM <- confusionMatrix(ldaFit, reference=rfmTest$Segment)
ldaCM

## The glmnet model
glmnGrid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1), lambda = seq(.01, .2, length = 40))
set.seed(476)
library(glmnet)
glmnFit <- train(Segment~Recency+Frequency+Monetary, 
                 data=rfmTrain, 
                 method = "glmnet",
                 tuneGrid = glmnGrid,
                 preProc = c("center", "scale"),
                 metric = "Accuracy",
                 trControl = ctrl)
glmnFit


glmnet2008 <- merge(glmnFit$pred,  glmnFit$bestTune)
glmnetCM <- confusionMatrix(glmnFit, reference=rfmTest$Segment)
glmnetCM

