library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(plotly)
library(readxl)
library(rfm)
library(stats)
library(factoextra)
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

"We can see that there are missing values in Description and Customer ID. As customer ID is important for analysis and
important for the clustering algorith, we remove these values from data. Description is also
necessary for further analysis."

# Function to preprocess data
data.preprocess <- function(data1)
{
  # Creating another column called TotalPrice
  data1$TotalPrice <- data1$Quantity * data1$Price
  
  # Removing Missing Values
  data1<-na.omit(data1)
  
  # Removing the Invoice rows that containts the letter C. C means this order was cancelled.
  data1<-data1[!grepl("C",data1$Invoice),]
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
rfm.calc <- function(x){
  z<-x %>% group_by(`Customer ID`) %>% 
     summarise(Recency = as.numeric(date1- max(InvoiceDate)), Frequency = n(), Monetary = sum(TotalPrice),firstpurchace=min(InvoiceDate))
  
  # Removing transactions with values above the 3rd Quantile and below Quantile 1
  Q1 <- quantile(z$Monetary, .25)
  Q3 <- quantile(z$Monetary, .75)
  IQR <- IQR(z$Monetary)
  z <- subset(z, z$Monetary>= (Q1 - 1.5*IQR) & z$Monetary<= (Q3 + 1.5*IQR))
  return(z)
}
rfm.scores<-rfm.calc(data)

# Kmeans Clustering and Assigning Clusters to the Dataset
set.seed(1029)
kmeans.clustering<- function(rfm)
{
  results <- list()
  rfm.normalized <- select(rfm,c('Recency','Frequency','Monetary'))
  kmeans.clust<-kmeans(rfm.normalized,center = 5,iter.max = 50)
  results$plot<-fviz_cluster(kmeans.clust,data=rfm.normalized,geom=c('point'),ellipse.type = 'euclid')
  rfm.normalized$`Customer ID` <- rfm$`Customer ID`
  rfm.normalized$clusters <- kmeans.clust$cluster
  results$data <- rfm.normalized
  return(results)
}
kmeans.plot<-kmeans.clustering(rfm.scores)[1]
kmeans.plot
rfm<-kmeans.clustering(rfm.scores)[2]
rfm
