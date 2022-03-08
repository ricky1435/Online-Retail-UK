# Classification of Online Customers using RFM 
* Developed a model that classifies customers based on their spending habits using K-means clustering in R. 
* The data was collected from [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Online+Retail+II).
* Engineereed features called Recency, Frequency and Monetary values from the data to classify customers based on their buying patterns.
* Created a Tableau Dashboard with important KPI's to help the business better understand their performance in the market.

## Code and Resources 
**R version:** 4.1.3

**Packages:** tidyverse, dplyr, ggplot2, caret, plotly, readxl, factoextra

**Data:** [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Online+Retail+II)

**R Code:** [Classification of Online Customers](https://github.com/ricky1435/Online-Retail-UK/blob/main/RFM.R)

## Tableau Dashboard before RFM analysis
https://public.tableau.com/app/profile/ruthvik.pvs/viz/Retail_dashboard/Dashboard1

## Data Cleaning
After collecting the data and creating the dashboard to analyze it clearly, we need to move to our next part of analysis that is customer classifaction based on spending habits. To do this, we have to first clean the data and create new features(RFM).
* Removed rows that does not have a customer ID
* Removed all cancelled transactions
* Created a new column named TotalPrice (TotalPrice = Quantity*Price)
* Removed all missing values
* Created three new features named Recency, Frequency and Monetary
      * Recency - What is the customers most recent transaction? 
      * Frequency - How frequently did the customer buy from our website? ( Count of customers transactions)
      * Monetary - How much on total did the customer spend? (Sum of all transactions)
* Removed all transactions that are below first Quantile and above third Quantile on boxplots. 

