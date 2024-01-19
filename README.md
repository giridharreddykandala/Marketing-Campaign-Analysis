# Marketing-Campaign-Analysis
# Readme File for Marketing Campaign Analysis

## Overview

This R code focuses on the exploratory data analysis (EDA), clustering, and logistic regression classification of a marketing campaign dataset. The dataset, loaded from an Excel file (`marketing_campaign.xlsx`), includes various features related to customer information, campaign responses, and product purchases. The analysis includes data cleaning, correlation analysis, EDA visualizations, clustering using hierarchical clustering, and logistic regression classification.

## Usage

1. **Data Loading:**
   - Ensure that the dataset file `marketing_campaign.xlsx` is in the same directory as the R script.
   - Use the `read_excel` function from the `readxl` library to load the dataset.

```
df <- read_excel("marketing_campaign.xlsx")
```

2. **Data Cleaning:**
   - The code performs data cleaning steps, such as handling outliers in the `Income` variable and converting categorical variables to factors.

```
# Example: Handling Outliers in Income
df_clust <- subset(df, Income < 500000)
df_clust$Response <- as.numeric(df_clust$Response)
df_clust$Education <- as.numeric(df_clust$Education)
df_clust$Kidhome <- as.numeric(df_clust$Kidhome)
df_clust$Teenhome <- as.numeric(df_clust$Teenhome)
df_clust$Complain <- as.numeric(df_clust$Complain)
df_clust$Marital_Status <- as.numeric(df_clust$Marital_Status)
```

3. **Exploratory Data Analysis (EDA):**
   - The code performs correlation analysis and generates various visualizations to understand the relationships between different variables.

```
# Example: Correlation between Income and Amount spent on different products
prod_data <- df %>%
  select(Income, MntWines, MntFruits, MntMeatProducts, MntFishProducts, MntSweetProducts, MntGoldProds)
cor(prod_data, use = "complete.obs")
corrplot(cor(prod_data, use = "complete.obs"), type = 'upper', method = "number")
```

4. **Clustering:**
   - The code performs hierarchical clustering on selected features and visualizes the resulting clusters.

```
# Example: Clustering
df_dist <- dist(df_clust, method = 'euclidean')
hclust_avg <- hclust(df_dist, method = 'ward.D2')
plot(hclust_avg)
```

5. **Logistic Regression Classification:**
   - The code builds a logistic regression model to predict the response variable based on selected features.

```
# Example: Logistic Regression Classification
logistic.model <- glm(formula = Response ~ Education + Kidhome + Teenhome + Complain + Marital_Status + Income + Recency + 
                       MntWines + MntFruits + MntMeatProducts + MntFishProducts + MntSweetProducts + MntGoldProds +
                       NumDealsPurchases + NumWebPurchases + NumCatalogPurchases + NumStorePurchases + NumWebVisitsMonth, 
                      data = df, family = "binomial")
```

6. **Model Evaluation:**
   - The code evaluates the logistic regression model's performance using confusion matrix and ROC curve.

```
# Example: Model Evaluation
logitPredict <- predict(logistic.model, df, type = "response")
logitPredictClass <- ifelse(logitPredict > 0.5, 1, 0)
actual_logit <- df$Response
cm_logit <- table(actual_logit, logitPredictClass)
Confusion_Matrix_Logit <- confusionMatrix(as.factor(actual_logit), as.factor(logitPredictClass))
```

## Dependencies

- R version 3.5.0 or higher
- Required R libraries: readxl, ggplot2, corrplot, tidyverse, caret, pROC
