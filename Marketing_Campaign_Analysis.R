rm(list=ls())
cat("\014")

#============= Libraries ====================================================
library("readxl")
library(ggplot2)
install.packages("corrplot")
library(corrplot)
library(tidyverse)
library(caret)
#============================================================================
#Reading xlsx file
df <- read_excel("marketing_campaign.xlsx")
#Converting to data frame
df <- as.data.frame(df)

#Data cleaning in Marital Status
df$Marital_Status[df$Marital_Status == "Absurd"] <- "Single" 
df$Marital_Status[df$Marital_Status == "Alone"] <- "Single" 
df$Marital_Status[df$Marital_Status == "YOLO"] <- "Single"
df$Marital_Status[df$Marital_Status == "Divorced"] <- "Single"
df$Marital_Status[df$Marital_Status == "Widow"] <- "Single"


#Converting required columns to factors
df$Response <- as.factor(df$Response)
df$Education <- as.factor(df$Education)
df$Kidhome <- as.factor(df$Kidhome)
df$Teenhome <- as.factor(df$Teenhome)
df$Complain <- as.factor(df$Complain)
df$Marital_Status <- as.factor(df$Marital_Status)
df[,c(21:26)] <- lapply(df[,c(21:26)],factor)


#============================================================================
#============================= Exploratory Data Analysis ====================
#Correaltion between Income and Amount spent on different products
prod_data <- df%>%
  select(Income,MntWines,MntFruits,MntMeatProducts,MntFishProducts,MntSweetProducts,MntGoldProds)
cor(prod_data, use = "complete.obs")
corrplot(cor(prod_data,use = "complete.obs" ),type = 'upper',method = "number",)

#Correaltion between Income and Products bought in different sources
purchase_data <- df%>%
  select(Income,NumDealsPurchases,NumWebPurchases,NumCatalogPurchases,NumStorePurchases)
cor(purchase_data, use = "complete.obs")
corrplot(cor(purchase_data,use = "complete.obs" ),type = 'upper',method = "number",)

#Number of Customers Grouped By Education
g1 <- ggplot(df, aes(Education, fill=Education))
g1+geom_bar(position='dodge', )+ geom_text(stat='count',
                                           aes(label = ..count..),
                                           position = position_dodge(width = 1.0), 
                                           vjust = -0.5)

#Education vs Income
g2<- ggplot(df, aes(Education, Income))
g2 +  geom_boxplot(notch = TRUE, fill = c("light green", "steel blue", "pink", "orange","red")) + ylim(NA,100000)

#Number of customers grouped by Marital Status
g3 <- ggplot(df,aes(Marital_Status,fill=Marital_Status))
g3 + geom_bar()  + geom_text(stat='count', aes(label = ..count..),position = position_dodge(width = 1.0), vjust = -0.5)

#Marital_Status vs Response Rate
ggplot(df, aes(x= Response,  group=Marital_Status)) + 
  geom_bar(aes(y = ..prop.., fill = (Marital_Status)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Marital_Status") +
  facet_grid(~Marital_Status) +
  scale_y_continuous(labels = scales::percent)

#Customer Education vs Response Rate
ggplot(df, aes(x= Response,  group=Education)) + 
  geom_bar(aes(y = ..prop.., fill = (Education)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Education") +
  facet_grid(~Education) +
  scale_y_continuous(labels = scales::percent)

#Customers Kids at home vs Response rate
ggplot(df, aes(x= Response,  group=Kidhome)) + 
  geom_bar(aes(y = ..prop.., fill = (Kidhome)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Kidhome") +
  facet_grid(~Kidhome) +
  scale_y_continuous(labels = scales::percent)

#Customers Teens at home vs Response rate
ggplot(df, aes(x= Response,  group=Teenhome)) + 
  geom_bar(aes(y = ..prop.., fill = (Teenhome)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Teenhome") +
  facet_grid(~Teenhome) +
  scale_y_continuous(labels = scales::percent)

#Customers Complaints Rate vs Response Rate
ggplot(df, aes(x= Response,  group=Complain)) + 
  geom_bar(aes(y = ..prop.., fill = (Complain)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Complain") +
  facet_grid(~Complain) +
  scale_y_continuous(labels = scales::percent)

#Income vs Amount Spent on Products
Amt1 <- data.frame(Income=df$Income,
                  Amount=c(df$MntWines,df$MntFruits,df$MntMeatProducts),
                  Response=df$Response,
                  group = c(rep("Wines",nrow(df)),
                            rep("Fruits",nrow(df)),
                            rep("Meat", nrow(df))))
ggplot(Amt1, aes(Income,Amount)) + geom_point(aes(color=Response)) + xlim(c(NA,100000))   + geom_smooth()+facet_grid(group ~.)

#Income vs Amount Spent on Products
Amt2 <- data.frame(Income=df$Income,
                   Amount=c(df$MntFishProducts,df$MntSweetProducts,df$MntGoldProds),
                   Response=df$Response,
                   group = c(rep("FishProducts",nrow(df)),
                             rep("SweetProducts",nrow(df)),
                             rep("Gold", nrow(df))))
ggplot(Amt2, aes(Income,Amount)) + geom_point(aes(color=Response)) + xlim(c(NA,100000))   + geom_smooth()+facet_grid(group~.)


#Response Rate for different Campaigns
Campaign <- data.frame(Response = c(df$AcceptedCmp1,df$AcceptedCmp2,df$AcceptedCmp3,df$AcceptedCmp4,df$AcceptedCmp5,df$Response),
                       group = c(rep("Campaign 1", nrow(df)),
                                 rep("Campaign 2", nrow(df)),
                                 rep("Campaign 3", nrow(df)),
                                 rep("Campaign 4", nrow(df)),
                                 rep("Campaign 5", nrow(df)),
                                 rep("Last Campaign", nrow(df))))

ggplot(Campaign, aes(Response,fill=Response)) + geom_bar() + geom_text(stat='count', aes(label = ..count..),position = position_dodge(width = 1.0), vjust = -0.5)+ facet_grid(.~group)


#====================================================================================
#=================================================================================
#========================= Clustering ===========================================

#Checking and Handling Outliers
g5<- ggplot(df, aes(Income))
g5 +  geom_boxplot(fill = c("orange"))

#Data Cleaning for Clustering
df_clust <- subset(df,Income < 500000)
df_clust$Response <- as.numeric(df_clust$Response)
df_clust$Education <- as.numeric(df_clust$Education)
df_clust$Kidhome <- as.numeric(df_clust$Kidhome)
df_clust$Teenhome <- as.numeric(df_clust$Teenhome)
df_clust$Complain <- as.numeric(df_clust$Complain)
df_clust$Marital_Status <- as.numeric(df_clust$Marital_Status)
df_clust$AcceptedCmp1 <- as.numeric(df_clust$AcceptedCmp1)
df_clust$AcceptedCmp2 <- as.numeric(df_clust$AcceptedCmp2)
df_clust$AcceptedCmp3 <- as.numeric(df_clust$AcceptedCmp3)
df_clust$AcceptedCmp4 <- as.numeric(df_clust$AcceptedCmp4)
df_clust$AcceptedCmp5 <- as.numeric(df_clust$AcceptedCmp5)
df_clust <- na.omit(df_clust)
df_clust <- df_clust%>%select(-Dt_Customer)

# mapping distances
df_dist <- dist(df_clust,method = 'euclidean')

# linkage method 
hclust_avg <- hclust(df_dist, method = 'ward.D2')

# Dendrogram 
plot(hclust_avg)

# 4 clusters
cut_avg <- cutree(hclust_avg, k = 4) 

# Add cluster number
df_cl <- mutate(df_clust, cluster = cut_avg)
View(df_cl)

# Summary statistics
sumstat <- df_cl %>% group_by(cluster) %>% summarize_each(list(mean))
View(sumstat)

#Total Amount Spent
df_cl$Amt_Spent <- df_cl$MntWines + df_cl$MntFruits + df_cl$MntMeatProducts + df_cl$MntFishProducts + df_cl$MntSweetProducts + df_cl$MntGoldProds

#Income vs Amt_Spent
g6 <- ggplot(df_cl,aes(Income,Amt_Spent))
g6 + geom_point(aes(color=as.factor(cluster))) + xlim(0,100000)  + ylim(c(0,2500) )


#Cluster vs Food Products
Prod_Cluster <- data.frame(Amount = c(sumstat$MntWines , sumstat$MntFruits , sumstat$MntMeatProducts , sumstat$MntFishProducts , sumstat$MntSweetProducts , sumstat$MntGoldProds),
                           Products = c(rep("Wines", nrow(sumstat)),
                                 rep("Fruits", nrow(sumstat)),
                                 rep("Meat", nrow(sumstat)),
                                 rep("Fish", nrow(sumstat)),
                                 rep("Sweets", nrow(sumstat)),
                                 rep("Gold", nrow(sumstat))),
                           Cluster = c(sumstat$cluster))


g7 <- ggplot(Prod_Cluster,aes(Products,Amount,fill=as.factor(Products)))
g7 + geom_col() + facet_grid(.~Cluster)

#=======================================================================================
#Classification
#==================================Logistic Regression =================================


logistic.model <- glm(formula = Response ~ Education + Kidhome + Teenhome + Complain +Marital_Status + Income + Recency+ 
                       MntWines+MntFruits+ MntMeatProducts + MntFishProducts + MntSweetProducts + MntGoldProds+
                      NumDealsPurchases+NumWebPurchases+NumCatalogPurchases+NumStorePurchases+NumWebVisitsMonth, 
                      data = df, family = "binomial")

summary (logistic.model)

#OddsRatio
exp(coef(logistic.model))
logit_OR <- as.data.frame(exp(cbind(OR = coef(logistic.model), confint.default(logistic.model))))
logit_OR %>% arrange(desc(logit_OR))

#Confusion Matrix
logitPredict <- predict(logistic.model, df, type = "response")
logitPredictClass <- ifelse(logitPredict > 0.5, 1, 0)
actual_logit <- df$Response
predict_logit <- logitPredictClass

cm_logit <- table(actual_logit, predict_logit)
cm_logit

TN=1831
FN=228
FP=52
TP=105
P=FN + TP
N=TN + FP
#True Positive Rate
TPR=TP/P;TPR
TNR=TN/N;TNR
FPR=FP/N;FPR
FNR=FN/P;FNR

Confusion_Matrix_Logit <- confusionMatrix(as.factor(actual_logit), as.factor(predict_logit))
Confusion_Matrix_Logit
Logit_Accuracy <- Confusion_Matrix_Logit$overall['Accuracy']
Logit_Accuracy

library(pROC)
ROC <- roc(df$Response,logitPredict)
plot(ROC, col = 'red', )
ROC$auc


