library(readxl)
#Import Data (class 1 - 2021-2022, class 2 - 2020-2021)
Final_data <- read_excel("~/OneDrive - University of Leeds/3rd Year/3200 Project/Final_data.xlsx")
View(Final_data)
Class1Pre<- read_excel("~/OneDrive - University of Leeds/3rd Year/3200 Project/Final_data.xlsx", sheet = 1)
Class1Post<- read_excel("~/OneDrive - University of Leeds/3rd Year/3200 Project/Final_data.xlsx", sheet = 2)
Class2Pre<- read_excel("~/OneDrive - University of Leeds/3rd Year/3200 Project/Final_data.xlsx", sheet = 3)
Class2post<- read_excel("~/OneDrive - University of Leeds/3rd Year/3200 Project/Final_data.xlsx", sheet = 4)
Class1PreNumeric <- read_excel("~/OneDrive - University of Leeds/3rd Year/3200 Project/Final_data.xlsx", sheet = 5)
TotalPreData<- read_excel("~/OneDrive - University of Leeds/3rd Year/3200 Project/Final_data.xlsx", sheet = 6)
ClusterData<- read_excel("~/OneDrive - University of Leeds/3rd Year/3200 Project/Final_data.xlsx", sheet = 7)
Attack_data<- read_excel("~/OneDrive - University of Leeds/3rd Year/3200 Project/Final_data.xlsx", sheet = 8)
Midfield_data<- read_excel("~/OneDrive - University of Leeds/3rd Year/3200 Project/Final_data.xlsx", sheet = 9)
Defend_data<- read_excel("~/OneDrive - University of Leeds/3rd Year/3200 Project/Final_data.xlsx", sheet = 10)
Defend_Test<- read_excel("~/OneDrive - University of Leeds/3rd Year/3200 Project/Final_data.xlsx", sheet = 11)
Transfer_data<- read_excel("~/OneDrive - University of Leeds/3rd Year/3200 Project/Final_data.xlsx", sheet = 12)
####Descriptive statistics####
install.packages("ggplot")
library(ggplot2)

#Structure of the dataset
str(Class1Pre)
str(Class2Pre)
#Min, Max and Range of $Transfer Fee across both samples
rng_fee1 <- range(Class1Pre$`Transfer fee [£m]`)
rng_fee2 <- range(Class2Pre$`Transfer fee (£m)`)

range1 <- function(x) {
  range <- max(x) - min(x)
  return(range)
}
range1(Class1Pre$`Transfer fee [£m]`) #Substitute any $variable to examine different range
View(range1)

#Mean, Median, Quartiles and Inter Quantile Range
mean(Class1Pre$`Transfer fee [£m]`, na.rm = TRUE)
median(Class1Pre$`Transfer fee [£m]`)
quantile(Class1Pre$`Transfer fee [£m]`, 0.25) #First Quartile
quantile(Class1Pre$`Transfer fee [£m]`, 0.75) #Third Quartile
quantile(Class1Pre$`Transfer fee [£m]`, 0.4) #4th decile
IQR(Class1Pre$`Transfer fee [£m]`) #inter-quartile range

#Standard deviation and Variance
sd(Class1Pre$`Transfer fee [£m]`) #StDev
var(Class1Pre$`Transfer fee [£m]`) #Variance

##Summary of stats##
summary(Class1Pre$`Transfer fee [£m]`)
Stat_by_Nationality <- by(Class1Pre, Class1Pre$`Nationality [country]`, summary) #summary of descriptive stats by single variable
#Descriptive Statistics - overview
install.packages("descr")
library(descr)
descr(DataA) 
SummStat <- summary(DataA)
SummStat
library(Hmisc)
Hmisc::describe(DataA)

#Detailed summary of stats
install.packages("pastecs")
library(pastecs)
stat.desc(DataA, norm = TRUE) 

#Detailed summary with graphs in viewer
install.packages("summarytools")
library(summarytools)
stview(dfSummary(DataA))

hist(DataA$Transfer_fee)
'Transfer Fee [£m]' <- DataA$Transfer_fee
hist(`Transfer Fee [£m]`, col="lightblue", ylim=c(0,60))
#Correlation
install.packages("corrplot")
library("corrplot")
corrplot(cor(Midfield_data),
         method = "number",
         type = "upper" # show only upper side
)
#Correlation test
install.packages("Hmisc")
library(Hmisc)
res <- rcorr(as.matrix(Class1PreNumeric)) 
round(res$P, 3) # display p-values (rounded to 3 decimals)

#Correlation of Midfielders
library("corrplot")
corrplot(cor(Class1PreNumeric),
         method = "number",
         type = "upper" # show only upper side
)

#Correlation matrix - condense variables used
correlation::correlation(Class1PreNumeric,
                         include_factors = TRUE, method = "auto"
)

#Presenting Data##
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

#Histograms
hist(Class1Pre$Age ~ Class1Pre$`Transfer fee [£m]`)
hist(Class1Pre$`Transfer fee [£m]`)
#Boxplot
boxplot(TotalPreData$Transfer_fee ~ TotalPreData$`Years remaining on contract`)
#Scattergraph
plot(TotalPreData$Transfer_fee ~ TotalPreData$`Years remaining on contract`)
#Scattergraph of Age over Transfer Fee sorted by continent
ggplot(DataA) +
  aes(x = `Years remaining on contract`, y = `Transfer_fee`, colour = `True Position`) +
  geom_point() +
  scale_color_hue()

ggplot(data = DataA, mapping = aes(x = `Years remaining on contract`, y = Transfer_fee`) +
  geom_point(color = "blue")
#Box plot Transfer Fee by position sorted by foot
ggplot(data = DataA, mapping = aes(x = `Years remaining on contract`, y = `Transfer_fee`) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.3, aes(color = `True Position`))

#####K-means Cluster analysis #####
install.packages("ClusterR")
install.packages("cluster")
install.packages("ggplot2")
install.packages("factoextra")
# Loading package
library(ClusterR)
library(cluster)
library(ggplot2)
library(factoextra)
#Remove non-numeric data
data_1 <- ClusterData[, -2]
#Scale data
df <- scale(data_1)
#Selecting optimal clusters#

fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")
#Running a k-means cluster
set.seed(123)
km.res <- kmeans(df, 3, nstart = 25)
print(km.res)

#Generating Confusion matrix
km.res$cluster
cm <- table(ClusterData$Position, km.res$cluster)
cm

data.frame <- km.res$centers
data.frame
#Visualising the cluster
fviz_cluster(km.res, data = df)

#### Regression Analysis ####

#### OLS Regressions ####
#Attack OLS - Benchmark MSE: 0.11 - RMSE: 0.33
Attack_data<- read_excel("~/OneDrive - University of Leeds/3rd Year/3200 Project/Final_data.xlsx", sheet = 8)
DfAttack <- Attack_data[,-1]
AttkTest <- DfAttack[1:33,]
AttkTrain <- DfAttack[34:43,]
modelAttk <- lm(Log_Fee~.,data = AttkTest)
summary(modelAttk)
predictAttk <- predict(modelAttk, newdata = AttkTrain)
predictAttk
#MSE - accuracy measure: 0.69 - RMSE: 0.83
Fee1 <- AttkTrain$Log_Fee
errorAttk <-mean((Fee1 - predictAttk) ^2)
errorAttk
#Visualise
par(mfrow=c(2,2))
plot(modelAttk)
par(mfrow=c(1,1))

#Defend OLS - Benchmark MSE: 0.26 - RMSE: 0.51
Defend_data<- read_excel("~/OneDrive - University of Leeds/3rd Year/3200 Project/Final_data.xlsx", sheet = 10)
DfDefend <- Defend_data[,-1]
DefendTest <- DfDefend[1:48,]
DefendTrain <- DfDefend[49:61,]
modelDef <- lm(Log_Fee~.,data = DefendTest)
summary(modelDef)
predictDef <- predict(modelDef, newdata = DefendTrain)
predictDef
#MSE - accuracy measure: 0.92 - RMSE: 0.96
Fee2 <- DefendTrain$Log_Fee
errorDef <-mean((Fee2 - predictDef) ^2)
errorDef 
#visualise
par(mfrow=c(2,2))
ggplot(modelDef)
par(mfrow=c(1,1))

#Plotting results
Model1_plot <- (plot(Predict1F,Results1))
library(ggplot2)
Plot1 <- ggplot(TransferMarkt, aes(x=Predict1F, y=Results1)) + 
  geom_point()+
  labs(title="Correlation between Predict log(Fee) and Transfermarkt Benchmark Log(Fee)",
       x="Model[1]_Log(Fee)", y = "Transfermarkt_Log(Fee)")
Plot1
#Results
Results1 <- TransferMarkt$`Log Predict`
Results1
Predict1F <- Predict1L[1:37]
Predict1F

##Stepwise regression analysis##
install.packages("tidyverse")
library(tidyverse)
install.packages("caret")
library(caret)
install.packages("leaps")
library(leaps)
install.packages("dplyr")
library(MASS)
# Fit the full model 
full.model <- lm(`Transfer fee [£m]` ~., data = TotalPreData)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(full.model)

#### PCA Regression model ####

DataA <- read_excel("~/OneDrive - University of Leeds/3rd Year/201149011 Data.xlsx", sheet = 1)
View(DataA)
TransferMarkt <- read_excel("~/OneDrive - University of Leeds/3rd Year/201149011 Data.xlsx", sheet = 2)

#PCA 
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
#Removing non-numeric data
d <- DataA[, -1] #Remove non-numeric data
df <- scale(d[, -1]) #Scale the data 

pc1 <- princomp(DataA[,c(9:77)], cor=TRUE, score=TRUE)
pc1 <- princomp(df, cor=TRUE, score=TRUE)
summary(pc1)
ggbiplot(pc1)
pc1$loadings[,1]
pc1$loadings[,14]
#Defining composite variables
v1 <- pc1$scores[1:99,1]
v2 <- pc1$scores[1:99,2]
v3 <- pc1$scores[1:99,3]
v4 <- pc1$scores[1:99,4]
v5 <- pc1$scores[1:99,5]
v6 <- pc1$scores[1:99,6]
v7 <- pc1$scores[1:99,7]
v8 <- pc1$scores[1:99,8]
v9 <- pc1$scores[1:99,9]
v10 <- pc1$scores[1:99,10]
v11 <- pc1$scores[1:99,11]
v12 <- pc1$scores[1:99,12]
v13 <- pc1$scores[1:99,13]
v14 <- pc1$scores[1:99,14]
#testing and training dataset
dfTest1 <- DataA[1:99,]
dfTrain1 <- DataA[100:136,]
Fees <- dfTrain1$Transfer_fee
#liner model
modelc <- lm(Transfer_fee~ v1+v2+v3+v5+v7+v10+v11+v12+v13,data = dfTest1)
summary(modelc)
Predict1 <- predict(modelc, newdata = dfTrain1) #predict
Prediction <- Predict1[1:37]
Prediction - Fees #not accurate error
#MSE
sum(Prediction - Fees)
mean((Fees - Predict1) ^2)
error <- mean((Fees - predict.lm(modelc, dfTrain1)) ^ 2) 
error
#RSE
k <- length(modelc$coefficients)-1 #Subtract one to ignore intercept
n <- length(modelc$residuals)
SSE <- sum(modelc$residuals**2)
sqrt(SSE/(n-(1+k)))

#Transfermarkt
#MSE
Predict2 <- TransferMarkt$`Transfermarkt prediction`
sum(Prediction - Fees)
mean((Fees - Predict2) ^2)
error <- mean((Fees - predict.lm(modelc, dfTrain1)) ^ 2) 
error
################

# histogram
hist(res_aov$residuals)

# QQ-plot
library(car)
qqPlot(res_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)
