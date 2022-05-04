library(readxl)
#Import Data (class 1 - 2021-2022, class 2 - 2020-2021)
DataA <- read_excel("~/OneDrive - University of Leeds/3rd Year/201149011 Diss Data.xlsx", sheet = 1)
View(DataA)
TransferMarkt <- read_excel("~/OneDrive - University of Leeds/3rd Year/201149011 Diss Data.xlsx", sheet = 2)

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

pc2 <- prcomp(DataA[,c(9:77)])
summary(pc2)
ggbiplot(pc2)
pc2$x[,1]
#Plotting PCA
pc1.position <- c(rep("Forward", 3), rep("Defender",4), rep("Midfield", 7),rep("Defender",3), "Midfield", rep("Forward", 3), rep("Defender",4), rep("Midfield", 3), "Defender", rep("Midfield", 3))
ggbiplot(pc1,ellipse=TRUE,  labels=rownames(DataA), groups=DataA$`True Position`)

pc2.position <- c(rep("Forward", 3), rep("Defender",4), rep("Midfield", 7),rep("Defender",3), "Midfield", rep("Forward", 3), rep("Defender",4), rep("Midfield", 3), "Defender", rep("Midfield", 3))
ggbiplot(pc2,ellipse=TRUE,  labels=rownames(DataA), groups=DataA$`True Position`)

