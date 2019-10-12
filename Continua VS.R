# Master of Science - Data Science and Analytics
# October 2019

# Library and WD ----------------------------------------------------------

setwd("C:/Users/alejpava/Sura/Maestría/Semestre II/Métodos Estadísticos")
library(ggplot2)
library(dplyr)

# Read Data ---------------------------------------------------------------

Data <- read.table("./datacontinuousstudents.csv", sep = ",", header = T)
#Data <- read.table("./databinarystudents.csv", sep = ",", header = T)
#Data <- read.table("./datacountstudents.csv", sep = ",", header = T)
janitor::clean_names(Data)
Data <- Data[,2:34]
Data <- na.omit(Data)

# Data Partition ----------------------------------------------------------

y <- as.data.frame(Data[,1])
X <- as.data.frame(Data[,2:33])

X_C <- dplyr::select(X,x1,x2,x8,x10,x11,x12,x21,x22,x23,x24,x27,x31,x32)
X_i <- dplyr::select(X,x3,x4,x5,x6,x7,x9,x13,x14,x15,x16,x17,x18,x19,x20,x25,x26,x28,x29,x30)
X_sc <- cbind(scale(X_C),X_i)

y <- as.matrix(y)
X_sc <- as.matrix(X_sc)
data_total <- data.frame(y, X_sc)

data_part <- caret::createDataPartition(y = data_total[,1], times = 1, p = 0.8, list = F)
data_train <-data_total[data_part, ]
data_test  <-data_total[-data_part, ]

# Lasso -------------------------------------------------------------------

cv <- glmnet::cv.glmnet(X_sc, y, alpha = 1)
model_lasso <- glmnet::glmnet(X_sc, y, alpha = 1, lambda = cv$lambda.min)
plot(cv)
sele_lasso <- as.numeric(model_lasso$beta)!=0
head(X_sc[,sele_lasso])

# Elastic Net -------------------------------------------------------------

cv_en <- glmnet::cv.glmnet(X_sc, y, alpha = 0.3)
model_en <- glmnet::glmnet(X_sc, y, alpha = 0.3, lambda = cv_en$lambda.min)
plot(cv_en)
sele_en <- as.numeric(model_en$beta)!=0
head(X_sc[,sele_en])

# Ridge -------------------------------------------------------------------

cv_r <- glmnet::cv.glmnet(X_sc[,sele_en], y, alpha = 0)
model_ridge <- glmnet::glmnet(X_sc[,sele_en], y, alpha = 0, lambda = cv_r$lambda.min)
plot(cv_r)
model_ridge$beta

# BMA ---------------------------------------------------------------------

model_2 <- BMA::bic.glm(x=X_sc,y=y,maxCol = 55,glm.family=gaussian(link="identity"))
select <- model_2$probne0>50
head(X_sc[,select])

# GLM ---------------------------------------------------------------------

modelo <- glm(Data...1. ~ x10+x23+x31+x13+x20+x25, data=data_train, family=gaussian(link="identity"))
model_pred <- predict(modelo,data.frame(data_test[,2:33]),type = "response") 
mean((data_test[,1]-model_pred)^2)
# car::vif(modelo)
# 
# DM <- data.frame("x"=(1:length(model_pred)),"y"=y,"y'"=(model_pred))
# ggplot2::ggplot(DM,aes(x)) +
#   geom_line(aes(y = Data...1., colour = "Real"), size = 1) +
#   geom_line(aes(y = y., colour = "Modelo"), size = 1)

# Count -------------------------------------------------------------------

count_pred <- rep(0, length(model_pred))
for (i in 1:length(count_pred)){
  if(model_pred[i]>=-1){
    count_pred[i]=1
  }else{
    count_pred[i]=0
  }
}

count_real <- rep(0, length(data_test[,1]))
for (i in 1:length(count_real)){
  if(data_test[i,1]>=-1){
    count_real[i]=1
  }else{
    count_real[i]=0
  }
}

count_pred <- as.factor(count_pred)
count_real <- as.factor(count_real)
CM <- caret::confusionMatrix(data = count_pred, reference = count_real)
accy <- (CM$table[1,1]+CM$table[2,2])/sum(CM$table)
prcs <- CM$table[2,2]/(CM$table[2,2]+CM$table[1,2])
rcll <- CM$table[2,2]/(CM$table[2,2]+CM$table[2,1])
c(accy,prcs,rcll)