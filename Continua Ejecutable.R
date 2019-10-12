# Master of Science - Data Science and Analytics
# October 2019

# Library and WD ----------------------------------------------------------

setwd("C:/Users/alejpava/Sura/Maestría/Semestre II/Métodos Estadísticos")
library(ggplot2)
library(dplyr)

# Read Data ---------------------------------------------------------------

Data_Train <- read.table(file.choose(),header=T,dec=".",sep=",")
janitor::clean_names(Data_Train)
Data_Train <- Data_Train[,2:34]
Data_Train <- na.omit(Data_Train)

Data_Test <- read.table(file.choose(),header=T,dec=".",sep=",")
janitor::clean_names(Data_Test)
Data_Test <- Data_Test[,2:33]
Data_Test <- na.omit(Data_Test)

# Data Partition ----------------------------------------------------------

# Train

y_train <- as.data.frame(Data_Train[,1])
X_train <- as.data.frame(Data_Train[,2:33])

X_train_C <- dplyr::select(X_train,x1,x2,x8,x10,x11,x12,x21,x22,x23,x24,x27,x31,x32)
X_train_i <- dplyr::select(X_train,x3,x4,x5,x6,x7,x9,x13,x14,x15,x16,x17,x18,x19,x20,x25,x26,x28,x29,x30)
X_train_scale <- scale(X_train_C)
X_train_sc <- cbind(X_train_scale,X_train_i)

y_train <- as.matrix(y_train)
X_train_sc <- as.matrix(X_train_sc)
data_train <- data.frame(y_train, X_train_sc)
colnames(data_train)[1] <- "y"

# Test

#y_test <- as.data.frame(Data_Test[,1])
X_test <- as.data.frame(Data_Test[,1:32])

X_test_C <- dplyr::select(X_test,x1,x2,x8,x10,x11,x12,x21,x22,x23,x24,x27,x31,x32)
X_test_i <- dplyr::select(X_test,x3,x4,x5,x6,x7,x9,x13,x14,x15,x16,x17,x18,x19,x20,x25,x26,x28,x29,x30)
X_test_scale <- scale(X_test_C, center=attr(X_train_scale, "scaled:center"), 
                      scale=attr(X_train_scale, "scaled:scale"))
X_test_sc <- cbind(X_test_scale,X_test_i)

#y_test <- as.matrix(y_test)
X_test_sc <- as.matrix(X_test_sc)
#data_test <- data.frame(y_test, X_test_sc)
#colnames(data_test)[1] <- "y"

# GLM ---------------------------------------------------------------------

modelo <- glm(y ~ x10+x23+x31+x13+x20+x25, data=data_train, family=gaussian(link="identity"))
model_pred <- predict(modelo,data.frame(X_test_sc),type = "response") 
MSE <- mean((y_test-model_pred)^2)

# Count -------------------------------------------------------------------

count_pred <- rep(0, length(model_pred))
for (i in 1:length(count_pred)){
  if(model_pred[i]>=-1){
    count_pred[i]=1
  }else{
    count_pred[i]=0
  }
}

count_real <- rep(0, length(y_test))
for (i in 1:length(count_real)){
  if(y_test[i]>=-1){
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
# Plot --------------------------------------------------------------------

DM <- data.frame("x"=(1:length(model_pred)),"y"=y_test,"y'"=(model_pred))
ggplot2::ggplot(DM,aes(x)) +
  geom_line(aes(y = Data_Test...1., colour = "Real"), size = 1) +
  geom_line(aes(y = y., colour = "Modelo"), size = 1)