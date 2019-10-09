library(glmnet)
library(MASS)
library(caret)
library(pROC)
library(dplyr)
library(e1071)


#Load data======================================

#Training Data

dataset_binomial <- read.table("~/Desktop/databinarystudents.csv", sep = ",", header = T)
#  clean_names()
data_train <- dataset_binomial[,2:34]


#
#Load Test Data------ please define the route to the file with the tests data.
#
dataset_binomial_test <- read.table("~/Desktop/databinarystudents.csv", sep = ",", header = T)

data_test<- dataset_binomial_test[,2:34]


##  Remove NA-
sum(is.na(data_train))
data_train <- na.omit(data_train) 
sum(is.na(data_test))
data_test <- na.omit(data_test)







# Data Partition ----------------------------------------------------------



# train = sample(1:nrow(X), nrow(X)/2)
y_train <- as.matrix(data_train[,1])
X_train <- as.matrix(data_train[,2:33])

# test = (-train )
y_test <- as.matrix(data_test[,1])
X_test <- as.matrix(data_test[,2:33])

#matrix to dataframe
X_train <- as.data.frame((X_train))
X_test <- as.data.frame((X_test))

# Select columns of the dataframe
X_train_C <- select(X_train,x1,x2,x8,x9,x10,x11,x12,x21,x22,x23,x24,x25,x26,x27,x30,x31,x32)
X_train_i <- select(X_train,x3,x4,x5,x6,x7,x13,x14,x15,x16,x17,x18,x19,x20,x28,x29)

X_test_C <- select(X_test,x1,x2,x8,x9,x10,x11,x12,x21,x22,x23,x24,x25,x26,x27,x30,x31,x32)
X_test_i <- select(X_test,x3,x4,x5,x6,x7,x13,x14,x15,x16,x17,x18,x19,x20,x28,x29)

#Scale 
X_train_scaled <- scale(X_train_C)

X_test_scaled = scale(X_test_C, center=attr(X_train_scaled, "scaled:center"), 
                      scale=attr(X_train_scaled, "scaled:scale"))

#merge continuos data and dicotoma data
#full data
X_train_scdffull <- cbind(X_train_scaled, X_train_i, y_train)
X_test_scdffull <- cbind(X_test_scaled, X_test_i, y_test)

# x data
X_train_scdf <- cbind(X_train_scaled, X_train_i)
X_test_scdf <- cbind(X_test_scaled, X_test_i)

#dataframe to matrix
X_train_sc <- as.matrix(X_train_scdf)
X_test_sc <- as.matrix(X_test_scdf)


###
#train the model------------
####
modelo <- glm(y_train ~ x2 + x18+ x17 + x20+ x23  ,family=binomial(link='logit'),data=X_train_scdffull)




model_pred <- predict(modelo,X_test_scdf,type = "response")
model_preddd <- ifelse(model_pred > 0.5, 1, 0)


#
####Plot the results-------------------------------
###


# Convert to factor: p_class
p_class <- factor(model_preddd)
yt <- factor(y_test)

###
####  ¡¡¡¡ Include the size of the y train vector!!!!!!---------------
###
ytestvalues <- y_test[1:150] 
mp <- model_preddd[1:150] 


# Create confusion matrix
confusionMatrix(data = p_class,reference = yt)
#ROCR Curve
par(pty = "s")
roc(ytestvalues,model_pred, plot=TRUE,legacy.axes=TRUE)



