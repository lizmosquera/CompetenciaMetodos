# Master of Science - Data Science and Analytics
# October 2019

# Binary Database

# Libraries ---------------------------------------------------------------


library(glmnet)
library(MASS)
library(caret)
library(pROC)
library(dplyr)
library(e1071)
library(BMA)
library(BMS)
library(data.table)


# Load the data-------
dataset_binomial <- read.table("~/Desktop/databinarystudents.csv", sep = ",", header = T)
#  clean_names()
dataset_binomial <- dataset_binomial[,2:34]

str(dataset_binomial)


sum(is.na(dataset_binomial))
dataset_binomial <- na.omit(dataset_binomial) 


#Divide data in train and test
data_part <- createDataPartition(y = dataset_binomial[,1], times = 1, p = 0.7, list = F)
data_trainus <-dataset_binomial[data_part, ]
data_testus  <-dataset_binomial[-data_part, ]

y_testus <- as.matrix(data_testus[,1])
X_testus <- as.matrix(data_testus[,2:33])

y_trainus <- as.matrix(data_trainus[,1])
X_trainus <- as.matrix(data_trainus[,2:33])


## Scaling

## divide into regresors and dependent variable
y <- as.matrix(dataset_binomial[,1])
X <- as.matrix(dataset_binomial[,2:33])

#
# Data Partition
#
data_part <- createDataPartition(y = dataset_binomial[,1], times = 1, p = 0.7, list = F)
data_train <-dataset_binomial[data_part, ]
data_test  <-dataset_binomial[-data_part, ]

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

# Correlation Matrix ----------
M <- cor(X)
col <- colorRampPalette(c("grey"))
corrplot::corrplot(M, method = "color", order="hclust", col=RColorBrewer::brewer.pal(n=8, name="Greys"))



# Lasso using glmnet----------------------
fit = glmnet(x =X_trainover ,y =y_trainover , family = "binomial")

plot(fit, xvar = "dev", label = TRUE)

plot(fit, xvar = "lambda", label = FALSE)

# El Lambda que obtenemos al analizar sobre el error de clasificacion y el auc
cvfit = cv.glmnet(X_trainover, y_trainover, family = "binomial", type.measure = "class")
plot(cvfit)

cvfit$lambda.min # brings the value of Lambda ( minimum mean cross-validated error)
cvfit$lambda.1se # Trae el modelo mas regularizado tal que el error este a una desviacion estandar del minimo

cvfit = cv.glmnet(X_trainover, y_trainover, family = "binomial", type.measure = "auc")
plot(cvfit)

cvfit$lambda.min # brings the value of Lambda ( minimum mean cross-validated error)
cvfit$lambda.1se # Trae el modelo mas regularizado tal que el error este a una desviacion estandar del minimo

coef(cvfit, s = "lambda.1se")

# x1+x2 +x23+x4+x17+x19





# AIC Feature Selection stepwise-----

fit <- glm(y~.,family=binomial(link='logit'),data=Xtraindf)
step <- stepAIC(fit, direction="both",k=2) # k=log(150) 2
step$anova
summary(step)


#x1+x23+x4+x17+x19 
#



modelo <- glm(y_train ~ x18+x23+x20+x17+x2  ,family=binomial(link='logit'),data=X_train_scdffull)

model_pred <- predict(modelo,X_test_scdf,type = "response")
model_preddd <- ifelse(model_pred > 0.5, 1, 0)

# BMA ---------------------------------------------------------------------

#bic.glm
att1 <- BMA::bic.glm(x=X_train_scaled,y=y_train,maxCol = 55,glm.family=binomial(link="logit"))
select <- att1$probne0>50
head(X_train_scaled[,select])


#bma
att = bms(dataset_binomial, mprior = "uniform", g = "UIP", user.int = F)

coef(att)

summary(att)


image(att)


# GLM Predict------------


modelo <- glm(y_train ~ x18 + x17+ x23 + x20 + x2   ,family=binomial(link='logit'),data=X_train_scdffull)

model_pred <- predict(modelo,X_test_scdf,type = "response")
model_preddd <- ifelse(model_pred > 0.5, 1, 0)


# Convert to factor: p_class
p_class <- factor(model_preddd)
yt <- factor(y_test)

# Create confusion matrix

caret::confusionMatrix(data = p_class,reference = yt)




ytestvalues <- y_test[1:45] 
mp <- model_preddd[1:45] 

#ROCR Curve

par(pty = "s")
roc(ytestvalues,model_pred, plot=TRUE,legacy.axes=TRUE)


car::vif(modelo)




