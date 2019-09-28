# Alejandro Palacio Vasquez, Code: 201910049228
# Master of Science - Data Science and Analytics
# August 2019

# Set WD and Load Library -------------------------------------------------

setwd("C:/Users/alejpava/Sura/Maestría/Semestre II/Métodos Estadísticos")
library(caret)
library(janitor)
library(tidyverse)
library(precrec)
library(ggplot2)
library(Rtsne)
library(ROCR)
library(MASS)
library(glmnet)
library(BayesVarSel)
library(spikeslab)
library(BoomSpikeSlab)
library(coda)

# Read Data ---------------------------------------------------------------

dataset_conti <- read.table("./datacontinuousstudents.csv", sep = ",", header = T)%>%
   clean_names()
dataset_conti <- dataset_conti[,2:34]

dim(dataset_conti)
sum(is.na(dataset_conti))
dataset_conti <- na.omit(dataset_conti)
dataset_conti <- scale(dataset_conti)

y <- as.matrix(dataset_conti[,1])
X <- as.matrix(dataset_conti[,2:33])

M <- cor(x)
col <- colorRampPalette(c("grey"))
corrplot::corrplot(M, method = "color", order="hclust", col=RColorBrewer::brewer.pal(n=8, name="Greys"))

# Data Partition ----------------------------------------------------------

data_part <- createDataPartition(y = dataset_conti[,1], times = 1, p = 0.7, list = F)
data_train <-dataset_conti[data_part, ]
data_test  <-dataset_conti[-data_part, ]

# train = sample(1:nrow(X), nrow(X)/2)
# test = (-train )

y_test <- as.matrix(data_test[,1])
X_test <- as.matrix(data_test[,2:33])

y_train <- as.matrix(data_train[,1])
X_train <- as.matrix(data_train[,2:33])

# Step AIC ----------------------------------------------------------------

fit <- lm(y~.,data=data.frame(data_train))
step <- stepAIC(fit, direction="both",k=2) # k=log(150) 2
step$anova
summary(step)

# Ridge -------------------------------------------------------------------

# lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
grid = 10^seq(10,-2, length =100)

# Setting alpha = 0 implements ridge regression

#ridge_cv <- cv.glmnet(X, y, alpha = 0, lambda = lambdas_to_try,
#                      standardize = TRUE, nfolds = 10)

ridge_mod = glmnet(X, y, alpha =0, lambda=grid)

ridge_mod = glmnet(X[train ,], y[train], alpha =0, lambda =grid, thresh =1e-12)
ridge_pred = predict(ridge_mod, s=4, newx = X[test ,])
mean((ridge_pred - y.test)^2)



ridge_cv <- cv.glmnet(X[train ,], y[train], alpha=0, nfolds=20, lambda = grid)
plot(ridge_cv)
bestlam = ridge_cv$lambda.min
ridge_pred = predict(ridge_mod, s = bestlam, newx = X[test ,])
mean((ridge_pred-y.test)^2)

# Fit final model, get its sum of squared residuals and multiple R-squared
model_cv <- glmnet(X, y, alpha = 0, lambda = lambda_cv, standardize = TRUE)
y_hat_cv <- predict(model_cv, X)
ssr_cv <- t(y - y_hat_cv) %*% (y - y_hat_cv)
rsq_ridge_cv <- cor(y, y_hat_cv)^2

# DR <- data.frame("x"=(1:150),"y"=dataset_conti$y,"y'"=y_hat_cv)
# ggplot(DR,aes(x)) +
#   geom_line(aes(y = y, colour = "Real"), size = 1) +
#   geom_line(aes(y = s0, colour = "Ridge"), size = 1)

DR <- data.frame("x"=(1:150),"y"=y.test,"y'"=ridge_pred)
ggplot(DR,aes(x)) +
  geom_line(aes(y = y, colour = "Real"), size = 1) +
  geom_line(aes(y = X1, colour = "Ridge"), size = 1)


# Lasso -------------------------------------------------------------------

set.seed(1)
lasso_mod = glmnet(X_train, y_train, alpha = 1, lambda = grid) 
plot(lasso_mod)

lasso_cv <- cv.glmnet(X_train, y_train, alpha = 1)
plot(lasso_cv)
bestlam = lasso_cv$lambda.min

lasso_pred = predict(lasso_mod, s = bestlam, newx = X_test)
lasso_coef = predict(lasso_mod, type = "coefficients", s = bestlam)


# Elastic Net -------------------------------------------------------------

# Train the model
train_control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 10,
                              search = "random",
                              verboseIter = TRUE)

elastic_net_model <- train(y ~ .,
                           data = data_train,
                           method = "glmnet",
                           trControl = train_control,
                           tuneLength = 25)
elastic_net_model$bestTune

enet_mod = glmnet(X_train, y_train, alpha = elastic_net_model$bestTune[,1], lambda = elastic_net_model$bestTune[,2])
coef(enet_mod)

#modelo <- glm(y ~ x1 + x2+ x3+ x5 + x6 + x10 + x12 + x13 + x17 + x19 + x20 + x23 + x24 + x25 + x27 + x28 + x29 
#              + x31 + x32, data=dataset_conti, family=gaussian(link="identity"))

# x1+x3+x4+x6+x7+x9+x10+x12+x13+x14+x15+x16+x19+x20+x22+x23+x25+x28+x29+x31
# BS&S x1+x2+x7+x10+x11+x21+x23+x27+x32
# S&S x1+x2+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16x17+x18+x20+x21+x23+x26+x27+x28+x29+x30+x31+x32

modelo <- glm(y ~ x1+x2+x10+x11+x23+x27+x29+x30+x32, data=data.frame(data_train), family=gaussian(link="identity"))
model_pred <- predict(modelo,data.frame(X_test))
mean((model_pred - y_test)^2)

DM <- data.frame("x"=(1:44),"y"=y_test,"y'"=model_pred)
ggplot(DM,aes(x)) +
  geom_line(aes(y = y, colour = "Real"), size = 1) +
  geom_line(aes(y = y., colour = "Modelo"), size = 1)

enet_pred = predict(enet_mod, newx = X_test)
mean((enet_pred - y_test)^2)

enet_cv <- glmnet(X, y, alpha = elastic_net_model$bestTune[,1], lambda = elastic_net_model$bestTune[,2])
coef(enet_cv)
y_hat_enet <- predict(enet_cv, X)
rsq_enet_cv <- cor(y, y_hat_enet)^2

DR <- data.frame("x"=(1:150),"y"=dataset_conti$y,"y'"=y_hat_enet)
ggplot(DR,aes(x)) +
  geom_line(aes(y = y, colour = "Real"), size = 1) +
  geom_line(aes(y = s0, colour = "Enet"), size = 1)


niter = 10000
SSBoom <- BoomSpikeSlab::lm.spike(y_test ~ X_test, niter = niter) #McCulloch and George (1997) approach
PIP <- colMeans(SSBoom$beta != 0)
PIP
SummarySS <- summary(mcmc(SSBoom$beta))
SummarySS

SS <- spikeslab::spikeslab(y_test ~ X_test) #Ishwaran and Rao approach
SS$bma