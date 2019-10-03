
library(BMS)



### Load Dataset---------

dataset_binomial <- read.table("~/Desktop/databinarystudents.csv", sep = ",", header = T)
#  clean_names()
dataset_binomial <- dataset_binomial[,2:34]

str(dataset_binomial)

##  Remove NA---------------
sum(is.na(dataset_binomial))
dataset_binomial <- na.omit(dataset_binomial)  





att = bms(dataset_binomial, mprior = "uniform", g = "UIP", user.int = F)

coef(att)

summary(att)

###------ Test the obtained model.

# x18 + x17+ x23 + x20 + x16 + x24 



