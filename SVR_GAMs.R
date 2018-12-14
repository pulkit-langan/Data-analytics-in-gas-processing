#set working directory 
setwd("E:/College/Sem 7/CLD411/") 

#load the dataset 
teg = read.csv("teg.csv", header =T) 

#create scatterplots 
pairs(teg[,1:3], lower=NULL) 

#split the data into training data and testing data 
set.seed(10)
library(caTools) 
sample = sample.split(teg, SplitRatio = 0.85) 
train = subset(teg,sample==T) 
test = subset(teg,sample==F) 

#fit a simple linear regression model 
model1 = lm(Water.Dew.Point.Temperature ~ Contactor.Temperature + TEG.Purity , data = train) 

par(mfrow=c(2,2))
plot(model1)
summary(model1) 

#build svr model for a wide range of values of parameters
library(e1071)
svr_rad = tune(svm, Water.Dew.Point.Temperature ~ Contactor.Temperature + TEG.Purity,  
               data = train, ranges= list(epsilon = seq(0,1,0.01), cost = 2^(2:9) ) , kernel ="radial")

#select the best model
svr_rad_best = svr_rad$best.model

#make prediction on test data
pred_y = predict(svr_rad_best,test)

#scatterplot of actual vs predicted values

library(ggplot2)

require(ggplot2)

x= seq(1,length(test$Water.Dew.Point.Temperature))
water_dpt= test$Water.Dew.Point.Temperature

data <- data.frame(x,water_dpt)

g1 = ggplot(data,aes(x=x,y=water_dpt)) + geom_point(aes(color="Actual Values")) + geom_point(aes(y=pred_y,color="Predicted Values"))

g1 + labs(title="Performance of SVR", y="Water Dew Point Temperature", x="Index")

#check adequacy of model
err = test$Water.Dew.Point.Temperature - pred_y
SS_res = sum(err^2)

SS_tot=c()

for(i in 1:nrow(test))
{
  SS_tot[i] = (test$Water.Dew.Point.Temperature[i] - mean(test$Water.Dew.Point.Temperature))^2
}

Rsq = 1-(SS_res/sum(SS_tot))
Rsq

#build GAMs model with various dof

Rsq_gam = c()
library(gam)

for(j in 1:20)
{
  
  gam <- gam(Water.Dew.Point.Temperature~s(Contactor.Temperature,df=j)+s(TEG.Purity,df=j),data = train)
  pred = predict(gam,test)
  
  SS_tot_gam = c()
  
  for(i in 1:nrow(test))
  {
    SS_tot_gam[i] = (test$Water.Dew.Point.Temperature[i] - mean(test$Water.Dew.Point.Temperature))^2
  }
  
  err = test$Water.Dew.Point.Temperature - pred
  SS_res_gam = sum(err^2)
  Rsq_gam[j] = 1-(SS_res_gam/sum(SS_tot_gam))
  
}


gam1 <- gam(Water.Dew.Point.Temperature~s(Contactor.Temperature,df=10)+s(TEG.Purity,df=10),data = train)
pred = predict(gam1,test)

SS_tot_gam = c()

for(i in 1:nrow(test))
{
  SS_tot_gam[i] = (test$Water.Dew.Point.Temperature[i] - mean(test$Water.Dew.Point.Temperature))^2
}

#scatterplot of actual vs predicted values


g2 = ggplot(data,aes(x=x,y=water_dpt)) + geom_point(aes(color="Actual Values")) + geom_point(aes(y=pred,color="Predicted Values"))

g2 + labs(title="Performance of GAMs", y="Water Dew Point Temperature", x="Index")


err = test$Water.Dew.Point.Temperature - pred
SS_res_gam = sum(err^2)
Rsq_gam= 1-(SS_res_gam/sum(SS_tot_gam))


