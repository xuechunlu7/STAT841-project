X_CFS<-read.csv("/Users/xuechunlu/OneDrive/20W/STAT841/STAT841-project/data/cfs_training_data.csv")
X_PCA<-read.csv("/Users/xuechunlu/OneDrive/20W/STAT841/STAT841-project/data/pca_20_attr_training_data.csv")
data<-read.csv("data/train.csv")
y<-as.factor(data$Response)
# X_CFS<-cbind(X_CFS,y)
# X_PCA<-cbind(X_PCA,y)
# X_CFS 69281 obs of 35 variables 
head(X_CFS)
head(X_PCA)



# library(neuralnet)
# model<-neuralnet(Response~.,data = X_CFS, hidden = 3)
# hist(predict(model,X_CFS[,-35]))
# X_CFS$Response<-

library(nnet)
X_CFS$Response<-as.factor(X_CFS$Response)
nn1 <- nnet(Response~., data = X_CFS, size = 20, rang = 0.1,
            decay = 5e-4, maxit = 200, na.action = na.omit)

pred<-predict(nn1, X_CFS[,-35],type="class") 
pred
# error rate
sum(pred!=y)/69281
# RMSE
sum((as.numeric(pred)-as.numeric(y))^2)/69281
# MAE
sum(abs(as.numeric(pred)-as.numeric(y)))/69281





# PCA
X_PCA_2<-cbind(X_PCA,y)
head(X_PCA_2)
nn2 <- nnet(y~., data = X_PCA_2, size = 20, rang = 0.1,
            decay = 5e-4, maxit = 200, na.action = na.omit)
pred2 <- predict(nn2, X_PCA,type="class") 
pred2

sum(pred2!=y)/69281
# RMSE
sum((as.numeric(pred2)-as.numeric(y))^2)/69281
# MAE
sum(abs(as.numeric(pred2)-as.numeric(y)))/69281

