# import data
CFS<-read.csv("/Users/xuechunlu/OneDrive/20W/STAT841/STAT841-project/data/cfs_training_data.csv")
head(CFS)
colnames(CFS)
data_CFS<-CFS[,1:35]
head(data_CFS)
X_CFS<-CFS[,1:35] # 53443 * 35
y_CFS<-as.factor(CFS$Response) # 53443

PCA<-read.csv("/Users/xuechunlu/OneDrive/20W/STAT841/STAT841-project/data/pca_20_attr_training_data.csv")
colnames(PCA)
data_PCA<-PCA[,2:22] 
X_PCA<-PCA[2:21] # 53443 * 20
y_PCA<-as.factor(PCA$Response)
length(y_PCA) # 5443

# data<-read.csv("data/train.csv")
# y<-as.factor(data$Response)




# X_CFS<-cbind(X_CFS,y)
# X_PCA<-cbind(X_PCA,y)
# X_CFS 69281 obs of 35 variables 




# library(neuralnet)
# model<-neuralnet(Response~.,data = X_CFS, hidden = 3)
# hist(predict(model,X_CFS[,-35]))
# X_CFS$Response<-
n<-length(y_CFS)
library(nnet)

nn1 <- nnet(as.factor(Response) ~., 
            data = data_CFS, 
            size = 20, 
            rang = 0.1,
            decay = 5e-4, 
            maxit = 200, 
            na.action = na.omit)

pred <- predict(nn1, X_CFS, type="class") 
pred

# error rate
sum(pred!=y_CFS)/n
# RMSE
sum((as.numeric(pred)-as.numeric(y_CFS))^2)/n
# MAE
sum(abs(as.numeric(pred)-as.numeric(y_CFS)))/n





# # PCA
# X_PCA_2<-cbind(X_PCA,y)
# head(X_PCA_2)
# nn2 <- nnet(y~., data = X_PCA_2, size = 20, rang = 0.1,
#             decay = 5e-4, maxit = 200, na.action = na.omit)
# pred2 <- predict(nn2, X_PCA,type="class") 
# pred2
# 
# sum(pred2!=y)/69281
# # RMSE
# sum((as.numeric(pred2)-as.numeric(y))^2)/69281
# # MAE
# sum(abs(as.numeric(pred2)-as.numeric(y)))/69281



nn1 <- nnet(as.factor(Response) ~., 
            data = data_CFS, 
            size = 20, 
            rang = 0.1,
            decay = 5e-4, 
            maxit = 200, 
            na.action = na.omit)





# a 10-fold cross validation

neural.net<-function(train){
  # maxit is the number of iterations for training neural networks
  model<-nnet(as.factor(train$Response) ~., 
              data = train, 
              size = 20, 
              rang = 0.1,
              decay = 5e-4, 
              maxit = 200, 
              na.action = na.omit)
  model
}

# on CFS
total<-data.frame(matrix(0,nrow=0,ncol=2))
k<-10
list<-1:k
index<-sample(1:k, nrow(data_CFS), replace = TRUE)
# Loop for 10 fold cross validation
for(i in 1:k){
  train <- subset(data_CFS[,-1], index %in% list[-i])
  test <- subset(data_CFS[,-1], index %in% c(i))
  # Removing  id columns from train, test
  train<-train[-ncol(data_CFS)]
  test<-test[-ncol(data_CFS)]
  # Building model
  nn.model<-neural.net(train)
  # Using built model for prediction
  test$pred<-predict(nn.model, newdata=test, type="class")
  # Appending actual and predicted results for each Cross validation step to data frame
  total<-rbind(total,data.frame(test$Response,test$pred))
}
nrow(total)
total

# Error rate
sum(total$test.pred != total$test.Response)/n # 0.4829257
# RMSE 
sqrt(sum((as.numeric(total$test.Response) - as.numeric(total$test.pred))^2)/n) # 5.698314
# MAE
sum(abs(as.numeric(total$test.Response) - as.numeric(total$test.pred)))/n # 1.373033




# on PCA

total<-data.frame(matrix(0,nrow=0,ncol=2))
k<-10
list<-1:k
head(data_PCA)
index<-sample(1:k, nrow(data_PCA), replace = TRUE)
# Loop for 10 fold cross validation
for(i in 1:k){
  train <- subset(data_PCA[,-1], index %in% list[-i])
  test <- subset(data_PCA[,-1], index %in% c(i))
  # Removing  id columns from train, test
  train<-train[-ncol(data_PCA)]
  test<-test[-ncol(data_PCA)]
  # Building model
  nn.model<-neural.net(train)
  # Using built model for prediction
  test$pred<-predict(nn.model, newdata=test, type="class")
  # Appending actual and predicted results for each Cross validation step to data frame
  total<-rbind(total,data.frame(test$Response,test$pred))
}
nrow(total)==n
total

# Error rate
sum(total$test.pred != total$test.Response)/n # 0.5418296
# RMSE 
sqrt(sum((as.numeric(total$test.Response) - as.numeric(total$test.pred))^2)/n) # sqrt(6.157252)
# MAE
sum(abs(as.numeric(total$test.Response) - as.numeric(total$test.pred)))/n # 1.53

