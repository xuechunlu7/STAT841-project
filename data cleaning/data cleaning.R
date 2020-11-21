# import data
data = read.csv("data/train.csv",header = T)
dim(data)

# delete attributes with more than 30% missing data
acc = c()
for(i in 1:dim(data)[2]){
  if(sum(is.na(data[,i]))>=dim(data)[1]*0.3){acc = c(acc,i)}
}
data = data[,-acc]
dim(data)

# multiple imputation 
indx <- apply(data, 2, function(x) any(is.na(x)));indx
which(indx == TRUE)
# Employment_Info_1 Employment_Info_4 Employment_Info_6 Medical_History_1 
# 13                16                18                34 
install.packages("mice")
library(mice)
tempdata<-mice(data,m=5)
train2<-complete(tempdata,1)
# write the data with imputations to file train2.csv
write.csv(train2,
          file = "/Users/xuechunlu/OneDrive/20W/STAT841/STAT841-project/data/train2.csv", row.names = FALSE)
# test reading the data
train2<-read.csv("/Users/xuechunlu/OneDrive/20W/STAT841/STAT841-project/data/train2.csv")
colnames(train2)
X<-train2[,-c(1,119)]
X[,2]<-as.factor(X[,2])
y<-as.data.frame(as.factor(train2[,c(119)]))
train2<-cbind(X,y)
colnames(train2)[118]<-c("Risk")
colnames(train2)
# PCA 
library(PCAmixdata)

X.quanti<-X[,-c(2)]
X.quali<-as.data.frame(X[,2])

# 20 variables were selected out of 117 features (ID excluded).
PCA.model<-PCAmix(X.quanti = X.quanti, X.quali = X.quali, ndim = 20)
X_PCA<-predict(PCA.model, 
                           X.quanti = X.quanti, 
                           X.quali = X.quali)



# CFS

# 33 variables were selected out of 117 features.
col_vec<-rep(0,ncol(X.quanti))
for (i in 1:ncol(X.quanti)){
 col_vec[i]<-abs(cor(X.quanti[,i],as.numeric(as.matrix(y))))
}


index<-order(col_vec, decreasing=TRUE)[1:33]
col_vec[index]
colnames(X.quanti)[index]
# [1] 0.38160075 0.35139480 0.28658411 0.25916886 0.25770596 0.23989555 0.22017594 0.20960984 0.20243403 0.15955664
# [11] 0.15923002 0.13754155 0.13486343 0.13151860 0.13047593 0.12219577 0.11640813 0.11487047 0.11358045 0.11311191
# [21] 0.10202267 0.09625868 0.09392704 0.09381673 0.09367305 0.09357624 0.09337679 0.09288386 0.09082449 0.08923293
# [31] 0.08459523 0.08372928 0.08078256

library(rcompanion)
cramerV(train2$Risk,train2$Product_Info_2) # 0.1445
index<-order(col_vec, decreasing=TRUE)[1:32]
features<-colnames(X.quanti)[index]
features<-append(features,"Product_Info_2")
index2<-rep(0,33)
for (i in 1:33){
  index2[i]<-which(colnames(X)==features[i])
}
index2
X_CFS<-X[,index2]
