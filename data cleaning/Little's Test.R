data<-read.csv("/Users/xuechunlu/OneDrive/20W/STAT841/STAT841-project/data/train.csv")
acc<-c()
for(i in 1:dim(data)[2]){
  if(sum(is.na(data[,i]))>=dim(data)[1]*0.3){acc = c(acc,i)}
}
data = data[,-acc]
dim(data)


library(remotes)
install_version("BaylorEdPsych", "0.5")
library(BaylorEdPsych)
install_version("mvnmle", "0.1-11")
library(mvnmle)
?BaylorEdPsych
data1<-data[,1:50]
data2<-data[,51:100]
data3<-data[,101:119]
model1<-LittleMCAR(data1)
model1$p.value # p-value is 0
model2<-LittleMCAR(data2)
model2$p.value # p- value is 1
model3<-LittleMCAR(data3)
model3$p.value # p-value is 0
 