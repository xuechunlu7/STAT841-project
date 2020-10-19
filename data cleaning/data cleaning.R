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
read.csv("/Users/xuechunlu/OneDrive/20W/STAT841/STAT841-project/data/train2.csv")
