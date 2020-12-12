train = read.csv("train.csv")
train0 = train[,-128]
test = read.csv("test.csv")
data = rbind(train0, test)
par(mfrow = c(2, 2), mar=c(4,4,2,2))

###### Univerate Analysis
###### Continuous Variables
###### Normailzed Age
hist(data$Ins_Age, xlab = "Normalized Age", ylab = "", probability = TRUE,
     main = "", xlim = extendrange(c(0, 1)), cex.axis = 0.8, cex.lab = 0.8)
lines(density(data$Ins_Age), lwd = 2, col = "firebrick")

###### Normalized Height
hist(data$Ht, xlab = "Normalized Height", ylab = "", probability = TRUE,
     main = "", xlim = extendrange(c(0, 1)), cex.axis = 0.8, cex.lab = 0.8)
lines(density(data$Ht), lwd = 2, col = "firebrick")

###### Normalized Weight
hist(data$Wt, xlab = "Normalized Weight", ylab = "", probability = TRUE,
     main = "", xlim = extendrange(c(0, 1)), cex.axis = 0.8, cex.lab = 0.8)
lines(density(data$Wt), lwd = 2, col = "firebrick")

###### Normalized BMI
hist(data$BMI, xlab = "Normalizes BMI", ylab = "", probability = TRUE,
     main = "", xlim = extendrange(c(0, 1)), cex.axis = 0.8, cex.lab = 0.8)
lines(density(data$BMI), lwd = 2, col = "firebrick")

###### Summary Statistics
IQR = function(x){
        results = summary(x)
        results[5] - results [2]
}

m1 = c(mean(data$Ins_Age), mean(data$Ht), mean(data$Wt), mean(data$BMI))
m2 = c(median(data$Ins_Age), median(data$Ht), median(data$Wt), median(data$BMI))
d = c(sqrt(var(data$Ins_Age)), sqrt(var(data$Ht)), sqrt(var(data$Wt)), sqrt(var(data$BMI)))
i = c(IQR(data$Ins_Age), IQR(data$Ht), IQR(data$Wt), IQR(data$BMI))
summ = round(rbind(m1, m2, d, i), 4)
dimnames(summ) = list(c("Mean", "Median", "Std Dev", "IQR"), c("Age", "Height", "Weight", "BMI"))
write.csv(summ, "Continuous_Summary.csv")

par(mfrow = c(1, 1))
plot(c(0.9, 1.9, 2.9, 3.9), m1, xlim = c(0.8, 4.2), ylim = c(0, 1), main = "", xaxt = "n",
     xlab = "Age          Height          Weight          BMI", ylab = "", type = "h", col = "red", lwd = 2)
lines(m2, type = "h", col = "firebrick", lwd = 2)
lines(c(1.1, 2.1, 3.1, 4.1), d, type = "h", col = "navyblue", lwd = 2)
lines(c(1.2, 2.2, 3.2, 4.2), i, type = "h", col = 5, lwd = 2)
legend("topright", c("Mean", "Median", "Std Dev", "IQR"), col = c("red", "firebrick", "navyblue", 5), lty = 1, lwd = 2)

###### Categorical Variables
###### Insuredinfo_3
insured_info_3 = table(data$InsuredInfo_3)
barplot(insured_info_3[order(insured_info_3)], horiz = TRUE, las = 1,
        col = c("bisque3", "bisque1"), xlim = c(0, 20000), cex.main = 0.9, 
        cex.axis = 0.8, xlab = "Frequency", main = "Insured Info 3")

###### Insurance_History_1
insurance_history_1 = table(data$Insurance_History_1)
barplot(insurance_history_1[order(insurance_history_1)], horiz = TRUE, las = 1,
        col = c("bisque1", "bisque3"), cex.main = 0.9,
        cex.axis = 0.8, xlab = "Frequency", main = "Insurance History 1")

###### Response Variable
par(mfrow = c(1, 1))
response = table(train$Response)
barplot(response, col = c("bisque1", "bisque3"), ylab = "Frequency",
        ylim = c(0, 20000), main = "Response")

###############################################################################

###### Bivariate Analysis
r1 = train[train$Response==1, ]
r2 = train[train$Response==2, ]
r3 = train[train$Response==3, ]
r4 = train[train$Response==4, ]
r5 = train[train$Response==5, ]
r6 = train[train$Response==6, ]
r7 = train[train$Response==7, ]
r8 = train[train$Response==8, ]

###### Age
plot(x = sort(unique(r1$Ins_Age)), y = as.vector(table(r1$Ins_Age)), type = "l", ylim = c(0, 700),
     main = "Normalized Age vs. Risk Level", xlab = "Age", ylab = "Count", col = "red")
lines(x = sort(unique(r2$Ins_Age)), y = as.vector(table(r2$Ins_Age)), type = "l", col = 2)
lines(x = sort(unique(r3$Ins_Age)), y = as.vector(table(r3$Ins_Age)), type = "l", col = 3)
lines(x = sort(unique(r4$Ins_Age)), y = as.vector(table(r4$Ins_Age)), type = "l", col = 4)
lines(x = sort(unique(r5$Ins_Age)), y = as.vector(table(r5$Ins_Age)), type = "l", col = 5)
lines(x = sort(unique(r6$Ins_Age)), y = as.vector(table(r6$Ins_Age)), type = "l", col = 6)
lines(x = sort(unique(r7$Ins_Age)), y = as.vector(table(r7$Ins_Age)), type = "l", col = 7)
lines(x = sort(unique(r8$Ins_Age)), y = as.vector(table(r8$Ins_Age)), type = "l", col = 8)
legend("topright", legend = 1:8, col = c("red", 2:8), lty = 1, cex = 0.7)

###### Ht
plot(x = sort(unique(r1$Ht)), y = as.vector(table(r1$Ht)), type = "l", ylim = c(0, 2000),
     main = "Normalized Height vs. Risk Level", xlab = "Height", ylab = "Count", col = "red")
lines(x = sort(unique(r2$Ht)), y = as.vector(table(r2$Ht)), type = "l", col = 2)
lines(x = sort(unique(r3$Ht)), y = as.vector(table(r3$Ht)), type = "l", col = 3)
lines(x = sort(unique(r4$Ht)), y = as.vector(table(r4$Ht)), type = "l", col = 4)
lines(x = sort(unique(r5$Ht)), y = as.vector(table(r5$Ht)), type = "l", col = 5)
lines(x = sort(unique(r6$Ht)), y = as.vector(table(r6$Ht)), type = "l", col = 6)
lines(x = sort(unique(r7$Ht)), y = as.vector(table(r7$Ht)), type = "l", col = 7)
lines(x = sort(unique(r8$Ht)), y = as.vector(table(r8$Ht)), type = "l", col = 8)
legend("topright", legend = 1:8, col = c("red", 2:8), lty = 1, cex = 0.7)

###### Wt
plot(x = sort(unique(r1$Wt)), y = as.vector(table(r1$Wt)), type = "l", ylim = c(0, 900),
     main = "Normalized Weight vs. Risk Level", xlab = "Weight", ylab = "Count", col = "red")
lines(x = sort(unique(r2$Wt)), y = as.vector(table(r2$Wt)), type = "l", col = 2)
lines(x = sort(unique(r3$Wt)), y = as.vector(table(r3$Wt)), type = "l", col = 3)
lines(x = sort(unique(r4$Wt)), y = as.vector(table(r4$Wt)), type = "l", col = 4)
lines(x = sort(unique(r5$Wt)), y = as.vector(table(r5$Wt)), type = "l", col = 5)
lines(x = sort(unique(r6$Wt)), y = as.vector(table(r6$Wt)), type = "l", col = 6)
lines(x = sort(unique(r7$Wt)), y = as.vector(table(r7$Wt)), type = "l", col = 7)
lines(x = sort(unique(r8$Wt)), y = as.vector(table(r8$Wt)), type = "l", col = 8)
legend("topright", legend = 1:8, col = c("red", 2:8), lty = 1, cex = 0.7)

###### BMI
boxplot(BMI~Response, data = train, main = "Normalized Weight vs. Risk Level", xlab = "Response", ylab = "BMI", 
        ylim = c(0, 1), col = c("red",2:8), border = adjustcolor("black", alpha.f = 0.5), names = 1:8)

###############################################################################

###### Multivariate Analysis
library(nnet)
library(knitr)
library(stargazer)

###### Continuous Variables with Response
model_cont = multinom(Response ~ Ins_Age + Ht + Wt + BMI, data = train)

###### summary(model_age)
z_cont = summary(model_cont)$coefficients / summary(model_cont)$standard.errors
p_cont = round((1 - pnorm(abs(z_cont))) * 2, 4)
p = kable(p_cont)
write.csv(p_cont, "MLR_p_values.csv")
