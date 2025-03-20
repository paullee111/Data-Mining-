##iris
data(iris)
str(iris)
View(iris)
##apply()
##column.sum
apply(iris[,1:4], 2, sum)
apply(iris[,1:4], 2, mean)
apply(iris[,1:4], 2, min)
summary(iris[,1:4])
lapply(iris[,1:4], sum)
sapply(iris[1:4], min)
sapply(iris[1:4], mean)
##csv
inf<-read.csv("C:/Users/user/Desktop/paul-data mining/infant.csv")
inf
View(inf)
setwd("C:/Users/user/Desktop/paul-data mining")
str(inf)
plot(inf$age,inf$weight)
head(inf)##앞에서 6개 보여주기
lm(inf$age~inf$weight, data = inf)##linear regression
fit<-lm(inf$age~inf$weight, data = inf)
abline(fit)
summary(fit)

library(vcd)
help(package = "vcd")
head(Arthritis)
str(Arthritis)
names(Arthritis)
dim(Arthritis)
length(Arthritis)
colnames(Arthritis)
##csv
data(Hitters)
colnames(Hitters)
Hitters<-read.csv("C:/Users/user/Desktop/paul-data mining/Hitters.csv")
head(Hitters)
dim(Hitters)
str(Hitters)
help(Hitters)
## 결측치 확인
is.na(Hitters)
##결측치 제거
N_Hitters<-na.omit(Hitters)
is.na(N_Hitters)
dim(N_Hitters)
## 결측치값 대체
library(VIM)
S_Hitters<-kNN(Hitters,k=3)
dim(S_Hitters)
dim(Hitters)
View(S_Hitters)
S_mean<-mean(S_Hitters$Salary)
S_mean
