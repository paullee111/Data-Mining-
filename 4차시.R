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


data(trees)
trees
str(trees)
dim(trees)
is.na(trees)##결측치 확인
sum(is.na(trees$Girth))
sum(is.na(trees$Volume))
sum(is.na(trees$Height))
summary(trees)
hist(trees$Girth)
hist(trees$Height)
hist(trees$Volume)
## 줄기 잎 그림
stem(trees$Girth)
stem(trees$Height)
stem(trees$Volume)
boxplot(trees$Girth)
boxplot(trees$Height)
boxplot(trees$Volume)
boxplot(trees)
##correlation coefficient 상관관계
cor(trees)
##plot(x,y)
plot(trees$Girth,trees$Height)
plot(trees$Girth,trees$Volume)
## linear Regression 1차 직선회귀모델
## lm(y ~ x)
fit<-lm(trees$Volume~trees$Girth, data = trees)
abline(fit)
summary(fit)
predict(fit)

fit2<-lm(trees$Volume~trees$Height, data = trees)
plot(trees$Height,trees$Volume)
abline(fit2)
summary(fit2)
predict(fit2)


fit3<-lm(trees$Girth~trees$Height, data=trees)
plot(trees$Height,trees$Girth)
abline(fit3)
summary(fit3)
predict(fit3)
##정규성 점검
library(psych)
qqnorm(trees$Girth)
qqline(trees$Girth)

qqnorm(trees$Height)
qqline(trees$Height)

qqnorm(trees$Volume)
qqline(trees$Volume)

describe(trees)
##skew:왜도. 기울기 정도, 음수는 오른쪽 양수는 왼쪽으로 치우쳐있음
##kurtosis : 첨도. 양수면 뾰족 음수면 납작 

shapiro.test(trees$Girth)
shapiro.test(trees$Height)
shapiro.test(trees$Volume)