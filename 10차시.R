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

data(airquality)
str(airquality)
dim(airquality)
help(airquality)
is.na(airquality)
sum(is.na(airquality))
air<-na.omit(airquality)
is.na(air)
dim(air)
summary(air)

##5주차 데이터 다루기

#정규분포 그려보기
pairs(trees)
cor(trees)
#상관관계
#다중 회귀 분석 실시

# dplyr 패키지 설치 및 로드하기(p.207)
install.packages("dplyr")
library(dplyr)


# mtcars 데이터 세트 구조 확인하기(p.208)
nrow(mtcars)
str(mtcars)


# 조건에 맞는 데이터 추출하기(p.209)
filter(mtcars, cyl == 4)


# 두 가지 조건에 맞는 데이터를 필터링하기(p.209)
filter(mtcars, cyl >= 6 & mpg > 20)


# 지정한 변수만 추출하기(p.210)
q<-head(select(mtcars, cyl,hp))
arrange(q,desc(hp))

# 오름차순 정렬하기(p.211)
head(arrange(mtcars, wt))


# 오름차순 정렬한 후 내림차순 정렬하기(p.212)
head(arrange(mtcars, mpg, desc(wt)))


# 새로운 열 추가하기(p.213)
head(mutate(mtcars, years = "1974"))

head(mutate(mtcars, mpg_rank = rank(mpg)))


# 중복 값 제거하기(p.214)
distinct(mtcars, cyl)

distinct(mtcars, gear)


# 여러 개 열에서 중복 값 제거하기(p.215)
distinct(mtcars, cyl, gear)


# 데이터 요약하기(p.216)
summarise(mtcars, cyl_mean = mean(cyl), cyl_min = min(cyl), cyl_max = max(cyl))

summarise(mtcars, mean(cyl), min(cyl), max(cyl))


# 그룹별로 요약하기(p.217)
gr_cyl <- group_by(mtcars, cyl)
summarise(gr_cyl, n())

gr_cyl <- group_by(mtcars, cyl)
summarise(gr_cyl, n_distinct(gear))


# 샘플 데이터 10개 추출하기(p.218)
sample_n(mtcars, 10)


# 전체 데이터의 20%를 샘플로 추출하기(p.219)
sample_frac(mtcars, 0.2)


# 파이프 연산자로 그룹별 요약하기(p.220)
group_by(mtcars, cyl) %>% summarise(n())


# 파이프 연산자 없이 순위 기준으로 정렬하기(p.220)
mp_rank <- mutate(mtcars, mpg_rank = rank(mpg))
arrange(mp_rank, mpg_rank)


# 파이프 연산자를 사용하여 순위 기준으로 정렬하기(p.221)
mutate(mtcars, mpg_rank = rank(mpg)) %>% arrange(mpg_rank)

##문제 cyl 별로 그룹화하여 마력(hp)가 가장 높은 차 찾기
mp_rank<-group_by(mtcars, cyl,hp)
mp_rank <- mutate(mtcars, hp_rank = rank(hp),cyl=rank(cyl))
mp_rank<-arrange(mp_rank, cyl,desc(hp_rank))
mp_rank
head(mp_rank)


##중간고사 이후 수업

##상관분석
#공분산 : 두 변수가 어느정도 결합되어있는지 나타내는 정도
#상관계수 : 공분산을 표준화한 값
#상관관계분석유형:
  #피어선 상관분석:수량 변수간 상관관계 분석
  #스피어만 상관관계:서열 변수간 상관관계분석
  #캔달타우 비모수 검정:서열관계에 대한 비모수 검정
trees
plot(trees$Girth,trees$Height)
fit3<-lm(trees$Girth~trees$Height, data=trees)
abline(fit3)
pairs(trees)
cov(trees)
cor(trees)
cor.test(trees$Girth,trees$Height)
##회귀분석 : 하나 이상의 독립변수로부터 종속변수를 예측하는 방법
#회귀분석의 목적:(1)종속변수와 연관된 종속변수를 발견 (2)관련된 변수들의 관계의 형태를 서술 (3)독립변수로부터 종속변수를 예측하는 방정식 제시
##모형의 유형:
  #일반선형회귀모형(최소제곱회귀모형)
  #일반화선형회귀모형(로지스틱 회귀분석,포아송회귀분석)
  #비선형회귀모형
#모형의 타당성 분석
par("mar")
par(mar=c(1,1,1,1))
par("mar")
## 10주차 수업
class(state.x77)
attributes(state.x77)#배열유형
#dataframe으로 바꾼다.
states2<-data.frame(state.x77[,c("Income","Illiteracy","Murder","Frost","Population")])
class(states2)
head(states2)
library(psych)
cor.plot(states2)
dev.off()#초기화
fit1<-lm(formula = Murder~Population+Income+Illiteracy+Frost,data=states)
fit2<-lm(formula = Murder~Population+Illiteracy+Income,data=states2)
anova(fit1,fit2)
AIC(fit1,fit2)
step(fit1,fit2)

#과제
#1.
trees
class(trees)
cor(trees)
cor.test(trees$Girth,trees$Volume)
cor.test(trees$Girth,trees$Height)
##Girth와Volume이 강한 상관관계를 가지는 두 변수이다.
#2.
##회귀분석 : 하나 이상의 독립변수로부터 종속변수를 예측하는 방법
#회귀분석의 목적:(1)종속변수와 연관된 종속변수를 발견 (2)관련된 변수들의 관계의 형태를 서술 (3)독립변수로부터 종속변수를 예측하는 방정식 제시
#타당성 검토 항목 : 
  #잔차의 선형성 : 잔차의 평균은 0
  #잔차의  등분산성 : 잔차의 분산이 일정해야한다. 
  #잔차의 정규성 : 정규분포를 따라야 한다. 
  #독립성 : 잔차는 영향을 받으면 안되고 독립적이어야 한다. 
#3.
#데이터가 정규 분포가 아닐 때,log ,sqrt() 로종속변수(y) 를 변환시켜 선형으로 만든다.
  #1)종속변수의 정규성을 살펴본다. 
  #2)회귀식 모데을 세워 회귀 모형을 검정한다. 
  #3)종속변수를log ,sqrt() 로 변환시켜 각각 회귀모형을 세우고 모형을 검정한다. 
  #4)결론을 도출한다.
#4.
data(cars)
class(cars)
head(cars)
str(cars)
is.na(cars)
na.omit(cars)
hist(cars$dist)#정규성 검정
qqnorm(cars$dist)
qqline(cars$dist,col='red')
# Shapiro-wilk test(most common)
shapiro.test(cars$dist)
#정규성이 아니다,변환해야한다
plot(log(cars$dist)~cars$speed)
fit2<-lm(log(cars$dist)~cars$speed)
summary(fit2)
abline(fit2)
plot(sqrt(cars$dist)~cars$speed)
fit3<-lm(sqrt(cars$dist)~cars$speed)
summary(fit3)
#sqrt변환이 더 적절함
#5.
class(state.x77)
state.x77
states<-data.frame(state.x77[,c("Income","Illiteracy","Murder","Frost")])
class(states)
head(states)
library(psych)
cor.plot(states)
fit<-lm(states$Murder~states$Income+states$Illiteracy)#모든변수모형
summary(fit)
states2<-data.frame(state.x77[,c("Population","Income","Illiteracy","Murder","Frost")])
fit2<-lm(states2$Murder~states2$Population+states2$Income+states2$Illiteracy+states2$Frost,data=states2)
op<-par(mfrow=c(2,2))
plot(fit2)
par(op)
##이상치 조사
qqplot(fit2,labels=row.names(states2),simulate=T)
rownames(states2)
fit3<-lm(states2$Murder~states2$Population+states2$Income+states2$Illiteracy+states2$Frost, data=states2[-28,])
summary(fit3)
anova(fit,fit2)
AIC(fit,fit2)
library(MASS)
stepAIC(fit2)
#AIC가 가장 적은 모델은 인구와 문맹률과 자살과의 관계를 나타낸 3번째이다. 
#첫번째 모델에서 수입과 1년중 추운날의 수는 자살률에 영향이 낮은 변수로 나타났음.