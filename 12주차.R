##[1]독립변수 범주화

##이원 분산분석
#[1]tapply(종속변수, 독립변수, 통계량)
attach(tooth)
ToothGrowth
attach(ToothGrowth)
levels(ToothGrowth$supp)
tapply(len,supp,mean)
tapply(len, list(supp,dose),mean)
aggregate(len, list(supp,dose),mean)
#[2]독립변수 범주형으로 변환
ToothGrowth$supp<-factor(ToothGrowth$supp)
ToothGrowth$dose<-factor(ToothGrowth$dose)
levels(ToothGrowth$dose)
#[3]회귀분석 lm()
lm(len~supp*dose,data=ToothGrowth)
#[4]분산분석 anova() 실시
out<-lm(len~supp*dose,data=ToothGrowth)
out<-lm(len~supp*dose,data=ToothGrowth)
anova(out)
out<-lm(len~supp+dose+supp*dose,data=ToothGrowth)
anova(out)
#[5]다중비교
##Dunnett
out1<-lm(len~dose, data=ToothGrowth)
dunnett<-glht(out1,linfct=mcp(dose="Dunnett"))
dunnett<-glht(out1,linfct=mcp(dose="Dunnett"))
summary(dunnett)

out2<-lm(len~supp,data=ToothGrowth)
dunnett2<-glht(out2,linfct=mcp(supp="Dunnett"))
summary(dunnett2)
##Tukey
out3<-lm(len~supp,data=ToothGrowth)
tukey1<-glht(out3,linfct=mcp(dose="Tukey"))
summary(tukey1)

out4<-lm(len~supp,data=ToothGrowth)
tukey2<-glht(out4,linfct=mcp(dose="Tukey"))
summary(tukey2)
