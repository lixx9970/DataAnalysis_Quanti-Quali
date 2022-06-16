View(melanoma_data)
M<-melanoma_data
names(M)
attach(M)
levels(Tumour)
levels(Site)
t=factor(Tumour)
s=factor(Site)
out1<-glm(Count~Tumour+Site,family = poisson)
out2<-glm(Count~Tumour*Site,family = poisson)


out3<-glm(Count~0+Tumour*Site,family = poisson)
out4<-glm(Count~0+Tumour+Site,family = poisson)

summary(out1)
summary(out2)
summary(out3)
summary(out4)
anova(out4,out3,test="LRT")

library(car)
Anova(out3,test.statistic = "LR",type=2)
X2<-sum(residuals(out3,type=c("pearson"))**2)
X2
anova(out1)
anova(out2)

M$h0 <- predict(out4, M, type="response")
xtabs(h0 ~ Tumour + Site, M)

M$h1 <- predict(out3, M, type="response")
xtabs(h1 ~ Tumour + Site, M)


