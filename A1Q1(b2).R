x1<-glm(time~u,family = gaussian(link = "identity"))
summary(x1)
x2<-glm(time~u+lot,family = gaussian(link = "identity"))
summary(x2)
x3<-glm(time~log(u),family = gaussian(link = "identity"))
summary(x3)
x4<-glm(time~log(u)+lot,family = gaussian(link = "identity"))
summary(x4)
x5<-glm(time~log(u)*lot,family = gaussian(link = "identity"))
summary(x5)


par(mfrow=c(2,3))

plot(x1$linear.predictors, std.residuals(x1), type="n", xlab="Linear Predictors", ylab="Studentised Deviance Residuals")
abline(h=c(qt(0.025,x1$df.residual),0,qt(0.975,x1$df.residual)), lty=2)
title("Standardised Residuals vs Fitted Values", sub="Clotting data identity normal GLM: Time ~ u(Concentration)")
points(x1$linear.predictors[lot==1], std.residuals(x1)[lot==1],pch="1")
points(x1$linear.predictors[lot==2], std.residuals(x1)[lot==2],pch="2")
qqnorm(std.residuals(x1), sub="Clotting data identity normal GLM: Time ~ u(Concentration)")
qqline(std.residuals(x1), lty=2)
abline(0,1)
temp <- qqnorm(std.residuals(x1), plot.it=F)
plot(x1, which=4, sub="Clotting data identity normal GLM: Time ~ u(Concentration)")
#######################################################################################
plot(x2$linear.predictors, std.residuals(x2), type="n", xlab="Linear Predictors", ylab="Studentised Deviance Residuals")
abline(h=c(qt(0.025,x2$df.residual),0,qt(0.975,x2$df.residual)), lty=2)
title("Standardised Residuals vs Fitted Values", sub="Clotting data identity normal GLM: Time ~ u + lot")
points(x2$linear.predictors[lot==1], std.residuals(x2)[lot==1],pch="1")
points(x2$linear.predictors[lot==2], std.residuals(x2)[lot==2],pch="2")
qqnorm(std.residuals(x2), sub="Clotting data identity normal GLM: Time ~ u + lot")
qqline(std.residuals(x2), lty=2)
abline(0,1)
temp <- qqnorm(std.residuals(x2), plot.it=F)
plot(x2, which=4, sub="Clotting data identity normal GLM: Time ~ u + lot")
########################################################################################
plot(x3$linear.predictors, std.residuals(glm3), type="n", xlab="Linear Predictors", ylab="Studentised Deviance Residuals")
abline(h=c(qt(0.025,glm3$df.residual),0,qt(0.975,glm3$df.residual)), lty=2)
title("Standardised Residuals vs Fitted Values", sub="Clotting data identity normal GLM: Time ~ log(u)(Concentration)")
points(glm3$linear.predictors[lot==1], std.residuals(glm3)[lot==1],pch="1")
points(glm3$linear.predictors[lot==2], std.residuals(glm3)[lot==2],pch="2")
qqnorm(std.residuals(glm3), sub="Clotting data identity normal GLM: Time ~ log(u)(Concentration)")
qqline(std.residuals(glm3), lty=2)
abline(0,1)
temp <- qqnorm(std.residuals(glm3), plot.it=F)
plot(glm3, which=4, sub="Clotting data identity normal GLM: Time ~ log(u)(Concentration)")
#######################################################################################
plot(x4$linear.predictors, std.residuals(x4), type="n", xlab="Linear Predictors", ylab="Studentised Deviance Residuals")
abline(h=c(qt(0.025,x4$df.residual),0,qt(0.975,x4$df.residual)), lty=2)
title("Standardised Residuals vs Fitted Values", sub="Clotting data identity normal GLM: Time ~ log(u)(Concentration) + lot")
points(x4$linear.predictors[lot==1], std.residuals(x4)[lot==1],pch="1")
points(x4$linear.predictors[lot==2], std.residuals(x4)[lot==2],pch="2")
qqnorm(std.residuals(x4), sub="Clotting data identity normal GLM: Time ~ log(u)(Concentration) + lot")
qqline(std.residuals(x4), lty=2)
abline(0,1)
temp <- qqnorm(std.residuals(x4), plot.it=F)
plot(x4, which=4, sub="Clotting data identity normal GLM: Time ~ log(u)(Concentration) + lot")
#######################################################################################
plot(x5$linear.predictors, std.residuals(x5), type="n", xlab="Linear Predictors", ylab="Studentised Deviance Residuals")
abline(h=c(qt(0.025,x5$df.residual),0,qt(0.975,x5$df.residual)), lty=2)
title("Standardised Residuals vs Fitted Values", sub="Clotting data identity normal GLM: Time ~ log(u)(Concentration) * lot")
points(x5$linear.predictors[lot==1], std.residuals(x5)[lot==1],pch="1")
points(x5$linear.predictors[lot==2], std.residuals(x5)[lot==2],pch="2")
qqnorm(std.residuals(x5), sub="Clotting data identity normal GLM: Time ~ log(u)(Concentration) * lot")
qqline(std.residuals(x5), lty=2)
abline(0,1)
temp <- qqnorm(std.residuals(x5), plot.it=F)
plot(x5, which=4, sub="Clotting data identity normal GLM: Time ~ log(u)(Concentration) * lot")



with(summary(x1),1-deviance/null.deviance)
with(summary(x2),1-deviance/null.deviance)
with(summary(x3),1-deviance/null.deviance)
with(summary(x4),1-deviance/null.deviance)
with(summary(x5),1-deviance/null.deviance)



anova(x2)
anova(x3)
anova(x4)
summary(x2)$dispersion
df<-x2$df.residual
df
x2$deviance/df
x2$deviance/summary(x2)$dispersion
c(qchisq(0.025,df),qchisq(0.975,df))
scaled.dev<-anova(x2)$Deviance/summary(x2)$dispersion
chisq.pvalues<-1-pchisq(scaled.dev,anova(x2)$Df)
cbind(anova(x2),"Scaled Dev"=scaled.dev,"Pr(>Chi)"=chisq.pvalues)
6.4013880/summary(x2)$dispersion
round(summary(x2)$coefficients,8)
