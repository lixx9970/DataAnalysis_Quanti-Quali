glm1<-glm(time~u,family = Gamma(link = "identity"))
summary(glm1)
glm2<-glm(time~u+lot,family = Gamma(link = "identity"))
summary(glm2)
glm3<-glm(time~log(u),family = Gamma(link = "identity"))
summary(glm3)
glm4<-glm(time~log(u)+lot,family = Gamma(link = "identity"))
summary(glm4)
glm5<-glm(time~log(u)*lot,family = Gamma(link = "identity"))
summary(glm5)


std.residuals <- function(model, type="deviance"){
  # Function to standardise residuals from a GLM model object
  # Produces standardised deviance residuals, unless type="pearson" requested
  std.error <- sqrt(summary(model)$dispersion * (1 - influence(model)$hat))
  std.res <- residuals(model)/std.error
  if (type=="pearson") std.res <- residuals(model, "pearson")/std.error
  std.res
}

par(mfrow=c(2,3))

plot(glm1$linear.predictors, std.residuals(glm1), type="n", xlab="Linear Predictors", ylab="Studentised Deviance Residuals")
abline(h=c(qt(0.025,glm1$df.residual),0,qt(0.975,glm1$df.residual)), lty=2)
title("Standardised Residuals vs Fitted Values", sub="Clotting data identity gamma GLM: Time ~ u(Concentration)")
points(glm1$linear.predictors[lot==1], std.residuals(glm1)[lot==1],pch="1")
points(glm1$linear.predictors[lot==2], std.residuals(glm1)[lot==2],pch="2")
qqnorm(std.residuals(glm1), sub="Clotting data identity gamma GLM: Time ~ u(Concentration)")
qqline(std.residuals(glm1), lty=2)
abline(0,1)
temp <- qqnorm(std.residuals(glm1), plot.it=F)
plot(glm1, which=4, sub="Clotting data identity gamma GLM: Time ~ u(Concentration)")
#######################################################################################
plot(glm2$linear.predictors, std.residuals(glm2), type="n", xlab="Linear Predictors", ylab="Studentised Deviance Residuals")
abline(h=c(qt(0.025,glm2$df.residual),0,qt(0.975,glm2$df.residual)), lty=2)
title("Standardised Residuals vs Fitted Values", sub="Clotting data identity gamma GLM: Time ~ u + lot")
points(glm2$linear.predictors[lot==1], std.residuals(glm2)[lot==1],pch="1")
points(glm2$linear.predictors[lot==2], std.residuals(glm2)[lot==2],pch="2")
qqnorm(std.residuals(glm2), sub="Clotting data identity gamma GLM: Time ~ u + lot")
qqline(std.residuals(glm2), lty=2)
abline(0,1)
temp <- qqnorm(std.residuals(glm2), plot.it=F)
plot(glm2, which=4, sub="Clotting data identity gamma GLM: Time ~ u + lot")
########################################################################################
plot(glm3$linear.predictors, std.residuals(glm3), type="n", xlab="Linear Predictors", ylab="Studentised Deviance Residuals")
abline(h=c(qt(0.025,glm3$df.residual),0,qt(0.975,glm3$df.residual)), lty=2)
title("Standardised Residuals vs Fitted Values", sub="Clotting data identity gamma GLM: Time ~ log(u)(Concentration)")
points(glm3$linear.predictors[lot==1], std.residuals(glm3)[lot==1],pch="1")
points(glm3$linear.predictors[lot==2], std.residuals(glm3)[lot==2],pch="2")
qqnorm(std.residuals(glm3), sub="Clotting data identity gamma GLM: Time ~ log(u)(Concentration)")
qqline(std.residuals(glm3), lty=2)
abline(0,1)
temp <- qqnorm(std.residuals(glm3), plot.it=F)
plot(glm3, which=4, sub="Clotting data identity gamma GLM: Time ~ log(u)(Concentration)")
#######################################################################################
plot(glm4$linear.predictors, std.residuals(glm4), type="n", xlab="Linear Predictors", ylab="Studentised Deviance Residuals")
abline(h=c(qt(0.025,glm4$df.residual),0,qt(0.975,glm4$df.residual)), lty=2)
title("Standardised Residuals vs Fitted Values", sub="Clotting data identity gamma GLM: Time ~ log(u)(Concentration) + lot")
points(glm4$linear.predictors[lot==1], std.residuals(glm4)[lot==1],pch="1")
points(glm4$linear.predictors[lot==2], std.residuals(glm4)[lot==2],pch="2")
qqnorm(std.residuals(glm4), sub="Clotting data identity gamma GLM: Time ~ log(u)(Concentration) + lot")
qqline(std.residuals(glm4), lty=2)
abline(0,1)
temp <- qqnorm(std.residuals(glm4), plot.it=F)
plot(glm4, which=4, sub="Clotting data identity gamma GLM: Time ~ log(u)(Concentration) + lot")
#######################################################################################
plot(glm5$linear.predictors, std.residuals(glm5), type="n", xlab="Linear Predictors", ylab="Studentised Deviance Residuals")
abline(h=c(qt(0.025,glm5$df.residual),0,qt(0.975,glm5$df.residual)), lty=2)
title("Standardised Residuals vs Fitted Values", sub="Clotting data identity gamma GLM: Time ~ log(u)(Concentration) * lot")
points(glm5$linear.predictors[lot==1], std.residuals(glm5)[lot==1],pch="1")
points(glm5$linear.predictors[lot==2], std.residuals(glm5)[lot==2],pch="2")
qqnorm(std.residuals(glm5), sub="Clotting data identity gamma GLM: Time ~ log(u)(Concentration) * lot")
qqline(std.residuals(glm5), lty=2)
abline(0,1)
temp <- qqnorm(std.residuals(glm5), plot.it=F)
plot(glm5, which=4, sub="Clotting data identity gamma GLM: Time ~ log(u)(Concentration) * lot")


with(summary(glm1),1-deviance/null.deviance)
with(summary(glm2),1-deviance/null.deviance)
with(summary(glm3),1-deviance/null.deviance)
with(summary(glm4),1-deviance/null.deviance)
with(summary(glm5),1-deviance/null.deviance)


glm2
anova(glm2)
anova(glm3)
anova(glm4)
anova(glm5)
summary(glm2)$dispersion
df<-glm2$df.residual
df
glm2$deviance/df
glm2$deviance/summary(glm2)$dispersion
c(qchisq(0.025,df),qchisq(0.975,df))
scaled.dev<-anova(glm2)$Deviance/summary(glm2)$dispersion
chisq.pvalues<-1-pchisq(scaled.dev,anova(glm2)$Df)
cbind(anova(glm2),"Scaled Dev"=scaled.dev,"Pr(>Chi)"=chisq.pvalues)
6.4013880/summary(out3)$dispersion
round(summary(out3)$coefficients,8)

