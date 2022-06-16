C=read.table(file.choose())
names(C)
View(C)

lot=factor(lot)
out1<-glm(time~u,family = Gamma(link = "log"))

#Test with first model of time as response and u as covariate
# out1=glm(time~u,family = Gamma(link="log"))
# summary(out1)
# Call:
#   glm(formula = time ~ u, family = Gamma(link = "log"))
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -0.5629  -0.3926  -0.1191   0.1909   1.0162  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   3.9796     0.1830  21.749 2.62e-13 ***
#   u            -0.0157     0.0036  -4.361 0.000485 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for Gamma family taken to be 0.2293923)
# 
# Null deviance: 7.7087  on 17  degrees of freedom
# Residual deviance: 3.0431  on 16  degrees of freedom
# AIC: 143.21
# 
# Number of Fisher Scoring iterations: 7

lp1<-3.9796-0.0157*u
res1<-residuals(out1,type=c("deviance"))
plot(lp1,res1)
plot(lp1,res1,col=c("red","blue")[lot])
plot(out1,1,col=c("red","blue")[lot])

#Test with second case of time as response and log(u) as covariate
out2<-glm(time~log(u),family = Gamma(link = "log"))
summary(out2)

# Call:
#   glm(formula = time ~ log(u), family = Gamma(link = "log"))
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -0.41322  -0.28555  -0.00434   0.16201   0.50548  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.25197    0.24778  21.196 3.90e-13 ***
#   log(u)      -0.58874    0.07201  -8.175 4.18e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for Gamma family taken to be 0.08257869)
# 
# Null deviance: 7.7087  on 17  degrees of freedom
# Residual deviance: 1.3073  on 16  degrees of freedom
# AIC: 127.71
# 
# Number of Fisher Scoring iterations: 5

lp2=5.25197-0.58874*log(u)
res2=residuals(out2,type = c("deviance"))
plot(lp2,res2)
plot(lp2,res2,col=c("red","blue")[lot])


#Test with third case of time as response and log(u) and factor lot as covariates

out3<-glm(time~log(u)+lot,family = Gamma(link = "log"))
lot1<-as.numeric(lot)
summary(out3)

# Call:
#   glm(formula = time ~ log(u) + lot, family = Gamma(link = "log"))
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -0.17470  -0.11596  -0.04281   0.06919   0.27749  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.44660    0.13453   40.48  < 2e-16 ***
#   log(u)      -0.58476    0.03772  -15.50 1.22e-10 ***
#   lot2        -0.47034    0.07095   -6.63 8.02e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for Gamma family taken to be 0.02265072)
# 
# Null deviance: 7.7087  on 17  degrees of freedom
# Residual deviance: 0.3211  on 15  degrees of freedom
# AIC: 104.28
# 
# Number of Fisher Scoring iterations: 5

lp3<-5.44660-0.58476*log(u)-0.47034*(lot1-1)
res3<-residuals(out3,type = c("deviance"))
plot(lp3,res3)
plot(lp3,res3,col=c("red","blue")[lot])

#Test with fourth case of time as response and log(u) and lot with its own intercept as covariate

out4<-glm(time~log(u)*lot,family = Gamma(link = "log"))
summary(out4)

# Call:
#   glm(formula = time ~ log(u) * lot, family = Gamma(link = "log"))
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -0.16943  -0.11534  -0.05439   0.07319   0.24588  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.50323    0.18794  29.282 5.83e-14 ***
#   log(u)      -0.60192    0.05462 -11.020 2.77e-08 ***
#   lot2        -0.58447    0.26578  -2.199   0.0452 *  
#   log(u):lot2  0.03448    0.07725   0.446   0.6621    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for Gamma family taken to be 0.02375284)
# 
# Null deviance: 7.70867  on 17  degrees of freedom
# Residual deviance: 0.31576  on 14  degrees of freedom
# AIC: 105.97
# 
# Number of Fisher Scoring iterations: 5

lp4=5.50323-0.60192*log(u)-(lot1-1)*(0.58447-0.03448*log(u))
res4=residuals(out4,type=c("deviance"))
plot(lp4,res4)
plot(lp4,res4,col=c("red","blue")[lot])

#Alternative plots to test the goodness of fit(Similar to lp vs. Res)

std.residuals <- function(model, type="deviance"){
  # Function to standardise residuals from a GLM model object
  # Produces standardised deviance residuals, unless type="pearson" requested
  std.error <- sqrt(summary(model)$dispersion * (1 - influence(model)$hat))
  std.res <- residuals(model)/std.error
  if (type=="pearson") std.res <- residuals(model, "pearson")/std.error
  std.res
}

par(mfrow=c(2,3))

plot(out1$linear.predictors, std.residuals(out1), type="n", xlab="Linear Predictors", ylab="Studentised Deviance Residuals")
abline(h=c(qt(0.025,out1$df.residual),0,qt(0.975,out1$df.residual)), lty=2)
title("Standardised Residuals vs Fitted Values", sub="Clotting data log gamma GLM: Time ~ u(Concentration)")
points(out1$linear.predictors[lot==1], std.residuals(out1)[lot==1],pch="1")
points(out1$linear.predictors[lot==2], std.residuals(out1)[lot==2],pch="2")
qqnorm(std.residuals(out1), sub="Clotting data log gamma GLM: Time ~ u(Concentration)")
qqline(std.residuals(out1), lty=2)
# abline(0,1)
temp <- qqnorm(std.residuals(out1), plot.it=F)
plot(out1, which=4, sub="Clotting data log gamma GLM: Time ~ u(Concentration)")
#######################################################################################
plot(out2$linear.predictors, std.residuals(out2), type="n", xlab="Linear Predictors", ylab="Studentised Deviance Residuals")
abline(h=c(qt(0.025,out2$df.residual),0,qt(0.975,out2$df.residual)), lty=2)
title("Standardised Residuals vs Fitted Values", sub="Clotting data log gamma GLM: Time ~ log(u)(Concentration)")
points(out2$linear.predictors[lot==1], std.residuals(out2)[lot==1],pch="1")
points(out2$linear.predictors[lot==2], std.residuals(out2)[lot==2],pch="2")
qqnorm(std.residuals(out2), sub="Clotting data log gamma GLM: Time ~ log(u)(Concentration)")
qqline(std.residuals(out2), lty=2)
# abline(0,1)
temp <- qqnorm(std.residuals(out2), plot.it=F)
plot(out2, which=4, sub="Clotting data log gamma GLM: Time ~ log(u)(Concentration)")
#######################################################################################
plot(out3$linear.predictors, std.residuals(out3), type="n", xlab="Linear Predictors", ylab="Studentised Deviance Residuals")
abline(h=c(qt(0.025,out3$df.residual),0,qt(0.975,out3$df.residual)), lty=2)
title("Standardised Residuals vs Fitted Values", sub="Clotting data log gamma GLM: Time ~ log(u)(Concentration) + lot")
points(out3$linear.predictors[lot==1], std.residuals(out3)[lot==1],pch="1")
points(out3$linear.predictors[lot==2], std.residuals(out3)[lot==2],pch="2")
qqnorm(std.residuals(out3), sub="Clotting data log gamma GLM: Time ~ log(u)(Concentration) + lot")
qqline(std.residuals(out3), lty=2)
# abline(0,1)
temp <- qqnorm(std.residuals(out3), plot.it=F)
plot(out3, which=4, sub="Clotting data log gamma GLM: Time ~ log(u)(Concentration) + lot")
#######################################################################################
plot(out4$linear.predictors, std.residuals(out4), type="n", xlab="Linear Predictors", ylab="Studentised Deviance Residuals")
abline(h=c(qt(0.025,out4$df.residual),0,qt(0.975,out4$df.residual)), lty=2)
title("Standardised Residuals vs Fitted Values", sub="Clotting data log gamma GLM: Time ~ log(u)(Concentration) * lot")
points(out4$linear.predictors[lot==1], std.residuals(out4)[lot==1],pch="1")
points(out4$linear.predictors[lot==2], std.residuals(out4)[lot==2],pch="2")
qqnorm(std.residuals(out4), sub="Clotting data log gamma GLM: Time ~ log(u)(Concentration) * lot")
qqline(std.residuals(out4), lty=2)
# abline(0,1)
temp <- qqnorm(std.residuals(out4), plot.it=F)
plot(out4, which=4, sub="Clotting data log gamma GLM: Time ~ log(u)(Concentration) * lot")
########################################################################################
plot(out5$linear.predictors, std.residuals(out5), type="n", xlab="Linear Predictors", ylab="Studentised Deviance Residuals")
abline(h=c(qt(0.025,out5$df.residual),0,qt(0.975,out5$df.residual)), lty=2)
title("Standardised Residuals vs Fitted Values", sub="Clotting data log gamma GLM: Time ~ log(u)(Concentration) * lot")
points(out5$linear.predictors[lot==1], std.residuals(out5)[lot==1],pch="1")
points(out5$linear.predictors[lot==2], std.residuals(out5)[lot==2],pch="2")
qqnorm(std.residuals(out5), sub="Clotting data log gamma GLM: Time ~ u + lot")
qqline(std.residuals(out5), lty=2)
# abline(0,1)
temp <- qqnorm(std.residuals(out5), plot.it=F)
plot(out5, which=4, sub="Clotting data log gamma GLM: Time ~ u + lot")

#To test if there is over or under dispersion
summary(out2)$dispersion
out2$deviance/out1$df.residual
out2$deviance
out2$df.residual
c(qchisq(0.025,out2$df.residual),qchisq(0.975,out2$df.residual))

# > out1$deviance/out1$df.residual
# [1] 0.1901921            Note: If we set a hypothesis testing to determine if there is an under or over dispersion, the result is less than 1 and we conclude that there occurs under-dispersion
# > out1$deviance
# [1] 3.043073
# > out1$df.residual
# [1] 16
# > c(qchisq(0.025,out1$df.residual),qchisq(0.975,out1$df.residual))
# [1]  6.907664 28.845351

out2$deviance/out2$df.residual
out3$deviance/out3$df.residual
out4$deviance/out4$df.residual

with(summary(out1),1-deviance/null.deviance)
with(summary(out2),1-deviance/null.deviance)
with(summary(out3),1-deviance/null.deviance)
with(summary(out4),1-deviance/null.deviance)
with(summary(out5),1-deviance/null.deviance)

anova(out2)
out3
anova(out3)
summary(out3)$dispersion
df<-out3$df.residual
df
out3$deviance/df
out3$deviance/summary(out3)$dispersion
c(qchisq(0.025,df),qchisq(0.975,df))
scaled.dev<-anova(out3)$Deviance/summary(out3)$dispersion
chisq.pvalues<-1-pchisq(scaled.dev,anova(out3)$Df)
cbind(anova(out3),"Scaled Dev"=scaled.dev,"Pr(>Chi)"=chisq.pvalues)
6.4013880/summary(out3)$dispersion
round(summary(out3)$coefficients,8)
