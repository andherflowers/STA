library(survival)
attach(cancer)
lognorm = survreg(Surv(time,status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, dist='lognormal', data=cancer)
summary(lognorm)
#----------------------------------------------------------
#15. (a)Notice that only sex and ECOG rating are significant.
#Test all the other variables simultaneously, controlling for sex and ECOG rating.
#Use a Wald test. Does it appear safe to drop all these variables?
#----------------------------------------------------------
source("http://www.utstat.toronto.edu/~brunner/Rfunctions/Wtest.txt")
Vhat = vcov(lognorm)
thetahat = lognorm$coefficients
sigmahat = lognorm$scale
thetahat = c(thetahat,log(sigmahat))
#H0: beta1=beta4=...=beta8=0. Express as H0: L theta = h
eMat = rbind(c(0,1,0,0,0,0,0,0,0),
             c(0,0,0,0,1,0,0,0,0),
             c(0,0,0,0,0,1,0,0,0),
             c(0,0,0,0,0,0,1,0,0),
             c(0,0,0,0,0,0,0,1,0))
Wtest(L=eMat, Tn=thetahat, Vn=Vhat) 
#dim(Vhat) ; get 9

model = survreg(Surv(time,status) ~ sex + ph.ecog, dist='lognormal', data=cancer); summary(model)
#----------------------------------------------------------
#i. Controlling for ECOG rating, median survival time for females is estimated to be times as great as median survival time for males.
#sex:	Male=0 Female=1
answer = exp(coef(model)["sex"]); answer #1.79
#----------------------------------------------------------
#ii. Predict the survival time in days for a new female patient with an ECOG rating of one. This number is the same as the estimated median.
#Now fit a model with just sex and ECOG rating
fem.ecog1 = data.frame(sex=2,ph.ecog=1)
pred = predict(model,newdata=fem.ecog1,type='linear',se=TRUE);pred #yhat
#compare with estimated median
beta0=coef(model)["(Intercept)"]; beta1=coef(model)["sex"]; beta2=coef(model)["ph.ecog"]; sigma=1.04
exp(beta0+beta1*2+beta2*1)
#----------------------------------------------------------
#iii. Give a 95% prediction interval for the survival time (in days) for a new female patient with an ECOG rating of one. Your answer is a pair of numbers. My upper prediction limit is around 8.7 years.
yhat = pred$fit
sigmasqhat = model$scale^2
se = sqrt(sigmasqhat+pred$se^2)
L = yhat - 1.96*se; U = yhat + 1.96*se
that= exp(yhat)
lower95 = exp(L); upper95 = exp(U)
pi = c(that,lower95,upper95)
names(pi) = c('t-hat','lower95','upper95');pi #divide by 365 to get 8.7 yrs
#----------------------------------------------------------
#iv. Compare the 95% confidence interval for median survival time, where gdot is the regression model,
#for females with ECOG = 1, median does not depend on sigma
gdot = cbind(exp(beta0+2*beta1+1*beta2),2*exp(beta0+2*beta1+1*beta2),
             exp(beta0+2*beta1+1*beta2),0)

Vhatfinal = vcov(model)
se = sqrt(as.numeric(gdot%*%Vhatfinal%*%t(gdot)));se

c(that,that-1.96*se,that+1.96*se)
