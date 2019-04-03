# Fit a model to the lung cancer data set
library(survival)
attach(cancer)
TimeToDeath = Surv(time, status)
#----------------------------------------------------------
#(a) model with one explanatory variable: Karnofsky performance score as rated by patient
ModelA = survreg(TimeToDeath~pat.karno, dist='weibull', data=cancer) #z=3.63 p-val=0.00028
summary(ModelA)
#----------------------------------------------------------
#(b) Weibull regression model with all the available explanatory variables, excluding institution
# Controlling for all other variables, is the Karnofsky performance score as rated by patient related to survival time?
ModelB = survreg(TimeToDeath ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, dist='weibull', data=cancer)
summary(ModelB) #z=1.56 p-val=0.11979
#----------------------------------------------------------
#(c) Without dropping any variables, test the diet and weight loss variables in a single test, controlling for all the other explanatory variables.
full = survreg(TimeToDeath ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, dist='weibull', data=cancer)
rest1 = survreg(TimeToDeath ~ age + sex + ph.ecog + ph.karno + pat.karno, dist='weibull', data=cancer)
anova(rest1,full) # LR test p-val=2.301935e-80

# Test diet and weight loss with a Wald test.
source("http://www.utstat.toronto.edu/~brunner/Rfunctions/Wtest.txt")
Vhat = vcov(full); Vhat
thetahat = full$coefficients; thetahat
sigmahat = full$scale; sigmahat
thetahat = c(thetahat,log(sigmahat))
#H0: beta9=beta8=0. Express as H0: L theta = h
eMat = rbind(c(0,0,0,0,0,0,1,0,0), c(0,0,0,0,0,0,0,1,0))
Wtest(L=eMat, Tn=thetahat, Vn=Vhat) #p-val=0.2073945
#----------------------------------------------------------
#(e)Create a data frame basing both full and restricted models upon a data set that has no missing values for the full model.
exc = cancer[-1]; newdata=na.omit(exc) #remove inst
nrow(newdata); ncol(newdata) #My data frame has 168 rows and 9 columns.
#----------------------------------------------------------
#(f) Based on this new data frame with no missing values, fit the full and restricted models, and test the difference with a LR test
TimetoDeathnew = Surv(newdata$time, newdata$status) #My test statistic value is G2 = 3.279398.
full2 = survreg(TimetoDeathnew ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, dist='weibull', data=newdata)
rest2 = survreg(TimetoDeathnew ~ age + sex + ph.ecog + ph.karno + pat.karno, dist='weibull', data=newdata)
anova(rest2,full2) #p-val=0.1940384
#----------------------------------------------------------
#(g)(i)Ho: beta1=0; Controlling for physician’s rating of how poorly the patient is doing, is median survival time different for males and female patients?
#(ii)beta2=0; Allowing for patient’s gender, is physician’s rating informative about survival time?
#(iii)logscale=0 her <0 so h increases; The default output of summary includes a test that will tell you whether the estimated hazard function is increasing or decreasing, without actually plotting it. Do the necessary paper-andpencil proof. 
#(iv) Estimate the median survival time for female patients with an ecog rating of 1.
#(v) v. This analysis coud continue. Look at table(ph.ecog). What is the next thing you would do with the data?

Model = survreg(TimetoDeathnew ~ sex + ph.ecog + ph.karno, dist='weibull', data=newdata); summary(Model)
Final = survreg(TimetoDeathnew ~ sex + ph.ecog, dist='weibull', data=newdata); summary(Final)

Sex = as.factor(sex) #sex is not a factor
contrasts(Sex) = contr.treatment(2,base=2) #fem as reference

beta0=5.8847; beta1=0.3672; beta2=-0.3479; sigma=0.722

med = exp(beta0+2*beta1+1*beta2)*log(2)^sigma

gdot = cbind(exp(beta0+2*beta1+1*beta2)*log(2)^sigma,
                    2*exp(beta0+2*beta1+1*beta2)*log(2)^sigma,
                    exp(beta0+2*beta1+1*beta2)*log(2)^sigma,
                    log(log(2))*log(2)^sigma*exp(beta0+2*beta1+1*beta2))

Vhatfinal = vcov(Final); Vhatfinal
se = sqrt(as.numeric(gdot%*%Vhatfinal%*%t(gdot)));se

c(med,med-1.96*se,med+1.96*se)
table(ph.ecog)
