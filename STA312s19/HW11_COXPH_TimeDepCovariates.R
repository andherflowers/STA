################################################################################
#Question 2
################################################################################
options(scipen=999)#;install.packages("KMsurv")
library("KMsurv"); library(survival); data("channing") #;help("channing")
library(survival)

channing =within(channing, {
  age= age/12
  ageentry = ageentry/12
  time= time/12
  
  gender[gender==1]=0
  gender[gender==2]=1
})

attach(channing)

agec = ageentry-mean(ageentry) # centered

summary(channing);summary(agec)

#(a)i. What was the median age at which participants entered the nursing home, in years? 900/12 = 75
# ii. What was the age of the youngest person to enter the nursing home, in years? 733/12 = 61.083
# iii. What was the age of the oldest person to enter the nursing home, in years? 1140/12 = 95
# iv. What proportion of the nursing home residents were women? 0.79 = 79%
# v. What was the longest length of stay at the nursing home, in years? look at max time 137/12 = 11.4167
# vi. What proportion of the observations were censored? look at death 1-0.381=61.9% (1=death, 0=alive)
#----------------------------------------------------------
#(b) Fit a proportional hazards model with age at entry and gender, no interaction yet.
agegender = coxph(Surv(time, death) ~ ageentry + gender); summary(agegender)

#i. Interpret both Z-tests in plain, non-statistical language.
#ii. Correcting for age, the estimated hazard of death is % as great for women. 68.67%
# iii. Give a 95% confidence interval for that last answer. Allowing for age, we estimate the hazard to be between % and % as great for women. 49.02 and 96. 18
# iv. Controlling for gender, if age at entry is increased by 10 years, we estimate the hazard of death to be multiplied by . exp(12*coef)=2.35
# v. Give a 95% confidence interval for that last number. (2.3264; 2.3747)
#vi. Test the proportional hazards assumption. What do you conclude?
cox.zph(agegender)
#----------------------------------------------------------

#(c) Now fit a model to test whether the sex difference in risk of death depends on the age
#with age at entry centered and age at entry uncentered. Which way do you like more? What is the evidence that the fit of the two models is the same?
gender_by_age = fem*ageentry
gender_by_agec = fem*agec

summary(coxph(Surv(time, death) ~ ageentry + fem + gender_by_age))
summary(coxph(Surv(time, death) ~ agec + fem + gender_by_agec))

################################################################################
#Question 3
################################################################################
liver = read.table("http://www.utstat.toronto.edu/~brunner/data/legal/liver.data.txt")
attach(liver)
help(subset)

#(a) What was the failure time for Patient 1? Patient 2? Patient 3? intervals 12, 1, and 11
#----------------------------------------------------------
#(b)i. How many patients took part in the study? 375 see summary
#ii. What was their mean age? 51.13
#iii. What percent were male? 1-0.4853 = 0.5147 (since 1=female 0=male)
newliver = subset(liver, start==0); summary(newliver)
#----------------------------------------------------------
#(c) Fit a proportional hazards model with no interactions
model = coxph(Surv(start,end,failed) ~ drug+age+sex+platelet); summary(model)
#i. Controlling for sex, experimental condition and platelet count, is there evidence that the risk of death depends on age?
#A. Answer the question Yes or No. Yes
#B. If the answer is Yes, state the conclusion in plain, non-statistical language.
#C. What test statistic value supports your conclusion? 3.327
# D. What p-value supports your conclusion? 0.000877 ***
#E. State the null hypothesis in symbols. Ho: beta3=0 for h(t)=ho(t)exp(beta1d2+beta2placebo+beta3age+beta4sex+beta5platelet)
#F. Do you reject H0? Answer Yes or No. Yes
#G. Are the results statistically significant? Answer Yes or No. Yes
#H. All other things being equal, if age is increased by one year, the estimated hazard of death is multiplied by . exp(coef*120months) = 6.6170
#I. Give a 95% confidence interval to go with that last estimate. 6.6170+-1.96*
#J. All other things being equal, if age is increased by ten years, the estimated hazard of death is multiplied by . You could get this from the output of summary with a calculator, but please use R and display the result on your printout.
#K. Give a 95% confidence interval to go with that last estimate. Again, this is an easy calculation based on the output of summary.


#ii. Controlling for age, experimental condition and platelet count, is there evidence that the risk of death depends on sex of patient?
# A. Answer the question Yes or No. No
#B. If the answer is Yes, state the conclusion in plain, non-statistical language.
#C. What test statistic value supports your conclusion? The answer is a number on your printout. -0.533
#D. What p-value supports your conclusion? 0.593720
#E. State the null hypothesis in symbols. Ho: beta4=0
#F. Do you reject H0? Answer Yes or No. No
#G. Are the results statistically significant? Answer Yes or No. No

source("http://www.utstat.toronto.edu/~brunner/Rfunctions/Wtest.txt")
beta_hat = model$coefficients; Vn_hat = vcov(model)
LL = rbind(c(1,0,0,0,0),c(0,1,0,0,0))
Wtest(LL,beta_hat,Vn_hat)

#iii. Controlling for age, sex and platelet count, is there evidence that experimental treatment (including the placebo condition) affects the chances of survival? Answer the question with a Wald test.
#A. Answer the question Yes or No. Yes
#B. What test statistic value supports your conclusion? 9.492974870
#C. What p-value supports your conclusion? 0.008682138
#D. State the null hypothesis in symbols. Ho: betad2=betaplacebo=0
#E. Do you reject H0? Answer Yes or No. Yes
#F. Are the results statistically significant? Answer Yes or No.
#G. If the results are statistically significant, follow up with all pairwise comparisons. State your conclusions in plain, non-statistical language.

pairwise = cbind(1,-1,0,0,0)
Wtest(pairwise,beta_hat,Vn_hat)
#----------------------------------------------------------
#(d) Now fit a model that allows you to test whether the effect of experimental treatment depends on the patient’s blood platelet count. Test the null hypothesis with a partial likelihood ratio test. What do you conclude?
intermodel = coxph(Surv(start,end,failed) ~ drug+age+sex+platelet+drug*platelet); summary(model)
anova(intermodel,model)
#----------------------------------------------------------
#(e) Test the proportional hazards assumption. What do you conclude? Ho: assume it is cox.ph
cox.zph(intermodel)
cox.zph(model)
