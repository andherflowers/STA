library(survival)
attach(veteran)
help(veteran)
TimeToStatus = Surv(time, status)
#fit a model with experimental treatment, cell type and Karnofsky score.
#----------------------------------------------------------
model1 = coxph(TimeToStatus ~ trt + celltype + karno); summary(model1)

#4 (a)i. Controlling for cell type and Karnofsky score, does treatment affect survival time?
#----------------------------------------------------------
#ii. Allowing for experimental treatment and cell type, does Karnofsky score predict survival? In spite of the word “predict,” you are beng asked for a significance test.
#----------------------------------------------------------
#4 (a)iii. Correcting for experimental treatment and Karnofsky score, do patents with different types of cancer (cell type) differ in their hazard of dying? Do a partial likelihood ratio test
#----------------------------------------------------------
trtkarno = coxph(TimeToStatus ~ trt + karno); anova(trtkarno, model1)
#----------------------------------------------------------
#4 (a)iv. Follow up the last question by carrying out tests for all pairwise comparisons of cancer types. Some of the comparisons you want are Z-tests on the summary output. Use Wald tests for the other comparisons. Directional conclusions are possible for all the tests that are statistically significant, including the Wald tests.

source("http://www.utstat.toronto.edu/~brunner/Rfunctions/Wtest.txt")
Vhat = vcov(model1); dim(Vhat) #model1 has squamous as reference category by default
betahat = model1$coefficients
sm_vs_ad = cbind(0,1,-1,0,0)
Wtest(L=sm_vs_ad, Tn=betahat, Vn=Vhat)
sm_vs_la = cbind(0,1,0,-1,0)
Wtest(L=sm_vs_la, Tn=betahat, Vn=Vhat)
ad_vs_la = cbind(0,0,1,-1,0)
Wtest(L=ad_vs_la, Tn=betahat, Vn=Vhat)
#----------------------------------------------------------
#4 (b) Now we are interested in whether there could be an effect of experimental treatment that depends on the type of cancer. Fit another model with experimental
# Make celltype coding dummy variables

std = c1 = c2 = c3 = c4 = numeric(length(trt))
c1[celltype=='squamous'] =1; c1[celltype=='large'] =-1;
c2[celltype=='smallcell'] =1; c2[celltype=='large'] =-1;
c3[celltype=='adeno'] =1; c3[celltype=='large'] =-1;
#c4[celltype=='large'] =-1
std[trt=='1'] = 1
treat = std; treat[treat==0] = -1
# Product terms for interactions
trtc1 = treat*c1; trtc2 = treat*c2; trtc3 = treat*c3
mint = coxph(TimeToStatus ~ trt + karno + c1 + c2 + c3 + trtc1 + trtc2 + trtc3); summary(mint)
#Display summary and carry out a partial likelihood ratio test
mres = coxph(TimeToStatus ~ trt + karno + c1 + c2 + c3); summary(mres); anova(mres, mint)
#----------------------------------------------------------
#4c test whether there might be an effect of treatment that depends on Karnofsky score.
karnotrt = karno*trt
mc = coxph(TimeToStatus ~ trt + celltype + karno + karnotrt); summary(mc)
#----------------------------------------------------------
#5a Fit a proportional hazards model

attach(cancer)
ph = coxph(Surv(time, status) ~ sex + ph.ecog); summary(ph)
#----------------------------------------------------------
#5b
table(ph.ecog); summary(ph.ecog)
#----------------------------------------------------------
#5c Fit a model with sex and ph.ecog, in which ph.ecog is represented by dummy

noecog = ph.ecog; noecog[ph.ecog==3] = NA
e1 = e2 = numeric(length(inst))
e1[ph.ecog==1] =1;
e2[ph.ecog==2]=1;
phkarnodummy = coxph(Surv(time, status) ~ sex + e1 + e2); summary(phkarnodummy)
#----------------------------------------------------------
#5d beta3 = 2*beta2
#----------------------------------------------------------
#5e Test the null hypothesis with a Wald test
Vhatc = vcov(phkarnodummy); dim(Vhatc)
betahatc = phkarnodummy$coefficients
LLc = cbind(0,-2,1)
Wtest(LLc, betahatc, Vhatc)
