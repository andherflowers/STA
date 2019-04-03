oink = read.table("http://www.utstat.toronto.edu/~brunner/data/legal/pigweight.data.txt")
head(oink); attach(oink)
n = length(Drug); n
# Make indicator dummy variables for Drug, 3 will be the reference category
Drug = as.factor(Drug) #Drug is not a factor
contrasts(Drug) = contr.treatment(3,base=3)
colnames(contrasts(Drug)) = c("Drug 1","Drug 2")
contrasts(Drug)

aggregate(Pigweight,by=list(Drug),FUN=mean)

fullmodel = lm(Pigweight ~ Drug+Momweight+Dadweight); summary(fullmodel)
justparents = lm(Pigweight ~ Momweight+Dadweight)
#----------------------------------------------------------
# (c) Predict dressed weight of a pig getting Drug 2, mother 140 lbs, father 185 lbs
#----------------------------------------------------------
pigd2 = data.frame(Momweight=140,Dadweight=185,Drug='2'); pigd2
predict(fullmodel,newdata=pigd2)
#----------------------------------------------------------
#(d)95% confidence interval for the difference in expected weight between drug 2 and 3 We know the Ho that the two regression lines are parallel is equivalent to H0:β2= 0, then the slope simplifies to β2.
#mu2 - mu3=beta4
#----------------------------------------------------------
expect = summary(fullmodel)$coef["DrugDrug 2","Estimate"] #extract from summary(lm)
tcrit = qt(0.975, df=70);tcrit
se = summary(fullmodel)$coef["DrugDrug 2","Std. Error"]; se
c(expect, expect-tcrit*se, expect+tcrit*se)
#----------------------------------------------------------
#(f) i. Controlling for mother’s weight and father’s weight, does type of drug have an effect on the expected weight of a pig?
anova(justparents,fullmodel) # Full vs restricted
#----------------------------------------------------------
#ii. which drug helps the average pig gain more weight, Drug 1 or Drug 2?
source("http://www.utstat.utoronto.ca/~brunner/Rfunctions/ftest.txt")
Ldrug12 = cbind(0,1,0,0,0)
ftest(fullmodel,Ldrug12,h=0) #statsign so choose D1 over D2,
#----------------------------------------------------------
#ii. which drug helps the average pig gain more weight, Drug 1 or Drug 3? #summary(fullmodel)
Ldrug13 = cbind(0,1,-1,0,0)
ftest(fullmodel,Ldrug13,h=0)
#----------------------------------------------------------
#iv. which drug helps the average pig gain more weight, Drug 2 or Drug 3? #summary(fullmodel)
Ldrug23 = cbind(0,0,1,0,0)
ftest(fullmodel,Ldrug23,h=0)
#----------------------------------------------------------
#v. does expected weight of a pig increase faster as a function of the mother’s weight, or as a function of the father’s weight? cant r ref is same
Lmomdad = cbind(0,0,0,1,-1); Lmomdad
ftest(fullmodel,Lmomdad,h=0)
