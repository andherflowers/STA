options(scipen = 999)
data = read.table("http://www.utstat.toronto.edu/~brunner/data/legal/expo.data2.txt")
attach(data)
# the data comes from an exponential distribution
#install.packages("survival",dependencies=TRUE)
library(survival) 
y = Surv(Time,Uncensored); y[1:20]
#----------------------------------------------------------
#11(a) Give the Kaplan-Meier estimate of the medan, and a 95% confidence interval
#----------------------------------------------------------
km = survfit(y ~ 1) ; km
#----------------------------------------------------------
#11(b) Based on the output from summary, give Sb(t) for t = 0.062.
#----------------------------------------------------------
summary(km)$surv[4]
#----------------------------------------------------------
#11(c) Give pb1, pb2, pb3 and pb4. These are numbers that you calculate from the output of summary.
#----------------------------------------------------------
summary(km)
phat1 = 48/50; phat2 = 46/47; phat3 = 43/44; phat4 = 42/43; phat1; phat2; phat3; phat4
#----------------------------------------------------------
#11(d) Reproduce Sb(0.062) from the answer to your last question
#----------------------------------------------------------
Shat0.062 = phat1*phat2*phat3*phat4; Shat0.062
#----------------------------------------------------------
#(e) Rproduce the standard error of Sb(0.062). You are calculating your answer to Question 8
#----------------------------------------------------------
estvarlog = 2/(50*48) +  1/(47*46) + 1/(44*43) + 1/(43*42)
estvarShat = estvarlog * Shat0.062^2 #one-variable delta method
seShat = sqrt(estvarShat); seShat # Compare 0.0437
#----------------------------------------------------------
#12(a) Compute the maximum likelihood estimate and a 95% confidence interval for the median. Compare your answer (3 numbers) to Question 11a
#----------------------------------------------------------
mloglike = function(theta, t, delta){ # Minus log likelihood function
  lambda = theta[1]
  logf = log(lambda) + (-lambda*t)
  logS = -(lambda*t)
  value = -sum(logf*delta) - sum(logS*(1-delta))
  return(value)
}

startvals = c(0.5)

search = optim(par=startvals, fn=mloglike, t=Time, delta=Uncensored,
               hessian=TRUE, lower=c(0), method='L-BFGS-B'); search
lambdahat = search$par;lambdahat

H = search$hessian
Vhat = solve(H)

medhat = lambdahat^(-1)*log(2)
gdot = -lambdahat^(-2)*log(2) #univariate delta method (this is gdot') so no need for as.numeric
v_medhat = gdot^2*Vhat
se_medhat = sqrt(v_medhat); se_medhat
lower95 = medhat - 1.96*se_medhat; upper95 = medhat + 1.96*se_medhat
c(medhat, lower95, upper95)
#----------------------------------------------------------
#12(b)Plot the Kaplan-Meier estimate of the survival function and the the MLE of S(t)
#----------------------------------------------------------
plot(km, xlab='Time',ylab='Survival')
title('Survival Funtion Estimate for expo.data2.txt')
x = seq(from=0,to=10,length=101)
ShatMLE = exp(-lambdahat*x)
lines(x,ShatMLE,lty=2, col = "red1")

x1 = c(0.5,0.75); y1 = c(0.8,0.8)
lines(x1,y1,lty=1)
text(1.25,0.8,'Kaplan-Meier of S(t)')
x2 = x1; y2 = c(0.7,0.7)
lines(x2,y2,lty=2, col = "red1")
text(1.25,0.7,'MLE of S(t)', col = "red1")

