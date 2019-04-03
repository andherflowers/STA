data = read.table("http://www.utstat.toronto.edu/~brunner/data/legal/expo.data2.txt")
attach(data) # Random sample from an exponential distribution with right censoring

lambda_hat = sum(Uncensored)/sum(Time); lambda_hat # 2(a) maximum likelihood estimate of lambda
vlambda_hat = lambda_hat^2/sum(Uncensored); vlambda_hat # 2(b) estimated asymptotic variance of lambda_hat
c(lambda_hat-1.96*sqrt(vlambda_hat),lambda_hat+1.96*sqrt(vlambda_hat)) # 2(c) 95% CI for lambda
###############################################################################
# 2(d) Estimated S(t) and confidence interval for t: seq(from=0,to=3,by=0.1)
# t, S(t), the LCL and the UCL into a matrix with 4 columns.
###############################################################################
t = seq(from=0,to=3,by=0.1)
St = exp(-sum(Uncensored)/sum(Time)*t)
LCL_delta = St-1.96*(lambda_hat^2/sum(Uncensored)*exp(-2*lambda_hat*t))
UCL_delta = St+1.96*(lambda_hat^2/sum(Uncensored)*exp(-2*lambda_hat*t))
cbind(t,St,LCL_delta,UCL_delta)
###############################################################################
# 2(e) Now do the same thing, using your confidence limits from Question 1(f)
###############################################################################
LCL_new = exp((-t*lambda_hat)-1.96*(lambda_hat*t)/sqrt(sum(Uncensored)))
UCL_new = exp((-t*lambda_hat)+1.96*(lambda_hat*t)/sqrt(sum(Uncensored)))
cbind(t,St,LCL_new,UCL_new)
###############################################################################
# 3(a) MLE of lambda by numerical search
###############################################################################
mloglike = function(theta, t, delta){ # Minus log likelihood function
  lambda = theta[1]
  logf = log(lambda) + (-lambda*t)
  logS = -(lambda*t)
  value = -sum(logf*delta) - sum(logS*(1-delta))
  return(value)
}

startvals = c(0.5)

search1 = optim(par=startvals, fn=mloglike, t=Time, delta=Uncensored,
                hessian=TRUE, lower=c(0), method='L-BFGS-B'); search1
c(lambda_hat,search1$par)
###############################################################################
# 3(b) estimated asymptotic variance of lambda by numerical search
###############################################################################
H = search1$hessian
Vhat = solve(H) # Solve returns the inverse.
c(vlambda_hat, Vhat)

