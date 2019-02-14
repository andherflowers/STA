################################################################################
# (e) The file contains a random sample from a Weibull distribution.
################################################################################
data=scan("http://www.utstat.toronto.edu/~brunner/data/legal/Weibull.data1.txt")

mloglike = function(theta,x){ #theta is (alpha,lambda)
  alpha = theta[1]; lambda = theta[2]
  n = length(x)
  value = -n*log(alpha) - (alpha*n)*log(lambda) - (alpha-1)*sum(log(x)) + sum((lambda*x)^alpha)
  return(value)
}
################################################################################
# i. Use R to find the MLE. The answer is a pair of numbers from your printout.
################################################################################
startvals = c(1,0.25) # I tried a few values
weibullsearch = optim(par=c(mean(data),var(data)), fn=mloglike, x=data, hessian=TRUE, lower=c(0,0), method='L-BFGS-B')
weibullsearch

alphahat = weibullsearch$par[1]; lambdahat = weibullsearch$par[2]
###############################################################################
#ii. Calculate a 95% confidence interval for alpha.
################################################################################
H = weibullsearch$hessian
eigen(H)$values

Vhat = solve(H)

se_alpha = sqrt(Vhat[1,1]) # since alpha = theta[1]
c(alphahat-1.96*se_alpha,alphahat+1.96*se_alpha)
###############################################################################
#iii. Give a point estimate of the expected value for the Weibull data.
###############################################################################
expect = lambdahat^(-1)*(gamma(1+1/alphahat)) # from 1(c), E(X^k) for k=1
c(expect, mean(data))
###############################################################################
# iv. Calculate a 95% confidence interval for the expected value. Hint: to apply the delta method, you will need the derivative of the gamma function.
###############################################################################
gdot_expect = cbind(lambdahat^(-1)*digamma(1+1/alphahat),(-1)*lambdahat^(-2)*gamma(1+1/alphahat))
se_expect = sqrt(as.numeric(gdot_expect%*%Vhat%*%t(gdot_expect)));se_expect
c(expect-1.96*se_expect,expect+1.96*se_expect)
###############################################################################
#v. Give a point estimate of the median for the Weibull data. The answer is a number that you calculate with R. It should appear on your printout. The answer should be fairly close to the sample median.
###############################################################################
medi = lambdahat^(-1)*(log(2))^(1/alphahat)
c(medi, median(data))
###############################################################################
# vi. Give a 95% confidence interval for the median. 
###############################################################################
gdot_med = cbind((-log(log(2))*log(2)^(1/alphahat))/(lambdahat*alphahat^2),(log(2)^(1/alphahat))/lambdahat^2)
se_median = sqrt(as.numeric(gdot_med%*%Vhat%*%t(gdot_med)));se_median
c(medi-1.96*se_median,medi+1.96*se_median)

###############################################################################
# vii.  Using the usual 0.05 significance level, test whether the expected value equals the median.
###############################################################################
gdot_both = cbind(digamma(1/alphahat+1)*(-1)/alphahat^2-(log(log(2))*log(2)^(1/alphahat))/alphahat^2, 0)
se_both = sqrt(as.numeric(gdot_both%*%Vhat%*%t(gdot_both)))

Z = (((gamma(1+1/alphahat)-(log(2))^(1/alphahat))-0))/se_both
pval = 2*(1-pnorm(abs(Z)))
c(Z,pval)
