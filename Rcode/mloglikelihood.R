#----------------------------------------------------------
# For a Normal random sample
# f(x) = 1/(sqrt(2*pi)*(sigma^2))*exp(-(x-mu)^2/2*sigma^2)
#----------------------------------------------------------

mloglike_norm = function(theta,x){ # theta is (mu,sigmasq)
  mu = theta[1]; sigmasq = theta[2]
  n = length(x)
  value = n/2*log(sigmasq) + n/2*log(2*pi) + sum((x-mu)^2)/(2*sigmasq)
  return(value)
}

#----------------------------------------------------------
# For a 2 parameters Weibull distribution
# f(x|alpha, lambda) = alpha*lambda^(alpha)*exp(-(lambda*x)^alpha)), t >= 0
#----------------------------------------------------------
mloglike_weibull = function(theta,x){ #theta is (alpha,lambda)
  alpha = theta[1]; lambda = theta[2]
  n = length(x)
  value = -n*log(alpha) - (alpha*n)*log(lambda) - (alpha-1)*sum(log(x)) + sum((lambda*x)^alpha)
  return(value)
}

#----------------------------------------------------------
# Finding maximum likelihood estimate numerically
# 2  S T E P S
#----------------------------------------------------------
# help(optim) # Minimizing a function (mloglike functions stand for minus loglikehihood functions)

startvals = c(,) # add-on e.g. can try using c(0,1) for mloglike_norm

normalsearch = optim(par=startvals, fn, x, hessian=TRUE, lower=, method='L-BFGS-B') # add-on
