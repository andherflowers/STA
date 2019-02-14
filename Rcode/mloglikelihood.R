# For a Weibull distribution

mloglike = function(theta,x){ #theta is (alpha,lambda)
  alpha = theta[1]; lambda = theta[2]
  n = length(x)
  value = -n*log(alpha) - (alpha*n)*log(lambda) - (alpha-1)*sum(log(x)) + sum((lambda*x)^alpha)
  return(value)
}
