###############################################################################
#5. Let the continuous random variable T have hazard function h(t)=(t−2)^2 for t > 0,
#so that the risk of failure decreases at first, and then increases without bound.
#(c) Using R, make a plot of f(t)
###############################################################################
t = seq(from=0,to=20,length=101)
Density = exp(-8/3)*(t-2)^2*exp(-(t-2)^3/3)
plot(t,Density,type='l',main="Density with h(t) = (t-2)^2")
###############################################################################
# 6. Let X have an exponential distribution with λ = 1, and let Y = log(X).
#The distribution of Y is called the (standard) Gumbel, or extreme value distribution.
#(c) Using R, make a plot of the standard Gumbel density.
###############################################################################
tt = seq(from=-10,to=10,length=101)
Density1 = exp(tt-exp(tt))
plot(tt,Density1,type='l',main="Standard Gumbel Density")
###############################################################################
#(g) The expected value of Y is surprisingly difficult. Use moment-generating functions.
#Recall that the moment-generating function of Y is M(t) = E(exp(Yt)) and M'(0) = E(Y)
#ii. Differentiate the moment-generating function of Y with respect to t and set t = 0.
###############################################################################
digamma(1)