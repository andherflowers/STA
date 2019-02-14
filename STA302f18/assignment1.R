#-----------------------------------------------------------------------
# Problem 1 a)
#-----------------------------------------------------------------------

real_estate_url = "http://www.math.unm.edu/~alvaro/real_estate.txt" # Entering data;
data = read.table(real_estate_url, header = TRUE) # import data in R;


price = data$Price # Formating data;
area = data$Living.Area
estate.reg = lm(price~area)
estate.reg$coef

# The regression equation is y = 94.45693x - 3116.95687

#-----------------------------------------------------------------------
# Problem 1 b)
#-----------------------------------------------------------------------

plot(area, price, pch=19, xlab="Living Area", ylab="Price", main="1 b) Luo") # Making scatterplot;
abline(estate.reg$coef, col="gray80") # Making regression line;

#-----------------------------------------------------------------------
# Problem 1 c)
#-----------------------------------------------------------------------

summary(estate.reg)
n = 1063 # Formatting data;

b0 = summary(estate.reg)$coef[1,1]
b1 = summary(estate.reg)$coef[2,1]

SEb = summary(estate.reg)$coef[2,2]

t.statistic = b / SEb
t.statistic
p.value = 2*(1-pt(t.statistic, n-2))
p.value

# The t statistic for testing H0 : β1 = 0 is t = 39.44502. This has df = 1061;
# R gives a P-value of 0. There is significant evidence (at α = 0.01 significance level) that β1 is nonzero.

#-----------------------------------------------------------------------
# Problem 1 d)
#-----------------------------------------------------------------------

# using the formula, we need t at alpha/2 = 0.025 (or qt for 1 - 0.025 = 0.975)
t_half_alpha = qt(0.975, df=n-2)
sxx = sum((area-mean(area))^2)

syy = sum((price-mean(price))^2)
sxy =  sum((area-mean(area))*(price-mean(price)))
sse = syy - b1*sxy
s = sqrt(sse/(n-2))

# for area = 4000 ft^2
(b0+b1*4000) - t_half_alpha*s*sqrt(1+1/n+(4000-mean(area))^2/sxx) # lower bound;
(b0+b1*4000) + t_half_alpha*s*sqrt(1+1/n+(4000-mean(area))^2/sxx) # upper bound;

# for area = 2000 ft^2
(b0+b1*2000) - t_half_alpha*s*sqrt(1+1/n+(2000-mean(area))^2/sxx) # lower bound;
(b0+b1*2000) + t_half_alpha*s*sqrt(1+1/n+(2000-mean(area))^2/sxx) # upper bound;
