lobster_url = "https://mcs.utm.utoronto.ca/~nosedal/data/lobster.txt"
data = read.table(lobster_url, header = TRUE)

traps = data$Traps.Fisher
year = data$Year

plot(year, traps, main="Traps per Fisher vs Year")

fita = lm(traps ~ year, data)
plot(fita, which=1, main="a) Luo")

# The residual plot has some structure. It looks like a parabola and has pattern in mean of residuals
# This means the model is not the most adequate and can be improved

plot(a.lm, which=5, main="a) Luo")

cooks.distance(fita)[45] #output : 0.2905614 
cooks.distance(fita)[50] #output : 0.1376595
cooks.distance(fita)[36] #output : 0.03788043

# The graph shows that Cook's distances seem to all be less than 0.5. Since we don't have distances > 1,
# it seems there are no influential points in these data.

# Even if the 45th data seemed like an influential outlier in the Residuals vs Fitted plot,
# We can double-checked that its not since 0.2905614 < 1.

fitb = lm(traps ~ year + I(year^2), data)
plot(fitb, which=1, main="b) Luo")

# The residual plot has less structure than part a), but still has some pattern in its spread.
# Still, the model is more adequate than part a).

plot(fitb, which=5, main="b) Luo")

cooks.distance(fitb)[45] #output : 0.3210773  
cooks.distance(fitb)[50] #output : 0.08095145 
cooks.distance(fitb)[46] #output : 0.05930704 

# The graph shows that Cook's distances seem to all be less than 0.5. Since we don't have distances > 1,
# it seems there are no influential points in these data.

# Even if the 45th data seemed like an influential outlier in the Residuals vs Fitted plot,
# We can double-checked that its not since 0.3210773 < 1.

plot(fitb, which=2, main="c) Luo")

# A straight, diagonal line in a normal probability plot indicates normally distributed data.
# The data is mostly normally distributed but we see departures from this straight line at the tails
# which indicate departures from normality.
