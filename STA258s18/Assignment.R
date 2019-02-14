# problem 1 a)
tissues_url<-"http://www.math.unm.edu/~alvaro/tissues.txt"
tissues_data<-read.table(tissues_url,header=TRUE)
luo1<-tissues_data$NUMUSED
# b)
s<-sd(tissues_data$NUMUSED)
s
t.test(luo1, mu=60, alternative="less")

# problem 2
tissues_url<-"http://www.math.unm.edu/~alvaro/tissues.txt"
tissues_data<-read.table(tissues_url,header=TRUE)
luo2<-tissues_data$USED60
# b)
prop.test(luo2,p=0.5)

# problem 3 
index_url <-"http://www.math.unm.edu/~alvaro/mansuccess.txt"
index_data<-read.table(index_url,header=TRUE)
luo3<-index_data

# problem 4
sales_url<-"http://www.math.unm.edu/~alvaro/sales.txt"
sales_data<-read.table(sales_url,header=TRUE)
luo.current<-sales_data$This.Year
luo.old<-sales_data$Last.Year
diff<-luo.current-luo.old
t.test(diff, conf.level = 0.90)

# problem 5
luo5<-(185-175)/(20/sqrt(10))
1-pnorm(luo5,175,20)

luo5b<-(185-196)/(20/sqrt(10))
pnorm(luo5b,0,1)

means<-c(175,178,181,184,187,190,193,196,199,202)
n<-10
sigma<-20
vec.z<-sqrt(n)*(185-means)/sigma
beta=pnorm(vec.z)
power<-1-beta
plot(means,power,type="l",main="luo (power curve)", xlab=expression(mu))
plot(means,beta,type="l",main="luo (OC curve)", xlab = expression(mu))
# problem 6
speeds_url<- "http://www.math.unm.edu/~alvaro/speeds.txt"
speeds_data<-read.table(speeds_url,header=TRUE)
luo6<-speeds_data$Speeds

test.stat<-((245-1)*var(luo6))/18
qchisq(0.9,244)
test.stat

# problem 7
new_highway_url<-"http://www.math.unm.edu/~alvaro/new-highway.txt"
new_data<-read.table(new_highway_url,header=TRUE)
luo.week1<-new_data$Week1
luo.week2<-new_data$Week2
var.test(luo.week1,luo.week2, alternative = "greater")
