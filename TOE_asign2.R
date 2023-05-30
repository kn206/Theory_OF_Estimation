y=rnorm(50)
n=5
sd=16
xbar=50

##xbar+-z*s.e
z=qnorm(0.975)
print(z)
s.e<- sd/sqrt(n)
print(s.e)
##upper bound
upp=xbar+z*s.e
##Lower bound
low=xbar-z*s.e
loplot(density(y))abline(v=mean(y),col="red")

