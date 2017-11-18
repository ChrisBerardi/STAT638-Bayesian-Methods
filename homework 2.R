#Homework 2, Stat638 Fall 2016

#3.1b
theta <- seq(0,1,.1)
binom <- dbinom(57,100,theta)
plot(theta, binom, ylab="Density")

#3.1c
denom <- dbinom(57,100,theta)*1/11
post <- (dbinom(57,100,theta)*1/11)/sum(denom)
plot(theta, post, ylab="Density")


#3.1d
theta <- seq(0,1,.001)
post <- dbinom(57,100,theta)*dbeta(theta,1,1)
plot(theta, post, type='l', ylab="Density")

#3.1e
post <-dbeta(theta,58,44)
plot(theta,post, type="l", ylab="Density")

#3.3a
npr1=120
npr2=12
spr1=10
spr2=1
a <- c(12,9,12,14,13,13,15,8,15,6)
b <- c(11,11,10,9,9,8,7,10,6,8,8,9,7)
npos1=length(a)
spos1=sum(a)
npos2=length(b)
spos2=sum(b)

#mean a
(spr1+spos1)/(npr1+npos1)
#variance
(spr1+spos1)/(npr1+npos1)^2
#.95 CI
qgamma(c(.025,.975), spr1+spos1,npr1+npos1)

#mean b
(spr2+spos2)/(npr2+spos2)
#variance
(spr2+spos2)/(npr2+spos2)^2
#.95 CI
qgamma(c(.025,.975),spr2+spos2,npr2+spos2)

#3.3b
x <- seq(0,25,.1)
plot(x, dgamma(x,12*50+spos2,50+npos2), type='l', ylab="Density")
for (i in 1:49){
	lines(x, dgamma(x, 12*i+spos2,i+npos2),type='l')
}


#3.4a
theta <- seq(0,1,.001)


#p(theta|y)
plot(theta, dbeta(theta,17,51),type='l',ylab="density", main="p(theta|y):black, p(theta):blue")

#p(theta)
lines(theta, dbeta(theta,2,8), type='l', col=36)

#p(y|theta)
plot(theta, dbinom(15, 43, theta),type='l', col=26, main="p(y|theta)", ylab="Density")

# mean: 17/(17+51)
# variance: 17*51/((17+51+1)*(17+51)^2)
# mode: (17-1)/(17-1+51-1)
#.95 CI
qbeta(c(.025,.975),17,51)

#3.4b
#p(theta|y)
plot(theta, dbeta(theta,23,45),type='l',ylab="density", main="p(theta|y):black, p(theta):blue")


#p(theta)
lines(theta, dbeta(theta,8,2), type='l', col=36)

#p(y|theta)
plot(theta, dbinom(23, 43, theta),type='l', col=26, ylab="Density",main="p(y|theta)" )

# mean: 23/(23+45)
# variance: (23*45)/((23+45+1)*(23+45)^2)
# mode: (23-1)/(23-1+45-1)
#.95 CI
qbeta(c(.025,.975),23,45)

#3.4c
prior <-rep(0,1001)
for (i in 0:1001){
	prior[i] <- .25*gamma(10)/gamma(2)/gamma(8)*((3*i/1000)*(1-i/1000)^7+(1-i/1000)*(i/1000)^7)
	}
plot(theta, prior, type='l', ylim=c(0,4), ylab="Density", main="beta(2,8):red, beta(8,2):blue, prior:black")
lines(theta, dbeta(theta,2,8), type='l', col=26)
lines(theta, dbeta(theta,8,2), type='l', col=36)

#3.4diii
theta <- seq(0,1,.001)
dens <- (3*dbeta(theta,17,36)+dbeta(theta,23,30))/4
plot(theta, dens, ylab="Density", type='l')

#3.7a
plot(theta, dbeta(theta,3, 14),type='l', ylab="Density")
# mean: 3/(3+14)
# variance: (3*14)/((3+14+1)*(3+14)^2)
# mode: (3-1)/(3-1+14-1)

#3.7c
y <- seq(1,278,1)
plot(y, beta(y+2,278-y+13)/beta(3,14)*choose(278,y), type='l', ylab='Pr')



#3.7d
plot(y, dbinom(y,278,2/15), type='l', ylab="Density")
# mean 278*2/15
# stdev sqrt(278*(2/15)*(1-2/15))