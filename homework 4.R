#Homework 4, Stat638 Fall 2016

#5.1a

school1 <-scan(file="C:/Users/Saistout/Desktop/Stat638/homework/data/school1.dat")
school2 <-scan(file="C:/Users/Saistout/Desktop/Stat638/homework/data/school2.dat")
school3 <-scan(file="C:/Users/Saistout/Desktop/Stat638/homework/data/school3.dat")

mu0=5
sig0=4
kap0=1
nu0=2

#school1
n1=length(school1)
ybar1=mean(school1)
sig21=var(school1)

kapn1=kap0+n1
nn1=nu0+n1

(mun1= (kap0*mu0+n1*ybar1)/kapn1)
(s2n1= (nu0*sig0+(n1-1)*sig21+(kap0*n1*(ybar1-mu0)^2)/kapn1)/nn1)
(sdn1 = sqrt(s2n1))

s2.ci1 <- 1/rgamma(10000, nn1/2, s2n1*nn1/2)
quantile(sqrt(s2.ci1),c(.025,.975))

theta.ci1 <- rnorm(10000, mun1, sqrt(s2.ci1/kapn1))
quantile(theta.ci1,c(.026,.975))

#school2
n2=length(school2)
ybar2=mean(school2)
sig22=var(school2)

kapn2=kap0+n2
nn2=nu0+n2

(mun2= (kap0*mu0+n2*ybar2)/kapn2)
(s2n2= (nu0*sig0+(n2-1)*sig22+(kap0*n2*(ybar2-mu0)^2)/kapn2)/nn2)
(sdn2 = sqrt(s2n2))

s2.ci2 <- 1/rgamma(10000, nn2/2, s2n2*nn2/2)
quantile(sqrt(s2.ci2),c(.025,.975))

theta.ci2 <- rnorm(10000, mun2, sqrt(s2.ci2/kapn2))
quantile(theta.ci2,c(.026,.975))

#school3
n3=length(school3)
ybar3=mean(school3)
sig23=var(school3)

kapn3=kap0+n3
nn3=nu0+n3

(mun3= (kap0*mu0+n3*ybar3)/kapn3)
(s2n3= (nu0*sig0+(n3-1)*sig23+(kap0*n3*(ybar3-mu0)^2)/kapn3)/nn3)
(sdn3 = sqrt(s2n3))

s2.ci3 <- 1/rgamma(10000, nn3/2, s2n3*nn3/2)
quantile(sqrt(s2.ci3),c(.025,.975))

theta.ci3 <- rnorm(10000, mun3, sqrt(s2.ci3/kapn3))
quantile(theta.ci3,c(.026,.975))

#b
a <- theta.ci1 < theta.ci2 & theta.ci2 < theta.ci3
sum(a)/length(a)

b <- theta.ci1 < theta.ci3 & theta.ci3 < theta.ci2
sum(b)/length(b)

c <- theta.ci2 < theta.ci1 & theta.ci1 < theta.ci3
sum(c)/length(c)

d <- theta.ci2 < theta.ci3 & theta.ci3 < theta.ci1
sum(d)/length(d)

e <- theta.ci3 < theta.ci1 & theta.ci1 < theta.ci2
sum(e)/length(e)

f <- theta.ci3 < theta.ci2 & theta.ci2 < theta.ci1
sum(f)/length(f)

#c
	ybarp1 <-rnorm(10000,mun1,sdn1)
	ybarp2 <- rnorm(10000,mun2,sdn2)
	ybarp3 <- rnorm(10000,mun3,sdn3)

a <- ybarp1 < ybarp2 & ybarp2 < ybarp3
sum(a)/length(a)

b <- ybarp1 < ybarp3 & ybarp3 < ybarp2
sum(b)/length(b)

c <- ybarp2 < ybarp1 & ybarp1 < ybarp3
sum(c)/length(c)

d <- ybarp2 < ybarp3 & ybarp3 < ybarp1
sum(d)/length(d)

f <- ybarp3 < ybarp1 & ybarp1 < ybarp2
sum(e)/length(e)

f <- ybarp3 < ybarp2 & ybarp2 < ybarp1
sum(f)/length(f)

#d
post <- theta.ci1 > theta.ci2 & theta.ci1 > theta.ci3
(sum(post)/length(post))

post <- ybarp1 > ybarp2 & ybarp1 > ybarp2
(sum(post)/length(post))

#5.2
ybara=75.2
s2a=7.3^2

ybarb=77.5
s2b=8.1^2

n=16
mu0=75
s20=100

kanu = c(1,2,4,8,16,32)
postprob = cbind(c(0,0,0,0,0,0),c(0,0,0,0,0,0))

for (i in 1:6){
	kna = kanu[i]+n
	nuna = kanu[i]+n
	muna = (kanu[i]*mu0+n*ybara)/kna
	s2na = (kanu[i]*s20+ (n-1)*s2a+kanu[i]*n*(ybara-mu0)^2)/kna/nuna
	s2.posta = 1/rgamma(10000, nuna/2, s2na*nuna/2)
	theta.posta = rnorm(10000, muna, sqrt(s2.posta/kna))

	knb = kanu[i]+n
	nunb = kanu[i]+n
	munb = (kanu[i]*mu0+n*ybarb)/knb
	s2nb = (kanu[i]*s20+ (n-1)*s2b+kanu[i]*n*(ybarb-mu0)^2)/knb/nunb
	s2.postb = 1/rgamma(10000, nunb/2, s2nb*nunb/2)
	theta.postb = rnorm(10000, munb, sqrt(s2.postb/knb))

	post = theta.posta < theta.postb
	postprob[i,] = c(kanu[i],sum(post)/length(post))	
}
plot(postprob, xlab="nu=kappa", ylab="Prob(thetaA < thetaB | ya,yb)")