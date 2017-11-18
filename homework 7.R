#Homework 7 Stat 638 Fall 2016
library(pscl)

#8.3
#Read data files
school1 = scan(file="C:/Users/Saistout/Desktop/Stat638/homework/data/school1.dat")
school2 = scan(file="C:/Users/Saistout/Desktop/Stat638/homework/data/school2.dat")
school3 = scan(file="C:/Users/Saistout/Desktop/Stat638/homework/data/school3.dat")
school4 = scan(file="C:/Users/Saistout/Desktop/Stat638/homework/data/school4.dat")
school5 = scan(file="C:/Users/Saistout/Desktop/Stat638/homework/data/school5.dat")
school6 = scan(file="C:/Users/Saistout/Desktop/Stat638/homework/data/school6.dat")
school7 = scan(file="C:/Users/Saistout/Desktop/Stat638/homework/data/school7.dat")
school8 = scan(file="C:/Users/Saistout/Desktop/Stat638/homework/data/school8.dat")

#Prior specification
mu_0=7
gamma_0=5
tau_0=10
eta_0=2
sig_0=15
nu_0=2

#sufficient statistics
Y1 = cbind(length(school1),mean(school1),var(school1))
Y2 = cbind(length(school2),mean(school2),var(school2))
Y3 = cbind(length(school3),mean(school3),var(school3))
Y4 = cbind(length(school4),mean(school4),var(school4))
Y5 = cbind(length(school5),mean(school5),var(school5))
Y6 = cbind(length(school6),mean(school6),var(school6))
Y7 = cbind(length(school7),mean(school7),var(school7))
Y8 = cbind(length(school8),mean(school8),var(school8))
Y= list(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)


#Use Prof. Hart's code
post = normal.hierarchy.suff(Y,20000,mu_0,gamma_0,eta_0,tau_0,nu_0,sig_0)

#A
#Plot ACF, so thin by 2
acf(post[[2]], main="Sigma Squared")
acf(post[[3]], main="Mu")
acf(post[[4]], main="Tau Squared")

odds = seq(1,20000,2)
post_sig= post[[2]][odds]
post_mu= post[[3]][odds]
post_tau= post[[4]][odds]

#Assess stationarity
sig_station = post_sig[0:500]
for ( i in 2:10){
sig_station = cbind(sig_station,post_sig[(i-1)*500:(i*500)])
}
boxplot(sig_station, main="Stationarity of Sigma Squared")

mu_station = post_mu[0:500]
for ( i in 2:10){
mu_station = cbind(mu_station,post_mu[(i-1)*500:(i*500)])
}
boxplot(mu_station, main="Stationarity of Mu")

tau_station = post_tau[0:500]
for ( i in 2:10){
tau_station = cbind(tau_station,post_tau[(i-1)*500:(i*500)])
}
boxplot(tau_station, main="Stationarity of Tau Squared")

#B
#Posterior Inference
mean(post_sig)
quantile(post_sig,c(.025,.975))

mean(post_mu)
quantile(post_mu,c(.025,.975))

mean(post_tau)
quantile(post_tau,c(.025,.975))

plot(density(post_sig), col="blue", xlim=c(0,30), main="Posterior(blue) vs. Prior(Red) Sigma Squared")
x = seq(.001,30,.001)
lines(x,densigamma(x,nu_0/2,nu_0*sig_0/2), col="red")

plot(density(post_mu), col="blue", main="Posterior(blue) vs. Prior(Red) Mu")
x = seq(-10,30,.001)
lines(x,dnorm(x,mu_0,gamma_0), col="red")

plot(density(post_tau), col="blue", xlim=c(0,150), ylim=c(0,.055),main="Posterior(blue) vs. Prior(Red) Tau Squared")
x = seq(.001,150,.001)
lines(x,densigamma(x,eta_0/2,eta_0*tau_0/2), col="red")

#C
r_den = post_sig+post_tau
plot(density(post_tau/r_den), col="blue", xlim=c(0,1), ylim=c(0,20),main="Posterior(blue) vs. Prior(Red) Tau Squared/ Sigma Squared+ Tau Squared")
x = seq(.05,2,.001)
denom = densigamma(x,nu_0/2,nu_0*sig_0/2)+densigamma(x,eta_0/2,eta_0*tau_0/2)
prior = densigamma(x,eta_0/2,eta_0*tau_0/2)/denom
lines(density(prior),col="red")

#D
percent= rep(0,7)
for (i in 1:6){
percent[i]=sum(post[[1]][,7] < post[[1]][,i])/20000
}
percent[7]=sum(post[[1]][,7] < post[[1]][,8])/20000

#E
means = cbind(mean(school1),mean(school2),mean(school3),mean(school4),mean(school5),mean(school6),mean(school7),mean(school8))
expect = cbind(mean(post[[1]][,1]),mean(post[[1]][,2]),mean(post[[1]][,3]),mean(post[[1]][,4]),mean(post[[1]][,5]),mean(post[[1]][,6]),mean(post[[1]][,7]),mean(post[[1]][,8]))
plot(means, expect, main="Sample Means vs. Posterior Expectations", ylim=c(6,11))
text(means,expect,c(1,2,3,4,5,6,7,8), pos=3)

mean(post_mu)
sum_pri = 0
elements = 0
for (i in 1:8){
	sum_pri = sum_pri+Y[[i]][1]*Y[[i]][2]
	elements = elements + Y[[i]][1]
}
sum_pri/elements