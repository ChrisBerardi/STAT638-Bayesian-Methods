#Homework 6
library(mvtnorm)
library(MCMCpack)

#7.3
blue <- scan(file="C:/Users/Saistout/Desktop/Stat638/homework/data/bluecrab.dat")
orange <- scan(file="C:/Users/Saistout/Desktop/Stat638/homework/data/orangecrab.dat")

odd = seq(1,100,2)
even = seq(2,100,2)

bluedepth = blue[odd]
bluerear = blue[even]

orangedepth = orange[odd]
orangerear = orange[even]

y_bar_b = c(mean(bluedepth) ,mean(bluerear))
y_bar_o = c(mean(orangedepth),mean(orangerear))
sig0_b = rbind(c(var(bluedepth),cov(bluedepth,bluerear)),c(cov(bluedepth,bluerear),var(bluerear)))
sig0_o = rbind(c(var(orangedepth),cov(orangedepth,orangerear)),c(cov(orangedepth,orangerear),var(orangerear)))

nu0=4
n=50
set.seed(250)
theta_b <- sig_b <- NULL
for (i in 1:10000)
{
#new theta
sign = solve( solve(sig0_b)+n*solve(sig0_b))
mun = sign%*%( solve(sig0_b)%*%y_bar_b+n*solve(sig0_b)%*%y_bar_b)
thetan= rmvnorm(1,mun,sign)
#new sigma
sign= sig0_b+ (t(cbind(bluedepth,bluerear))-c(thetan))%*%t(t(cbind(bluedepth,bluerear))-c(thetan))
sigman = solve(rwish(nu0+n, solve(sign)))

theta_b <- rbind(theta_b,thetan)
sig_b <- rbind(sig_b,c(sigman))
}

theta_o <- sig_o <- NULL
for (i in 1:10000)
{
#new theta
sign = solve( solve(sig0_o)+n*solve(sig0_o))
mun = sign%*%( solve(sig0_o)%*%y_bar_o+n*solve(sig0_o)%*%y_bar_o)
thetan= rmvnorm(1,mun,sign)
#new sigma
sign= sig0_b+ (t(cbind(orangedepth,orangerear))-c(thetan))%*%t(t(cbind(orangedepth,orangerear))-c(thetan))
sigman = solve(rwish(nu0+n, solve(sign)))

theta_o <- rbind(theta_o,thetan)
sig_o <- rbind(sig_o,c(sigman))
}
#b
plot(theta_o, col="orange", ylim=c(11,18),xlim=c(10.5,13.5), ylab=expression(theta ~ "1"), xlab=expression(theta ~"2"), main="Orange crabs in orange, blue in blue")
points(theta_b, col="blue")

#c

cor_b = NULL
cor_o = NULL
for (i in 1:10000) {
	cor_b[i]=sig_b[i,2]/sig_b[i,1]/sig_b[i,4]
}
for (i in 1:10000) {
	cor_o[i]=sig_o[i,2]/sig_o[i,1]/sig_o[i,4]
}
plot(density(cor_b),col="blue",ylim=c(0,16), main="Orange in orange, blue in blue")
lines(density(cor_o),col="orange")
(sum(cor_b < cor_o)/length(cor_b))

#Extra Problem
blood <- matrix(scan(file="C:/Users/Saistout/Desktop/Stat638/homework/data/bloodwork.txt"),178,3, byrow="TRUE")
theta_i = apply(blood, 2, mean)
cov_i = cov(blood)

out = missing(blood,theta_i,cov_i,10000)
theta = matrix(unlist(out[2]), ncol=3)
cov = out[3]

par(mfrow=c(3,2))
plot(theta[,1],theta[,2], main=expression(theta ~"1 vs." ~ theta ~"2"), xlab=expression(theta~"1"), ylab=expression(theta~"2"), col="blue")
plot(theta[,1],theta[,3], main=expression(theta ~"1 vs." ~ theta ~"3"), xlab=expression(theta~"1"), ylab=expression(theta~"3"), col="blue")
plot(theta[,2],theta[,3], main=expression(theta ~"2 vs." ~ theta ~"3"), xlab=expression(theta~"2"), ylab=expression(theta~"3"), col="blue")
plot(density(theta[,1]), main=expression("Kernal Density Estimate" ~ theta ~"1"), xlab=expression(theta~"1"), ylab="Density", col="blue")
plot(density(theta[,2]), main=expression("Kernal Density Estimate" ~ theta ~"2"), xlab=expression(theta~"2"), ylab="Density", col="blue")
plot(density(theta[,3]), main=expression("Kernal Density Estimate" ~ theta ~"3"), xlab=expression(theta~"3"), ylab="Density", col="blue")

plot(sqrt(cov[[1]][1,1,]),sqrt(cov[[1]][2,2,]), main=expression(sigma ~"1 vs." ~ sigma ~"2"), xlab=expression(sigma~"1"), ylab=expression(sigma~"2"), col="red")
plot(sqrt(cov[[1]][1,1,]),sqrt(cov[[1]][3,3,]), main=expression(sigma ~"1 vs." ~ sigma ~"3"), xlab=expression(sigma~"1"), ylab=expression(sigma~"3"), col="red")
plot(sqrt(cov[[1]][2,2,]),sqrt(cov[[1]][3,3,]), main=expression(sigma ~"2 vs." ~ sigma ~"3"), xlab=expression(sigma~"2"), ylab=expression(sigma~"3"), col="red")
plot(density(sqrt(cov[[1]][1,1,])), main=expression("Kernal Density Estimate" ~ sigma ~"1"), xlab=expression(sigma~"1"), ylab="Density", col="red")
plot(density(sqrt(cov[[1]][2,2,])), main=expression("Kernal Density Estimate" ~ sigma ~"2"), xlab=expression(sigma~"2"), ylab="Density", col="red")
plot(density(sqrt(cov[[1]][3,3,])), main=expression("Kernal Density Estimate" ~ sigma ~"3"), xlab=expression(sigma~"3"), ylab="Density", col="red")

par(mfrow=c(1,1))
plot(density(cov[[1]][1,2,]/sqrt(cov[[1]][1,1,])/sqrt(cov[[1]][2,2,])), main=expression("Kernal Density Estimate" ~ rho ~"12"), xlab=expression(rho~"12"), ylab="Density")
plot(density(cov[[1]][1,3,]/sqrt(cov[[1]][1,1,])/sqrt(cov[[1]][3,3,])), main=expression("Kernal Density Estimate" ~ rho ~"13"), xlab=expression(rho~"13"), ylab="Density")
plot(density(cov[[1]][2,3,]/sqrt(cov[[1]][2,2,])/sqrt(cov[[1]][3,3,])), main=expression("Kernal Density Estimate" ~ rho ~"23"), xlab=expression(rho~"23"), ylab="Density")

# CIs
quantile(theta[,1],c(.025,.975))
quantile(theta[,2],c(.025,.975))
quantile(theta[,3],c(.025,.975))

quantile(sqrt(cov[[1]][1,1,]),c(.025,.975))
quantile(sqrt(cov[[1]][2,2,]),c(.025,.975))
quantile(sqrt(cov[[1]][3,3,]),c(.025,.975))

mean(theta[,1])+1.96*sqrt(var(theta[,1]))
mean(theta[,1])-1.96*sqrt(var(theta[,1]))
mean(theta[,2])+1.96*sqrt(var(theta[,2]))
mean(theta[,2])-1.96*sqrt(var(theta[,2]))
mean(theta[,3])+1.96*sqrt(var(theta[,3]))
mean(theta[,3])-1.96*sqrt(var(theta[,3]))

mean(sqrt(cov[[1]][1,1,]))+1.96*sqrt(var(sqrt(cov[[1]][1,1,])))
mean(sqrt(cov[[1]][1,1,]))-1.96*sqrt(var(sqrt(cov[[1]][1,1,])))
mean(sqrt(cov[[1]][2,2,]))+1.96*sqrt(var(sqrt(cov[[1]][2,2,])))
mean(sqrt(cov[[1]][2,2,]))-1.96*sqrt(var(sqrt(cov[[1]][2,2,])))
mean(sqrt(cov[[1]][3,3,]))+1.96*sqrt(var(sqrt(cov[[1]][3,3,])))
mean(sqrt(cov[[1]][3,3,]))-1.96*sqrt(var(sqrt(cov[[1]][3,3,])))
