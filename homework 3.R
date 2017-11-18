#Homework 3

#4.1
#n=50, y=57, beta(1,1) prior
#posterior beta(58,44)
#posterior beta(32,21)

pop1 <- rbeta(5000,58,44)
pop2 <- rbeta(5000,32,21)

sum( pop1 < pop2)/length(pop1 <pop2)

#4.2a
spr1=120
spr2=12
npr1=10
npr2=1
a <- c(12,9,12,14,13,13,15,8,15,6)
b <- c(11,11,10,9,9,8,7,10,6,8,8,9,7)
npos1=length(a)
spos1=sum(a)
npos2=length(b)
spos2=sum(b)

theta1 <- rgamma(10000, spr1+spos1,npr1+npos1)
theta2 <- rgamma(10000, spr2+spos2,npr2+npos2)

mean(theta2<theta1)

#4.2b
prob <- rep(0,50)
for (i in 1:50){
	theta2 <-rgamma(10000,12*i+spos2,i+npos2)
	prob[i] <- mean(theta2<theta1)
}
prob

#4.2c
theta1 <- rgamma(10000, spr1+spos1,npr1+npos1)
theta2 <- rgamma(10000,spr2+spos2,npr2+npos2)

y1 <-rpois(10000,theta1)
y2 <-rpois(10000,theta2)

mean(y2<y1)

prob <- rep(0,50)
for (i in 1:50){
	theta2 <-rgamma(10000,12*i+spos2,i+npos2)
	y1 <-rpois(10000,theta1)
	y2 <-rpois(10000,theta2)
	prob[i] <- mean(y2<y1)
}
prob

#4.8a
data1 <- c(1, 0, 0, 1, 2, 2, 1, 5, 2, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 2, 1, 3, 2, 0, 0, 3, 0, 0, 0, 2, 1, 0, 2, 1,
0, 0, 1, 3, 0, 1, 1, 0, 2, 0, 0, 2, 2, 1, 3, 0, 0, 0, 1, 1)

data2 <- c(2, 2, 1, 1, 2, 2, 1, 2, 1, 0, 2, 1, 1, 2, 0, 2, 2, 0, 2, 1, 0, 0, 3, 6, 1, 6, 4, 0, 3, 2, 0, 1, 0, 0, 0, 3, 0,
 0, 0, 0, 0, 1, 0, 4, 2, 1, 0, 0, 1, 0, 3, 2, 5, 0, 1, 1, 2, 1, 2, 1, 2, 0, 0, 0, 2, 1, 0, 2, 0, 2, 4, 1, 1, 1,
 2, 0, 1, 1, 1, 1, 0, 2, 3, 2, 0, 2, 1, 3, 1, 3, 2, 2, 3, 2, 0, 0, 0, 1, 0, 0, 0, 1, 2, 0, 3, 3, 0, 1, 2, 2, 2,
 0, 6, 0, 0, 0, 2, 0, 1, 1, 1, 3, 3, 2, 1, 1, 0, 1, 0, 0, 2, 0, 2, 0, 1, 0, 2, 0, 0, 2, 2, 4, 1, 2, 3, 2, 0, 0,
 0, 1, 0, 0, 1, 5, 2, 1, 3, 2, 0, 2, 1, 1, 3, 0, 5, 0, 0, 2, 4, 3, 4, 0, 0, 0, 0, 0, 0, 2, 2, 0, 0, 2, 0, 0, 1,
 1, 0, 2, 1, 3, 3, 2, 2, 0, 0, 2, 3, 2, 4, 3, 3, 4, 0, 3, 0, 1, 0, 1, 2, 3, 4, 1, 2, 6, 2, 1, 2, 2)


theta1 <-rgamma(5000, 2+sum(data1), 1+length(data1))
theta2 <-rgamma(5000, 2+sum(data2), 1+length(data2))

y_bar1 = rep(0,5000)
y_bar2 =  rep(0,5000)
for (i in 1:5000){
	y1 <-rpois(5000,theta1)
	y2 <-rpois(5000,theta2)
	y_bar1[i] = mean(y1)
	y_bar2[i] = mean(y2)
}

plot(density(y_bar1, from=0, to=max(y_bar2),.1), col="red", main="Comparison of Monte Carlo Predicitive Posteriors red: Y1, blue:Y2")
lines(density(y_bar2, from=0, to=max(y_bar2),.1), col="blue")

#4.8b
quantile(theta2-theta1,c(.025,.975))
quantile(y_bar2-y_bar1,c(.025,.975))


#4.8c
plot(density(rpois(218,1.4), from=0, to=max(data2), 1), col="blue",main="Comparison of empirical(red) to poisson(blue)")
lines(density(data2, from=0, to=max(data2), 1), col="red")

#4.8d
zero = rep(0,5000)
one = rep(0,5000)
for (i in 1:5000){
	sample = rpois(218,theta2[i])
		zero[i] = sum(sample == 0)
		one[i] = sum(sample == 1)
}
plot(zero, one)
points(74,49, col="red", bg="red", pch=21)
