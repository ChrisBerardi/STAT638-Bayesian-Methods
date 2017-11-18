#Homework 8 Stat638 Fall 2016

#9.2
data = read.table(file="C:/Users/Saistout/Desktop/Stat638/homework/data/azdiabetes.dat", header=TRUE)
#a
g = nrow(data)
nu0=2
sig0=1
samp = 20000
y=matrix(data[,2])
X = as.matrix(cbind(rep(1,nrow(data)),data[,c(1,3:7)]))
n = dim(X)[1]
p = dim(X)[2]

Hg = (g/(g+1))* X%*%solve(t(X)%*%X)%*%t(X)
SSRg = t(y)%*%(diag(1,nrow=n)-Hg)%*%y

s2 = 1/rgamma(samp, (nu0+n)/2, (nu0*sig0+SSRg)/2)

Vb = g*solve(t(X)%*%X)/(g+1)
Eb = Vb%*%t(X)%*%y

E = matrix(rnorm(samp*p,0,sqrt(s2)),samp,p)
beta = t(t(E%*%chol(Vb))+c(Eb))

#posterior confidence intervals
apply(beta, 2, quantile, probs=c(.025,.975))

#b use Prof. Hart's code
z=rep(1,6)
X = X[,2:7]
select = modelselect(X,y,2000,z)

apply(select[[1]],2,quantile, probs=c(.025,.975))
beta0 = NULL
for( i in 1:7){
	beta0=cbind(beta0, sum(select[[1]][,i] !=0)/length(select[[1]][,1]))
}
(beta0)

#9.3
#a
data = read.table(file="C:/Users/Saistout/Desktop/Stat638/homework/data/crime.dat", header=TRUE)
g = nrow(data)
nu0=2
sig0=1
samp = 20000
y = as.matrix(data[,1])
X = as.matrix(cbind(rep(1,g),data[,2:16]))
comb = cbind(y,X)

n = dim(X)[1]
p = dim(X)[2]

Hg = (g/(g+1))* X%*%solve(t(X)%*%X)%*%t(X)
SSRg = t(y)%*%(diag(1,nrow=n)-Hg)%*%y

s2 = 1/rgamma(samp, (nu0+n)/2, (nu0*sig0+SSRg)/2)

Vb = g*solve(t(X)%*%X)/(g+1)
Eb = Vb%*%t(X)%*%y

E = matrix(rnorm(samp*p,0,sqrt(s2)),samp,p)
beta = t(t(E%*%chol(Vb))+c(Eb))

apply(beta, 2, quantile, probs=c(.025,.975))

#OLS
attach(data)
ols = lm(y ~ M + So + Ed + Po1 + Po2 + LF + M.F + Pop + NW + U1 + U2 + GDP + Ineq + Prob)
detach(data)

#Baysian Approach
z=rep(1,15)
select = modelselect(X[,2:16],y,2000,z)
apply(select[[1]],2,quantile, probs=c(.025,.975))

#b
error_ols=NULL
for (i in 1:1000){
index = sample(1:46,23)
y_tra = comb[index,1]
y_tst = comb[-index,1]
X_tra = as.data.frame(comb[index,3:16])
X_tst = as.data.frame(comb[-index,3:16])

attach(X_tra)
ols = lm(y_tra ~ M + So + Ed + LF + NW + U1 + GDP + Ineq)
y_ols = predict(ols, X_tst)
error_ols = rbind(error_ols,sum((y_tst-y_ols)^2/length(y_ols)))
detach(X_tra)
}
plot(y_ols, y_tst, xlab="OLS Prediction" ,ylab="Data Values", main="OLS")

error_bays=NULL
for (i in 1:1000){
index = sample(1:46,23)
y_tra = as.matrix(comb[index,1])
y_tst = comb[-index,1]
X_tra = as.matrix(comb[index,c(2,3,4,7,10,11,13,14)])
X_tst = comb[-index,c(2,3,4,7,10,11,13,14)]


g = nrow(X_tra)
nu0=2
sig0=1
samp = 20000
n = dim(X_tra)[1]
p = dim(X_tra)[2]

Hg = (g/(g+1))* X_tra%*%solve(t(X_tra)%*%X_tra)%*%t(X_tra)
SSRg = t(y_tra)%*%(diag(1,nrow=n)-Hg)%*%y_tra

s2 = 1/rgamma(samp, (nu0+n)/2, (nu0*sig0+SSRg)/2)

Vb = g*solve(t(X_tra)%*%X_tra)/(g+1)
Eb = Vb%*%t(X_tra)%*%y_tra

E = matrix(rnorm(samp*p,0,sqrt(s2)),samp,p)
beta = t(t(E%*%chol(Vb))+c(Eb))
beta= apply(beta,2,mean)
y_bays = NULL
	for(j in 1:24){
		y_pred = NULL
		for (k in 1:8){
		y_pred = rbind(y_pred,beta[k]*X_tst[j,k])
		}
	y_bays = rbind(y_bays,sum(y_pred))
	}
error_bays = rbind(error_bays,sum((y_tst-y_bays)^2/length(y_bays)))
}

plot(y_bays,y_tst,xlab="g-Prior Prediction" ,ylab="Data Values", main="g-Prior")

mean(error_bays)
mean(error_ols)
