#Homework 5

#6.1
nobach <- scan(file="C:/Users/Saistout/Desktop/Stat638/homework/data/menchild30nobach.dat")
bach <- scan(file="C:/Users/Saistout/Desktop/Stat638/homework/data/menchild30bach.dat")

y_bara = mean(bach)
na = length(bach)
nb = length(nobach)
y_barb = mean(nobach)
gam_bar = y_barb/y_bara

at=2
bt=1

gam = c(8,16,32,64,128)

exp = rep(0,5)
num = 100000
for (j in 1:length(gam))
{
gibbs = matrix(nrow=num, ncol=2)
gibbs[1,] = c(y_bara,gam_bar)
for (i in 2:num) {
#new value of theta from full conditional
gibbs[i,1] = rgamma(1,y_bara*na+y_barb*nb+at,na+nb*gibbs[i-1,2]+bt)
#new value of gamma from full conditional
gibbs[i,2] = rgamma(1,y_barb*nb+gam[j],gam[j]+na*gibbs[i-1,1])
}
exp[j]=mean(gibbs[,1]*gibbs[,2]-gibbs[,1])
}
(exp)
