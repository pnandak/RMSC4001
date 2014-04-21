#to make sure random number fall into all quantiles

#standard method
N = 2000
set.seed(5)
U1 = runif(N)
X1 = qnorm(U1)
par(mfrow=c(1,2))
hist(X1, freq=F,xlim= range(-3.5,3.5),nclass = 100)

#Strafied Sampling
U2 = runif(N)
i = 0:(N-1)
V = (U2+i)/N
X2 = qnorm(V)
hist(X2, freq=F,xlim= range(-3.5,3.5),nclass = 100)



