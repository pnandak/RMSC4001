#simulate the historical stock price 


#naive method: simulate as a portfolio jointly
n = 250
r  =0.05
dt = 1/n  #time step
sigma = c(0.4,0.3,0.35) #variance for each stock
SIGMA = matrix(c(1,0.3,0.3,0.3,1,0.3,0.3,0.3,1),3,3) #correlation matrix
SIGMA = SIGMA * (sigma %*% t(sigma)) #get covariance matrix

S = matrix(0,3,n+1)
S[,1] = c(100,90,80) #initial price for 3 stock
for (i in 1:n){
	#need to adjust r?
	S[,i+1] = S[,i]*exp(r*dt+rnorm(3)%*%chol(SIGMA)*sqrt(dt)) #use cholesky decomp to simulate covariance structure
}

P = S[1,]+S[2,]+S[3,] #price path for whole portfolio
print(P)
plot(P,type="l")

returnS = diff(t(log(S))) #log return for each day
returnP = diff(log(P))

cov(returnS) #estimaiton of covariance of log return
(A = var(returnS)/dt)
(B = var(returnP)/dt)
C = S[,n+1] #terminal price

time = (0:n)/n #plot realization of 3 individual stocks
range = c(min(S)*0.5,max(S)*1.2)
plot(time,S[1,],type="l",xlim=c(0,1.6),ylim=range,ylab="stock price",col=2)
lines(time,S[2,],type="l")
lines(time,S[3,],type="l",col=3)
lines(c(1,1),range,type="l")


#simulate scenarios
#BS formula
K = 250
t = 0.5
d1 = (log(sum(C)/K)+(r+B^2/2)*t)/sqrt(B*t)
d2 = d1 = sqrt(B*t)
sum(C)*pnorm(d1)-K*exp(-r*t)*pnorm(d2)

#from t=1 to t=1.5
N = 20000
S = matrix(0,N,3)
S[,1] = C[1];S[,2]=C[2];S[,3]=C[3]

for(i in 1 :125){
	S = S*exp(r*dt + matrix(rnorm(3*N),N,3)%*%chol(A)*sqrt(dt))
}
P = S[,1]+S[,2]+S[,3]
price = exp(-r*T)*pmax(P-K,0)
mean(price)


#plot 20 sample path for S
for(i in 1:20){
	lines(c(1,1.5),c(C[1],S[i,1]),type="l",col=2)
	lines(c(1,1.5),c(C[2],S[i,2]),type="l")
	lines(c(1,1.5),c(C[3],S[i,3]),type="l",col=3)

}
















