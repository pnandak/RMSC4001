#set parameter
N = 10000 #number of path
n = 100 #
dt =1/n
r = 0.03
sigma = 0.4
S0 =10
K =12
nu = r -sigma^2/2
stock = matrix(0,N,n+1) #row is each path
y = c(rep(0,N)) #to store 
put = c(rep(0,N))
boundary = c(rep(0,n))
stock[,1] = S0 #set initial price
check1 = proc.time() #check point to count computation time

#generate asset price paths
for(i in 1:n){
	stock[,i+1] = stock[,i]*exp(nu*dt+sigma*sqrt(dt)*rnorm(N))
}

y = pmax((K-stock[,n+1]),0)  #terminal payoff
for(j in n:2){
	a = which(stock[,j]<K) #identify in-the-money paths
	if(length(a) >= 3){ #ensure exists solution for regression
		S = stock[a,j]
		A = coef(lm(y[a]~S + S^2,singular.ok=TRUE)) #compute conditional expectation function
		if(is.na(A[3])) {A[3]=0}
	X = matrix(c(rep(1,N),stock[,j],stock[,j]^2),ncol=3) #X matrix
	put = X %*% A #estimated conditional payoff
	put = exp(-r*dt)*pmax(put,0) #discount + ensure in-the-money, get f(S(t))

	#determine Y & find boundary of K-S(t) < f(S(t))
	b = which((K-stock[,j])>put)
	y = exp(-r*dt)*y #simulated payoff
	y[b] = K-stock[b,j] #assign y as K-S when K-S<P
	if(length(b)==0) {boundary[j]=NA} #boundary cannot be estimated
	else {boundary[j] = max(stock[b,j])}
}
else{
	y = exp(-r*dt)*y
	boundary[j] = NA
}

}
check2 = proc.time()

#plot boundary
boundary[n+1] = K
boundary[1:20]=NA
time = (0:n)/n
plot(time,boundary,type="h",ylim=c(0,K),xlab="time",ylab = "asset price")

(price = exp(-r*dt)*mean(y))
check2 - check1










