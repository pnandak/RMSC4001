#amercian style floating arithmetic asian put option

N = 10000 #number of indenp. path
n = 100
dt = 1/n
r = 0.03
sigma = 0.4
S0 = 10
nu = r -0.5*sigma^2

stock = matrix(0,N,n+1)
K = matrix(0,N,n+1) #use to comptute arith-average
y = c(rep(0,N)) #use to store ... at each step for N path
put= rep(0,N)
stock[,1] = S0
K[,1] = S0
A = matrix(0,6,n)  #to store regression coefficients
check1 = proc.time()

#generate asset price path and realized average
for ( j in 1:n){#N indenp. path at same time
	stock[,j+1] = stock[,j]*exp(nu*dt+sigma*sqrt(dt)*rnorm(N,0,1))
	K[,j+1] = K[,j] +stock[,j+1]
}
for(j in 1:(n+1)) K[,j] = K[,j]/j
y = pmax((K[,n+1]-stock[,n+1]),0) # terminal payoff


#compute conditional expectation function
for(j in n:3){ #from last step up to initial 3 step
	#collect in-the-money paths to do regression- which means valuable to exercise
	a = which(stock[,j] < K[,j])
	if(length(a) >= 6){ #at least exists 6 path to perform regression 
		S = stock[a,j] #get stock price 
		zeta = K[a,j] #strike price
		#why regression here not return 6 coef?
		A[,j] = coef(lm(y[a]~S + I(S^2) + zeta + I(zeta^2) + S*zeta))
		xx = c(rep(1,N),stock[,j],stock[,j]^2,K[,j],K[,j]^2,stock[,j]*K[,j])
		X = matrix(xx,ncol = 6) #prepare data matrix
		put = X %*% A[,j] #y-hat
		put = exp(-r*dt)*pmax(put,0) #discount one step
		#find S that is in the exercising region
		b = which((K[,j]-stock[,j])>put)
		y = exp(-r*dt)*y #dsicount one step back to compare
		y[b] = K[b,j] - stock[b,j] #assign y as K-S if K-S >p

	}
	else {y = exp(-r*dt)*y}
}

price = exp(-2*r*dt)*mean(y)



