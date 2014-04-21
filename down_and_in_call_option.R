#Down-and-in call option

S0 = 10
K = 12
V = 9   # "barrier"
T = 1
r = 0.03
sigma = 0.4
dt = 1/250  # daily prices
nu = r - sigma^2/2
N =1000

t = (1:(1/dt))*dt # time step
ST = rep(0,1/dt)
Ci = rep(0,N)

set.seed(1)
for(j in 1:N){ # N path

	z = rnorm(1/dt)
	for( i in 1:(1/dt)){
		ST[i] = S0 * exp(nu*t[i]+sigma*sqrt(dt)*sum(z[1:i]))
	}
 
	if (min(ST)<V) Ci[j] = exp(-r*T)*max(ST[1/dt]-K,0) # textbook has typo?
	else Ci[j] = 0

}

C.bar = mean(Ci)










