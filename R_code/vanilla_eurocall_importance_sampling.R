###### Part(1): Standard method ########
N = 10000
S0 = 100
K = 140
t = 1
r = 0.05
sigma = 0.3
nu  = r-sigma^2/2

set.seed(1)
Ci = rep(0,N)
for(i in 1:N){
	z = rnorm(1)
	ST = S0*exp(nu*t + sigma*sqrt(t)*z)
	Ci[i] = exp(-r*t)*max(ST-K,0)
}

(C.bar = mean(Ci)) #3.178492
(SE  = sqrt(var(Ci)/(n-1))) #0.7108816

##### Part(2): Importance sampling ########
N = 10000
S0 = 100
K = 140
t = 1
r = 0.05
sigma = 0.3
nu  = r-sigma^2/2
#z-hat
m = 0.5
s = 1.1

set.seed(1)
Ci = rep(0,N)
for(i in 1:N){
	z1 = rnorm(1)
	z2 = m/(sigma*sqrt(t))+s*z1
	ST = S0*exp(nu*t+m+s*sigma*sqrt(t)*z1)
	Ci[i] = s*exp(-r*t)*max(ST-K,0)*exp(z1^2/2 - z2^2/2)
}
(C.bar = mean(Ci)) #3.090862
(SE  = sqrt(var(Ci)/(n-1))) #0.1670396


##### Part(3): Theoritical price ########
#use BS formula
d1 = (log(S0/K)+(r+sigma^2/2)*t)/(sigma*sqrt(t))
d2 = d1 - sigma*sqrt(t)
(C = S0*dnorm(d1)-K*exp(-r*t)*dnorm(d2)) #3.1187


