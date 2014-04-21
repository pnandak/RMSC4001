#use gibbs samplimng to estimate Normal with unknow mean and variance

#simulate
require(pscl)
n = 10000
x = rnorm(n,2,5)


K = 10000
M = 1000
u = rep(0,K)
sigma = rep(0,K)
#set initial value
m = 0 
tao = 1
a = 1
b =1

for (i in 1:K){
	#sample u
	u[i] = rnorm(1,m,tao)
	#update a and b for sigma
	a = n/2 + a
	b = b + 0.5*sum((x-u[i])^2)
	#sample sigma

	sigma[i] = rigamma(1,a,b)

	#update m, tao
	m = (tao*mean(x)+m*sigma[i]/n)/(tao+sigma[1]/n)
	tao = (tao*sigma[i])/(n*tao+sigma[i])
}



mean_pos = mean(u[(M+1):K])
var_pos = mean(sigma[(M+1):K])