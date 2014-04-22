#normal tail probabilty using importance sampling

#p(x >a)
rGauss = function(n=1){
	
	re = rep(0,n)
	for(i in 1:n){
	repeat
	{v1= 2*runif(1,0,1)-1 #unif(-1,1)
	v2 = 2*runif(1,0,1)-1
	r = v1^2 + v2^2
	if(r < 1) break
	}
	re[i] =  sqrt(-2*log(r)/r)*v1
	}

	re
}

hist(rGauss(10000),30)
r = rGauss(10000)
qqnorm(r)


#=========================================#
a = 2
n = 1000
m = 100 
#function to generate exp(a)

p = rep(0,m)
for(i in 1:m){

	u = runif(n,0,1)
	exp = -1/a*log(u)
	p[i] = 1/a*mean(exp(a*exp)*(1/sqrt(2*pi)*exp(-(exp+a)^2/2)))

}
mean(p)
var(p)
#simplify the expression
#p = (exp(-a^2/2)/(a*sqrt(2*pi)))*mean(exp(-exp^2/2))

#most accurate value
1-pnorm(a)

#compare with direct simulation
p1 = rep(0,m)
for(i in 1:m){

	rn = rGauss(n) #here we use user-difined 
	p1[i] = length(which(rn>=a))/n

}
mean(p1)
var(p1)


















