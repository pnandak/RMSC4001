#box-muller normal polar form


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