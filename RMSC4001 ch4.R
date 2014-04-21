#ch4 generating random variables

#ex1- binomial n=10, p =0.7
n=10
p =0.7


#simulate binomial rv with n, p
gbin = function(number, p,n){
x = c()

for (k in 1 : number){
  c=p/(1-p)
  pr = (1-p)^n
  F= pr
  u= runif(1)

#actually can pre-compute the cdf 
for (i in 0:n){
  if(u <F){
    x = c(x,i)
    break
  }
  else{
    pr = c*(n-i)/(i+1)*pr
    F = F+pr
  }
  }
}

x
}


#simulate exp(k)

simexp = function(number, lambda)
{
  u = runif(number)
  x = -1/lambda*log(u)
  x
}

#simulate Gamma(n,lambda)
simGamma=  function(number,n,lambda)
{
  x = simexp(number,lambda)
  for(i in 1:n-1) x = x+simexp(number,lambda)
  
  x
}


#simulate f(x)=20x(1-x)^3 0<x<1 by using u(0,1)
k<- 1000   #target number of variable
x <- c(rep(0,k)) 
u1 <- c(rep(0,k))  #g
u2 <- c(rep(0,k)) 
c1 <- 0
y <- c(rep(NA,k))

for(i in 1:k)
{
  u1[i]=runif(1)
  u2[i]=runif(1)
  
  if(u2[i]<(256/27)*u1[i]*(1-u1[i])^3) x[i]=u1[i]   #meet the condition, x =y
  if(x[i]==0) c1=c1+1 #count number of rejected y values 

}
c1/k  #reject rate, expect: 64/135
for (i in 1:k)
{
if (x[i] != 0 ) y[i] <- x[i]
}
y[1:20]
hist(na.omit(y) ,prob=T)
lines(density(na.omit(y)))

#generare multivariate normal





