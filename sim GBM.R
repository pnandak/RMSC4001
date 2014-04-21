#simulation of Geomatric Brownian Motion
#dx(t)= udt + sigma * dwt
#s(t) =e^x(t)
#ds(t) = u*s(t)*dt + sigma*s(t)*dwt
#-> dlogs(t)=(u-1/2*sigma^2)dt+sigma*dwt

#main idea: first simaultion BM then multiply

#eg- mu = 0.03 sigma = 0.2

p =20 # num of path
N = 1000 #no of smaples in one path
S0 =1 #current stock price
mu = 0.03 #mean value
sigma = 0.2 #sd
nu = mu - sigma^2/2 #您怎么称呼...

x = matrix(rep(0,(N+1)*p),nrow = N+1) #store BM
y = matrix(rep(0,(N+1)*p),nrow = N+1) #store GBM
t= c(0:N)/N #step

for(j in 1:p)
{
  z = rnorm(N,0,1)
  x[1,j]=0
  y[1,j]=S0 #starting point
  
  for(i in 1:N)
  {
    x[i+1,j]=(1/sqrt(N))*sum(z[1:i])
    y[i+1,j]=y[1,j]*exp(nu*t[i+1]+sigma*x[i+1,j])
  }
}

matplot(t,y,type="l",xlab="t",ylab='GBM')


