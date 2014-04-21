#simulation of BM
BMsim = function(npaths,nSamples)
{
  p = npaths
  N = nSamples
  y = matrix(rep(0,(N+1)*p),nrow = N+1)
  t = c(0:N)/N
  
  for(j in 1 :p)
  {
    z =rnorm(N,0,1)
    y[1,j]=0 #initial state
    for(i in 1:N)
    {
      y[i+1,j]=(1/sqrt(N))*sum(z[1:i])
    }
  }
  matplot(t,y,type="l",ylab="Brownian motion")
  
}

set.seed(1)
BMsim(1,1000)
BMsim(5,1000)
BMsim(10,1000)
BMsim(100,1000)


