#simulation of Euro-call use Stratified Sampling
#key point: simulate normal random number evenly from the partitions
#theoretical price = 1.0139

n = 1000 #no. of path
B = 500 #no. of bins 
NB = n/B #sample r.v. in each bin?
S0 = 10
K = 12
mu = 0.03
sigma = 0.4
nu = mu-sigma^2/2
t =1
u = 0
z =0
ST = rep(0,NB) #for each interval
Ci = rep(0,NB)
Ci.bar=0
varr = 0

set.seed(2)
for (i in 0:(B-1))
{
  u = runif(NB) # in one bin
  z = qnorm((u+i)/B)
  
  for(j in 1:NB) #ugly loop
  {
    ST[j] = S0*exp(nu*t +sigma*sqrt(t)*z[j])
    Ci[j] = exp(-mu*t)*max(ST[j]-K,0)
  }
  
  Ci.bar = Ci.bar +mean(Ci)
  varr = varr + var(Ci)
}

(C = Ci.bar/B)
(SE = sqrt(varr/NB)/B)
