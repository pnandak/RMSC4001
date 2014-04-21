#simulation of VaR
#u = 0.003 sigma = 0.23 a = 5% n=10000
#theoritical: VaR = u + siagma*Z(a)

n=10000
alpha = 0.05
k = round(n*alpha)  #get target order
mu = 0.03
sigma = 0.23
r = rnorm(n,mu,sigma)
SR = sort(r)
(VaR = -SR[k])


#simulate positive GED for dow jones
#mu = 0.04% sigma =1.16%
funGED=function(x,v) #density of positive GEM()
{
  lamda<-((2^(-2/v)*gamma(1/v))/gamma(3/v))^(1/2)
  positiveGED= 2*(v*exp(-0.5 *(x/lamda)^v))/(gamma(1/v)*lamda*2^(1+1/v))
}

funEXP<-function(x) {  #density of exp()
  EXP<-exp(-x)
}

x=seq(0,2,0.001)
plot(x,funGED(x,1.21)*exp(x),pch=".:")  #thus set c=1.2 to bound positive GEM

N<-10000
VaR95<-c (rep(0,1000))
VaR99<-c(rep(0,1000))


#simulate weekly stock price
N=52
S0 =10
mu =0.03
sigma=0.4
nu =mu-sigma^2/2
t =(0:N)/N
dt = 1/N
x = rep(0,N+1) #since y[0] store S0
y = rep(0,N+1)
z =rnorm(N)
y[1]=S0

for(i in 1:N){
  x[i+1] = sqrt(dt)*sum(z[1:i])
  y[i+1]=y[1]*exp(nu*t[i+1]+sigma*x[i+1])
}

plot(t,y,type='l',ylab="simulated stock price")


#simulate euro-call option price
#refer to textbook p.80
#by BS formula , call price = 1.013918
p =10000 #number of path
N=52
k=12
S0 = 10
mu =0.03
sigma = 0.4
nu =mu-sigma^2/2
t =(0:N)/N
dt = 1/N
x =matrix(rep(0,(N+1)*p),nrow=N+1)
y =matrix(rep(0,(N+1)*p),nrow=N+1)

for(j in 1:p){
  z = rnorm(N)
  for( i in 1:N){
    x[i+1,j]=sqrt(dt)*sum(z[1:i])
    y[i+1,j]=S0*exp(nu*t[i+1]+sigma*x[i+1,j])
  }
}

ST = y[N+1,]
c = rep(0,p)
c =exp(-mu)*ifelse((ST-k)>0,ST-k,0)

(c.bar=mean(c))
(S= sqrt(var(c)*p/(p-1)))
# why sd(c) give different value?
(c.CI= c.bar+1.96*S/sqrt(p))
(c.CI[1]= c.bar-1.96*S/sqrt(p))


