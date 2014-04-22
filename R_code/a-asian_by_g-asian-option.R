#define parameter

N = 1000
TIME = 1
time = 0.2
r = 0.03
sigma = 0.4
St = 10
K =12
At = 10.5
Gt = 10.5
nu = r-sigma^2/2
dT = TIME - time
dt = TIME/100
n = floor(dT*100)
S = c(rep(St,N)) #N path, each entry store one path
A = c(rep(At*(100-n)+St,N))
InG = c(rep(log(Gt)*(100-n)+log(St),N))
CA  = c(rep(0,N))
CG = c(rep(0,N))

#generate asset price paths and averages
for (j in 1:n){
	S= S*exp(nu*dt+sigam*sqrt(dt)*rnorm(N))
	A = A+S
	InG = InG +log(S)
}

A = A/101
InG = InG/101
CA = exp(-r*dT)*pmax(A-K,0)
CG = exp(-r*dT)*pmax(exp(InG)-K,0)
mean(CG)

#compute analytical solution of geometric Asian option ptice
SIGMA = sigam^2*dT^3/(3*TIME^2)
d1= (log(St/K)+time/TIME*log(Gt/St)+nu*dT^2/2/TIME+SIGMA)/sqrt(SIGMA)
d2 = d1 - sqrt(SIGMA)
RtT = nu*dT^2/2/TIME + SIGMA/2 - r*dT
(CGfix = St*(Gt/St)^(time/TIME)*exp(RtT)*pnorm(d1)-K*exp(-r*dT)*pnorm(d2))
#0.13175 by theory


#compute regression coefficients
a = which(CA*CG !=0)
aa = coef(lm(CA~CG)) #regressioj
(price = aa[1]+CGfix*aa[2]) #evulate at mean








