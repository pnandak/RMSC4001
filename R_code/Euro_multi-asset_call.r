# simulate euro multi-asset call option

S0 = c(10,10)
r = 0.5
sigma1 = 0.3
sigma2 = 0.4
rho = 0.2
T = 0.5
N = 10000
z1 = rnorm(N)
z2= rnorm(N)
x1 = z1
x2 = z1*rho + sqrt(1-rho^2)*z2

S1 = S0[1]*exp((r-sigma1^2)*T + sigma1*sqrt(T)*x1)
S2  = S0[2]*exp((r-sigma2^2)*T + sigma2 * sqrt(T)*x2) 
c = mean(max(S1-S2,0))*exp(-r*T)
c











