#gibbs sampling  for multivarite normal

u1 = 1
u2  =1
sigma=1
sigma =2
col = 0.5
n = 50000
M = 5000
n=n+M

i = 1
y=  rep(0,n+1)
x=  rep(0,n+1)

for (i in 2 : n){
	x[i] = col*y[i-1]+sqrt(1-col^2)*rnorm(1)
	y[i] = col*x[i] +sqrt(1-col^2)*rnorm(1)
}

plot(x[M:n+1],y[M:n+1],type = "p")