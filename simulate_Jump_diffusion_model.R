#simulation of Jump diffusion model
#strange....


	MUY = 0.08
	SIGMAY = 0.4
	MUJ = 0
	SIGMAJ = 0.3
	LAMBDA = 3.5

	m = 100
	n = 2500
	dt = 1/n


	Y = matrix(log(100),m,n+1)
	#Y = matrix(100,m,n+1)

	for(i in 1:n){
		JUMP = ifelse(runif(m)<LAMBDA*dt,1,0)
		JumpSize = JUMP*rnorm(m,MUJ,SIGMAJ)
		Y[,i+1] =Y[,i] +rnorm(m,MUY*dt,SIGMAY*sqrt(dt))+JumpSize
	}
	Y = exp(Y)

	plot(Y[1,],type="l",ylim=c(30,200))
	for (k in 2:5){
		points(Y[k,],type="l")
	}