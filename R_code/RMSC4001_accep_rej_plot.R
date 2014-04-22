x = seq(0,30,by=1e-2)
normden = function(x,u,sigma){
  f = (sqrt(2*pi*sigma^2))^-1*exp(-(x-u)^2/(2*sigma^2))
}

plot(x,normden(x,15,5),type="l",ylim=c(0,0.11),ylab="density",main="normal vs. traingular dist")
segments(5,0,15,0.1,col="red")
segments(15,0.1,25,0,col="red")

c = 0.1/normden(15,15,5)
c=1.26
points(x,c*normden(x,15,5),col="blue",type="l")


x1 = seq(0,11,by=1e-2)
expden = function(x,lambda){
  f = lambda * exp(-x*lambda)
}
plot(x1,expden(x1,1/10),type="l",ylim=c(0,0.2),ylab="density",main="exp vs traingular dist")
segments(0,0.1,10,0,col="red")
points(x1,1.05*expden(x1,1/10),col="blue",type="l")










