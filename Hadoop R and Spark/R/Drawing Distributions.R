shadebetween <- function(lower, upper, mean=1, sd=2)
{
  
  par(mfrow=c(1,3))
  
  shadebelow(upper)
  shadebelow(lower)
  
  lb=lower; ub=upper
  
  x <- seq(-4,4,length=100)*sd + mean
  hx <- dnorm(x,mean,sd)
  
  plot(x, hx, type="n", ylab="",
       main="Normal Distribution", axes=FALSE)
  
  i <- x >= lb & x <= ub
  lines(x, hx)
  polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red") 
  
  area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
  result <- paste("P(",lb," < X <",ub,") =",
                  signif(area, digits=3))
  mtext(result,3)
  axis(1, at=seq(mean-4*sd, mean+4*sd, sd), pos=0)

#http://www.statmethods.net/advgraphs/probability.html
}

shadebelow <- function(upper, mean=1, sd=2)
{
  lb=mean-4*sd; ub=upper
  
  x <- seq(-4,4,length=100)*sd + mean
  hx <- dnorm(x,mean,sd)
  
  plot(x, hx, type="n", ylab="",
       main="Normal Distribution", axes=FALSE)
  
  i <- x >= lb & x <= ub
  lines(x, hx)
  polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red") 
  
  area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
  result <- paste("P( X <=",ub,") =",
                  signif(area, digits=3))
  mtext(result,3)
  axis(1, at=seq(mean-4*sd, mean+4*sd, sd), pos=0)
}

shadeabove <- function(lower, mean=1, sd=2)
{
  lb=lower; ub=mean+4*sd
  
  x <- seq(-4,4,length=100)*sd + mean
  hx <- dnorm(x,mean,sd)
  
  plot(x, hx, type="n", ylab="",
       main="Normal Distribution", axes=FALSE)
  
  i <- x >= lb & x <= ub
  lines(x, hx)
  polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red") 
  
  area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
  result <- paste("P( X >",lb,") =",
                  signif(area, digits=3))
  mtext(result,3)
  axis(1, at=seq(mean-4*sd, mean+4*sd, sd), pos=0)
}

mean1 = 0
mean2 = 100
sd1 = 1
sd2 = 10

normal_dist = rnorm(100000, mean1, sd1)
wide_variance_dist = rnorm(100000, mean1, sd2)
high_mean_dist = rnorm(100000, mean2, sd1)

plot(ecdf(normal_dist))
plot(ecdf(wide_variance_dist))
plot(ecdf(high_mean_dist))

breaks = seq(0,1,0.1)

q1 = quantile(normal_dist, breaks)
q2 = quantile(wide_variance_dist, breaks)
q3 = quantile(high_mean_dist, breaks)

plot(q1,q2)
plot(q1,q3)
plot(q2,q3)


