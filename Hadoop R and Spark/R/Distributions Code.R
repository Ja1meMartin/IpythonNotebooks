a <- var(replicate(10000, var(as.integer(rbinom(2,10,0.5)))))
b <- var(replicate(10000, var(as.integer(rbinom(100,10,0.5)))))
c <- var(replicate(10000, var(as.integer(rbinom(10000,10,0.5)))))
a/b
b/c


a <- var(replicate(10000, var(as.integer(rbinom(2,10,0.9)))))
b <- var(replicate(10000, var(as.integer(rbinom(100,10,0.9)))))
c <- var(replicate(10000, var(as.integer(rbinom(10000,10,0.9)))))
a/b
b/c

n <- 0:10
plot(n, dbinom(n, 10, 0.9), main="PDF for Binomial 10 trials 0.9 Success")


d = 1:5
e = d/sd(d)+ 10*mean(d)


x <- seq(0,10,0.2)
plot(x, pbinom(x, 10, 0.5), pch=1, col = c("red", "black"), 
     ylab="Cumalitive Probability", xlim=c(0,10), 
     main="CDF of Binomi Distribution with Even Probability",
     ylim=c(0,1))

two = table(rbinom(2,10,0.5))
two/2
two
plot(two/2)

x = 0:10
par(mfrow=c(1,2))
hist(rbinom(2,10,0.5)/2, breaks=seq(-0.5,10.5,1), xlab= "Successes",
     main="2 Experiments", freq=F,
     col = c("black", "red"))
plot(x, dbinom(x,10,0.5), ylim=c(0,0.5), main="Density of Successes",
     xlab="Successes", ylab="Density")

par(mfrow=c(1,2))
hist(rbinom(100,10,0.5), breaks=seq(-0.5,10.5,1), xlab= "Successes",
     col = c("black", "red"), ylim=c(0,0.5), freq=F,
     main = "100 Experiments")
hist(rbinom(10000,10,0.5), breaks=seq(-0.5,10.5,1), xlab= "Successes",
     col = c("black", "red"), ylim=c(0,0.5), freq=F,
     main = "10000 Experiments")