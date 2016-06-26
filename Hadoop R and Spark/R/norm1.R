
br = seq(-5,5,1/3)
colors = c("red", "black")
mean = 0
sd = 1

par(mfrow=c(1,3))
x <- rnorm(2,mean,sd)
hist(x, breaks = br, col = colours, main = "2 trials N.D.")

x <- rnorm(100,mean,sd)
hist(x, breaks = br, col = colours, main = "100 trials N.D.")

x <- rnorm(10000,mean,sd)
hist(x, breaks = br, col = colours, main = "10000 trials N.D.")



par(mfrow=c(1,2))
br = seq(-6,6,0.5)

x <- rnorm(10000,mean,sd)
hist(x, breaks = br, col = colours, main = "Mean 0, SD 1")

mean = 1

x <- rnorm(10000,mean,sd)
hist(x, breaks = br, col = colours, main = "Mean 1, SD 1")

br = seq(-5,5,1/3)
colors = c("red", "black")
mean = 0
sd = 1
par(mfrow=c(1,2))

x <- rnorm(10000,mean,sd)
hist(x, breaks = br, col = colours, main = "Mean 0, SD 1")

sd = 0.5

x <- rnorm(10000,mean,sd)
hist(x, breaks = br, col = colours, main = "Mean 0, SD 0.5")


mean = 1
sd=0.25
colours = c("red", "black")
par(mfrow=c(1,3))
x <- rnorm(2,mean,sd)
hist(x, breaks = br, col = colours)

x <- rnorm(100,mean,sd)
hist(x, breaks = br, col = colours)

x <- rnorm(10000,mean,sd)
hist(x, breaks = br, col = colours)

shadebelow <- function(x, mean = 1, sd = 4){
  shade.norm(x, sigma=sd, mu=mean, tail="lower", main="")
}

shadeabove <- function(x, mean = 1, sd = 4){
  shade.norm(x, sigma=sd, mu=mean, tail="lower")
}

library("asbio")
shadebelow(3)
pnorm(3,1,4)
pnorm(1.5,1,2, lower.tail = F)
1 - (pnorm(1,1,2) + pnorm(1,1,2, lower.tail = F))
pnorm(5,1,2) - pnorm(2,1,2)
pnorm(0,1,2, lower.tail = F)
pnorm(0.5,1,2) - pnorm(-1,1,2)
pnorm(-2,1,2) + pnorm(2,1,2, lower.tail = F)
# one to go here

# Electricity

odds = pnorm(118, 120, 2) - pnorm(116, 120, 2)
odds ** 3


# How to show that it is symmetric?

# Show the CDF
x <- seq(-6,6,0.5)
plot(x, pnorm(x), pch=1, col = c("red", "black"),
     ylab="Cumalitive Probability", xlab = "Standard Deviations",
     main="CDF of Normal Distribution",
     ylim=c(0,1))



