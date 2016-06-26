cols = c("blue", "red", "green", "yellow", "pink", "cyan")

par(mfrow = c(1,3))

hist(as.integer(runif(2,1,7)), col = cols, breaks=seq(0.5,6.5,1), 
     xlab="Number Rolled", main ="Roll 2 Times")
hist(as.integer(runif(100,1,7)), col = cols, breaks=seq(0.5,6.5,1), 
     xlab="Number Rolled", main ="Roll 100 Times")
hist(as.integer(runif(10000,1,7)), col = cols, breaks=seq(0.5,6.5,1), 
     xlab="Number Rolled", main ="Roll 10,000 Times")

cols = c("red", "black")
hist(rbinom(2,10,0.5), breaks=seq(-0.5,10.5,1), xlab= "Successes",
     main= "Binom (2,10,0.5)", col = cols)
hist(rbinom(100,10,0.5), breaks=seq(-0.5,10.5,1), xlab= "Successes", 
     main= "Binom (2,10,0.5)", col = cols)
hist(rbinom(10000,10,0.5), breaks=seq(-0.5,10.5,1), xlab= "Successes", 
     main= "Binom (10000,10,0.5)", col = cols)

hist(rbinom(2,10,0.9), breaks=seq(-0.5,10.5,1), xlab= "Successes",
     main= "Binom (2,10,0.9)", col = cols)
hist(rbinom(100,10,0.9), breaks=seq(-0.5,10.5,1), xlab= "Successes", 
     main= "Binom (2,10,0.9)", col = cols)
hist(rbinom(10000,10,0.9), breaks=seq(-0.5,10.5,1), xlab= "Successes", 
     main= "Binom (10000,10,0.9)", col = cols)