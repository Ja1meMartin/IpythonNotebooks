p = 4.5/3600
n = 3600
par(mfrow=c(1,1))

dbinomial <- dbinom(1:10, n, p)
dpoisson = dpois(1:10, 4.5)

plot(dbinomial,dpoisson)
cor(dbinomial,poisson)

qqplot(dbinomial, dpoisson)


