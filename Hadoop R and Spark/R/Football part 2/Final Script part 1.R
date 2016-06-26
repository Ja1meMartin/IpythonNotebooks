###
# Load libraries
###
library(ggplot2)
#library(lattice)
library(reshape2)


soccerDF = read.csv("engsoccerdata2.csv")
attach(soccerDF)


###
# Find the number of individual matchdays
###
length(unique(Date))

###
# Get the matches from the first soccer matchday
# Better than using head() which shows Accrington's first 6 games
# Date[order(Date)] Order Date column by Date
# Date[order(Date)][1] Select first factor
###
Date[Date == Date[order(Date)][[1]][[1]], ]
subset(soccerDF, Date == Date[order(Date)][[1]])
# soccerDF[Date == Date[order(Date)][1], ] produces an extra row of NA's


###
# Check for lines with missing data
###
soccerDF[!complete.cases(soccerDF),]

###
# Convert Season and tier to Factor
###
soccerDF$Season = as.factor(Season)
soccerDF$tier = as.factor(tier)

###
# Look at the summaries of each column
###
summary(soccerDF)

###
# When did we last have the full complement of league games?
###
z <- aggregate(totgoal, list(Date = Date), NROW) # Count games on each day
y <- z[z$x==46, ] # Pick days with 46 games
tail(y[order(y$Date),])


###
# Dates with most goals scored
###
z <- aggregate(totgoal, list(Date = Date), sum) # Goals on each date
head(z[order(-z$x),]) # All occur between 1927-36
z[order(-z$x)[7:20],] # Look at the next values

###
# Seasons with most goals scored
###
z <- aggregate(totgoal, list(Season = Season), mean) 
head(z[order(-z$x),]) # All occur between 1927-36
plot(z, main="Goals Per Game Per Season", ylab="Goals Per Game", ylim=c(0,5))

acf(z[, 2], main = "Autocorrelation with Varying Lags")
acf(z[z$Season >= 1970, 2], main = "Autocorrelation with Varying Lags 1970+")

# Ratio of variances of all data vs 1970+
var(z[, 2])
var(z[z$Season >= 1970, 2])
var(z[, 2]) /var(z[z$Season >= 1970, 2])

# Ratio of slopes of linear model of all data vs 1970+
t1 = 1:length(z[, 2])
lm1 <- lm(z[, 2] ~ t1)
t2 <- 1:length(z[z$Season >= 1970, 2])
lm2 <- lm(z[z$Season >= 1970, 2] ~ t2)
abs(lm1$coefficients[2]/lm2$coefficients[2])

###
# Who has played the most home games
###
y <- aggregate(totgoal, list(Team = visitor), NROW) 
z <- aggregate(totgoal, list(Team = home), NROW)
x <- merge(z,y)

###
# Most common results pre-post
# Find most common results overall
# Subset soccerDF into pre 1920, 1921-69 and 1970 on
# Show how most common results fared in the subsets
###

y <- aggregate(Season, list(Result = FT), NROW)
y <- y[order(-y$x),]
common_results <- y$Result[1:10]
common_percents <- y[1:10,2]/NROW(Season)

z1 = subset(soccerDF, as.numeric(as.character(Season)) <= 1920, "FT")
z = soccerDF[as.numeric(as.character(Season)) > 1920, c("Season","FT")]
z2 = subset(z, as.numeric(as.character(Season)) <= 1970, "FT")
z3 = subset(z, as.numeric(as.character(Season)) > 1970, "FT")

round(table(z1)[common_results]*100/(common_percents*nrow(z1)),digits=1)
round(table(z2)[common_results]*100/(common_percents*nrow(z2)),digits=1)
round(table(z3)[common_results]*100/(common_percents*nrow(z3)),digits=1)

###
# Goals Scored
# Subset soccerDF into pre 1920, 1921-69 and 1970 on
# Create pie charts for subsets
# Pl
###
cols = c("hgoal","vgoal","totgoal","goaldif")
z1 = subset(soccerDF, as.numeric(as.character(Season)) <= 1920, cols)
z = soccerDF[as.numeric(as.character(Season)) > 1920, c("Season", cols)]
z2 = subset(z, as.numeric(as.character(Season)) <= 1970, cols)
z3 = subset(z, as.numeric(as.character(Season)) > 1970, cols)


summary(z1); summary(z2); summary(z3)


slices <- function(df){
  return (c(sum(df$hgoal), sum(df$vgoal)))
}
lab = c("Home", "Away")

# Pie Charts of total goals home and away
pie(slices(soccerDF), lab, main="All")
pie(slices(z1), lab, main="Pre 1920")
pie(slices(z2), lab, main="1920-1969")
pie(slices(z3), lab, main="After 1970")

# Percentage of total goals home and away
round(slices(soccerDF)/sum(totgoal), digits=2)
round(slices(z1)/sum(z1$totgoal), digits=2)
round(slices(z2)/sum(z2$totgoal), digits=2)
round(slices(z3)/sum(z3$totgoal), digits=2)



###
# Distribution of goals at home, away, and total
###
sample_size = NROW(soccerDF)

goals = melt(soccerDF[c("hgoal","vgoal","totgoal")])
goals_count = aggregate(goals,  
                        by=list(goals= goals$value, type=goals$variable), FUN=NROW)

ggplot(data=goals_count,
       aes(x=goals, y=variable/sample_size, colour=type)) +
  geom_line() + ggtitle("Percentage of Matches With X Goals") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

###
# Distribution of Goal Differences
###
goals_diff= aggregate(goaldif,  
                        by=list(diff=goaldif), FUN=NROW)

ggplot(data=goals_diff,
       aes(x=goals_diff$diff, y=goals_diff$x/sample_size)) +
  geom_line() + ggtitle("Percentage of Matches With X Goal Difference") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

###
# QQPlot and Best-Fit Line for hgoal, vgoal, totgoal
###
par(mfrow=c(1,3))

qqnorm(hgoal, main = "Home Goals"); qqline(hgoal, col = 2,lwd=2,lty=2)
qqnorm(vgoal, main = "Away Goals"); qqline(hgoal, col = 2,lwd=2,lty=2)
qqnorm(totgoal, main = "Total Goals"); qqline(hgoal, col = 2,lwd=2,lty=2)


###
# QQ for Poisson Distrib
###
means = c(mean(hgoal), mean(vgoal), mean(totgoal))
labs = c("Home Goals", "Away Goals", "Total Goals")

x <- dcast(goals_count, goals~type)
x[is.na(x)] <- 0


for (i in 1:3 ) {
  mean = means[i]
  y = x[[i+1]]/sample_size
  plot(dpois(x[[1]], mean), y, xlab="Poisson Density", ylab="Sample Density", main=labs[[i]])
  abline(0,1)
}





###
# Goals per season by tier
###
z <- aggregate(totgoal, list(Season = Season, Tier=tier), mean) 
qplot(z$Season,z$x, round, main="Goals Per Game Per Season", xlab="Season", 
      ylab="Goals Scored Per Game", ylim=c(2,5), 
      color=-z$Tier, geom="line")

z1<-z[z$Season >= 1958,]

###
# Correlations and Scatterplot Matrix for Season and 4 Tiers
###
z_wide <- dcast(z1, formula = z1$Season ~ z1$Tier)

# Add correlation to matrix
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(z_wide,lower.panel = panel.smooth, upper.panel = panel.cor)

detach(soccerDF)
###
# Calculate League Tables
# REQUIRES: SparkR
###

library(SparkR)

sc	<-	sparkR.init()
sqlContext <- sparkRSQL.init(sc)

df <- createDataFrame(sqlContext, soccerDF)
soccerDF <- filter(df, df$Season >= 1971) #createDataFrame(sqlContext, engsoccerdata2)
cache(soccerDF)


###
# Create League Tables
###

league <-
  rbind(
    select(soccerDF, "Season", "tier", "home", "visitor", "hgoal", "vgoal"),
    select(soccerDF, "Season", "tier", "visitor", "home", "vgoal", "hgoal")
  )
cache(league)




# Season data
league$gd = league$hgoal-league$vgoal
t = groupBy(league, "home", "Season", "tier")


# best goal differences
gd <- summarize(t, gd = sum(league$gd))
cache(gd)
showDF(arrange(gd, desc(gd$gd)))
showDF(arrange(gd, asc(gd$gd))) # show DF from bottom

agg(
  t,
  GP = n(league$gd), 
  W = sum(ifelse(league$gd > 0, 1, 0 )),
  D = sum(ifelse(league$gd == 0, 1, 0 )),
  L = sum(ifelse(league$gd < 0, 1, 0 )),
  
  # Do this after
  # use if else to not repeat 38
  GF = sum(league$hgoal),
  GA = sum(league$vgoal),
  GD = sum(league$gd)
) -> a



a <- filter(a, a$GP > 10)

a$GFpg = round(a$GF*38/a$GP)
a$GApg = round(a$GA*38/a$GP)
a$GDpg = round(a$GD*38/a$GP)
points_for_win = 3
a$Ppg = round((a$W*points_for_win + a$D)*38/a$GP)
a$GDRatio = round((a$GFpg/a$GApg)* 100)/100 # Round to 2 places


cache(a)

# Best and Worst GD of All Time
showDF(arrange(a, desc(a$Ppg)))
showDF(arrange(a, asc(a$Ppg)))

# 2014-15 Premier League Table
showDF(arrange(filter(a, a$Season >= 2014 & a$tier == 1 ), desc(a$Ppg)))







###
# Leave for part 2 as hypothesis test
# Calculate Complement 
# Poisson goes to infinity which we can not represent so our
# last bin will be one representing P(X >= x)
# For Chi-square all bins must have E(x) > 5
# Therefore I have but 11+ goals in one bin 
# comp * sample_size -> 18.4306
###
comp = 0
max_goals = 13

while (comp*sample_size < 5){
  max_goals = max_goals - 1
  probs = dpois(0:max_goals, lambda=means[1])
  comp = (1-sum(probs))
  
}

probs

stopifnot(comp*sample_size > 5)

z = x[1:2]

while (nrow(z) > length(probs)+1){
  # make the second value in the second-last row 
  # equal the sum of the values in the last two rows
  z[nrow(z)-1,2] = z[nrow(z)-1,2] + z[nrow(z),2]
  
  # take all but the last row
  z = z[1:nrow(z)-1,]
}


tail(z)
chisq.test(z[,2]/sample_size, p=c(probs, comp))