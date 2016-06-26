library(reshape2)

soccerDF = read.csv("engsoccerdata2.csv")
attach(soccerDF)

sample_size = NROW(soccerDF)

means = c(mean(hgoal), mean(vgoal), mean(totgoal))

goals = melt(soccerDF[c("hgoal","vgoal","totgoal")])
goals_count = aggregate(goals,  
                        by=list(goals= goals$value, type=goals$variable), FUN=NROW)

labs = c("Home Goals", "Away Goals", "Total Goals")

x <- dcast(goals_count, goals~type)
x[is.na(x)] <- 0



for (i in 1:3 ) {
  mean = means[i]
  y = x[[i+1]]/sample_size
  plot(dpois(x[[1]], mean), y, xlab="Expected Density", 
       ylab="Observed Density", main=labs[[i]])
  abline(0,1)
}


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

chisq.test(z[,2]/sample_size, p=c(probs, comp)) -> chi
