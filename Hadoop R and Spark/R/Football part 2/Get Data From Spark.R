library(reshape2)


soccerDF = read.csv("engsoccerdata2.csv")
attach(soccerDF)


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


create_league_records <- function(soccerDataFrame) {
  
  soccerDataFrame$gd = soccerDataFrame$hgoal-soccerDataFrame$vgoal
  t = groupBy(soccerDataFrame, "home", "Season", "tier")
  
  gd <- summarize(t, gd = sum(soccerDataFrame$gd))
  
  agg(
    t,
    GP = n(soccerDataFrame$gd), 
    W = sum(ifelse(soccerDataFrame$gd > 0, 1, 0 )),
    D = sum(ifelse(soccerDataFrame$gd == 0, 1, 0 )),
    L = sum(ifelse(soccerDataFrame$gd < 0, 1, 0 )),
    
    # Do this after
    # use if else to not repeat 38
    GF = sum(soccerDataFrame$hgoal),
    GA = sum(soccerDataFrame$vgoal),
    GD = sum(soccerDataFrame$gd)
  ) -> a
  
  
  
  a <- filter(a, a$GP > 10)
  
  a$GFpg = round(a$GF*38/a$GP)
  a$GApg = round(a$GA*38/a$GP)
  a$GDpg = round(a$GD*38/a$GP)
  points_for_win = 3
  a$Ppg = round((a$W*points_for_win + a$D)*38/a$GP)
  a$GDRatio = round((a$GFpg/a$GApg)* 100)/100 # Round to 2 places
  
  data = collect(a)
  
  return(data)
}

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

data = collect(a)
saveRDS(data, file="allData.Rda")

home_games <- select(soccerDF, "Season", "tier", "home", "visitor", "hgoal", "vgoal")
home_records <- create_league_records(home_games)

away_games <- select(soccerDF, "Season", "tier", "visitor", "home", "vgoal", "hgoal")
away_records <- create_league_records(away_games)

secondHalfDF <- filter(soccerDF, soccerDF$quarter > 2 )
firstHalfDF <- filter(soccerDF, soccerDF$quarter <= 2 )


firstHalfLeague <-
  rbind(
    select(firstHalfDF, "Season", "tier", "home", "visitor", "hgoal", "vgoal"),
    select(firstHalfDF, "Season", "tier", "visitor", "home", "vgoal", "hgoal")
  )

secondHalfLeague <-
  rbind(
    select(secondHalfDF, "Season", "tier", "home", "visitor", "hgoal", "vgoal"),
    select(secondHalfDF, "Season", "tier", "visitor", "home", "vgoal", "hgoal")
  )

firstHalfRecords <- create_league_records(firstHalfLeague)
secondHalfRecords <- create_league_records(secondHalfLeague)

saveRDS(home_records, file="homeData.Rda")
saveRDS(away_records, file="awayData.Rda")
saveRDS(firstHalfRecords, file="firstHalfData.Rda")
saveRDS(secondHalfRecords, file="secondHalfData.Rda")
