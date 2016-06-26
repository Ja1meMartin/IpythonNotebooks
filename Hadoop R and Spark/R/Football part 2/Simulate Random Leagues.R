data <- readRDS(file = "allData.Rda")



# split home data for home mean and away mean
# split all data for points
readRDS(file="homeData.Rda") -> home
readRDS(file="awayData.Rda") -> away
readRDS(file="firstHalfData.Rda") -> first
readRDS(file="secondHalfData.Rda") -> second

tiers = levels(as.factor( home$tier))

means <- function(df){
  h <- mean( df$GFpg)/38
  a <- mean( df$GApg)/38
  
  return(c(h,a))
}

# simulate league

simulate_league <- function (number_of_teams, home_goals_mean, away_goals_mean){
  
  number_of_games = number_of_teams^2 # actually n*n-1 but will ignore games on diagonal
  
  print (c("Arguments Simulate League", number_of_teams, home_goals_mean, away_goals_mean))
  
  home_goals = rpois(number_of_games, as.numeric(home_goals_mean))
  away_goals = rpois(number_of_games, as.numeric(away_goals_mean))
  
  diff = home_goals-away_goals
  
  # helper function converts list of goal differences to points
  score_to_points <- function(scores){
    
    points = c()
    
    for (score in scores){
      if (score > 0) {points = c(points, 3)}
      if (score < 0) {points = c(points, 0)}
      if (score == 0) {points = c(points, 1)}
      
    }
    
    return (points)
    
  }
  
  print ("Percentage Draws")
  
  results = matrix(diff, nrow=number_of_teams, ncol=number_of_teams)
  
  print ( sum ( diff==0) / number_of_games)
  
  # For home matches get each row
  # For away matches get the negative of each column
  # Convert to points and sum points
  totals = c()
  
  factor = 38/(2*(number_of_teams-1))
  
  for (j in 1:number_of_teams)
  {
    
    points_gained = factor* sum( score_to_points(
        c(results[j, i:number_of_teams!=j], -results[1:number_of_teams!=j, j])))
    
    if (points_gained == 0){
      print (c("POints Gained is 0 for ", j ))
    }
    
    
    totals = c(totals, points_gained )
  }
  
  return (sort(totals, decreasing = T))
}

points_from_df <- function (df){
  return (sort (as.integer( df$Ppg ), decreasing = T ))
}

split_seasons_by_tier = function(df, season){
  df= df[df$Season==season,]
 
  tiers = as.factor( df$tier)
   
  s <-  split( df, tiers ) 
  dd <- data[data$Season==season, ]
  s1 <- split ( dd, as.factor(dd$tier))
  p1 <- lapply ( s1, points_from_df)
  m <- lapply( s , means) 
  nt <- lapply( s , NROW)
  
  division_names = c("Premiership", "Championship", "League 1", "League 2")
  
  argList <- mapply(c, m, nt,  SIMPLIFY=FALSE)
  argList <- mapply(c, argList, p1,  SIMPLIFY=T)
  argList <- mapply(c, argList, division_names,  SIMPLIFY=T)
  
  print (tail(argList))
  
  ap(argList)
  
}


# helper function for split seasons by tier
ap <- function(argList){
  for (arg in argList){
    
    number_of_teams = as.integer(arg[3])
    home_goals_mean = as.numeric(arg[1])
    away_goals_mean = as.numeric(arg[2])
    
    px <- simulate_league( number_of_teams, home_goals_mean , away_goals_mean)
    pt <- as.numeric(  unlist( arg[4:(length(arg)-1)] ))
    
    plot(px, pt,
         xlab="Simulated Points", ylab="Actual 2014 Points", main = arg[length(arg)],
         xlim=c(0,100), ylim=c(0,100))
    abline(0,1)
    
    print ("Chi Square Test" )
    expected = sample(px, 10000, replace = T)
    observed = sample(pt, 10000, replace = T)
    
    print (chisq.test( expected, observed ) )
  }
}

split_seasons_by_tier(home,2014) -> ss

###
# filter normal distribution
###
norm = rnorm(1500)
normf = norm[norm > 2 & norm < 3]
qqnorm(normf, main="QQPlot for Normal Distribution Between 2 and 3")





###
# Attach points achieved in year t to data from year t-1
# Remove teams playing in different divisions
###
data_subset = data[, c("home", "Season", "tier", "Ppg")]
names(data_subset) = c("home", "Season", "TIER", "PPG")
data_subset[, "Season"] = data_subset[, "Season"] - 1

following_season_join = merge (data, data_subset, by = c("home", "Season"))
attach(following_season_join)
following_season_join = following_season_join[tier==TIER,]

sts_corr <- function(df){
  #attach(df)
  
  tiers = as.factor( df$tier)
  s <-  split( df, tiers )
  
  for (i in s){
  print("\n\n")
  print(c("Corr for Gf", cor(i$GFpg, i$PPG)))
  print(c("Corr for GA", cor(i$GApg, i$PPG)))
  print(c("Corr for GD", cor(i$GDpg, i$PPG)))
  print(c("Corr for Points", cor(i$Ppg, i$PPG)))
  print(c("Corr for GD Ratio", cor(i$GFpg/i$GApg, i$PPG)))
  print(c("Corr for Pythag", cor(i$GFpg^2/(i$GFpg^2 + i$GApg^2), i$PPG^2)))
  print(c("Corr for percentage", cor(i$GApg^2/(i$GApg+i$GFpg)^2, i$PPG^2)))
  
  
  x <- seq(0.1,3,0.1);   y <- c()
  for (exp in x){    y <- c(y, cor(i$GFpg^exp, i$PPG ))  }
  plot (x, y, main = "GF raised to Exponent", xlab="Exponent", ylab="Correlation")
  
  x <- seq(2,5,0.2);   y <- c()
  for (exp in x){    y <- c(y, cor(i$GFpg^exp, i$PPG^exp ))  }
  plot (x, y, main = "GF and PPG raised to Exponent", xlab="Exponent", ylab="Correlation")
  
  
  
  
  x <- seq(0,150,5);   y <- c()
  for (v in x){    y <- c(y, cor((i$GF+v)/(i$GA+v), i$PPG ))  }
  plot (x, y, main="Add constant to Numerator and Denominator of GDRatio",
        xlab="Constant Added", ylab="Correlaion")
  
  

  # Plot the best model for each division
  plot(lm (i$PPG~i$GDpg))
  
}
}
sts_corr(following_season_join)


###
# The following function was a failure but aimed to improve
# correlations by cheking logs, standard scores and
# combinations thereof.
# A veritable time sink.
###
sts_cor2 <- function(df){
  #resp = PPG
  print(c("Corr for Gf", cor(log(GFpg), PPG)))
  print(c("Corr for GA", cor(log(GApg), PPG)))
  print(c("Corr for GD", cor(log(GDpg), PPG)))
  print(c("Corr for Points", cor(PPG, PPG)))
  print(c("Corr for GD Ratio", cor(log(GFpg)/log(GApg),PPG)))
  print(c("Corr for Pythag", cor(log(GFpg)^2/(log(GFpg)^2 + log(GApg)^2), PPG)))
  print(c("??????", cor(log(GFpg)/(log(GFpg)+log(GApg)), PPG)))
}
sts_cor2(following_season_join)


###
# Repeating what was done above for Season-to-Season correlations
###
data_subset = second[, c("home", "Season", "Ppg")]
names(data_subset) = c("home", "Season", "PPG")
first_join <- merge (first, data_substet, by = c("home", "Season"))

head(first_join)

sts_corr(first_join)

glm1 <- glm(PPG  ~ GFpg +  GApg + GDpg + GDRatio + Ppg)
glm2 <- glm(PPG  ~  GDpg + GDRatio + Ppg)
cor(GApg, PPG)
