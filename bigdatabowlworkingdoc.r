library(dplyr)
library(ggplot2)
library(tidyr)

#Import Datasets
games = read.csv('games.csv')
players = read.csv('players.csv')
plays = read.csv('plays.csv')
tracking2018 = read.csv('tracking2018.csv')
tracking2019 = read.csv('tracking2019.csv')
tracking2020 = read.csv('tracking2020.csv')
scoutingdata = read.csv('PFFScoutingData.csv')

# Combine tracking data
tracking = rbind(tracking2018, tracking2019)
tracking = rbind(tracking, tracking2020)

#Isolate punt plays
punts = plays %>%
  filter(specialTeamsPlayType == "Punt") %>%
  mutate(gameplay = paste(gameId,playId))

#Add gameplay column
scoutingdata = scoutingdata %>%
  mutate(gameplay = paste(gameId,playId))

# Merge scouting data to punt plays
punts = merge(punts, scoutingdata, by = "gameplay") %>%
  select(1,4,8,9,10:12,14:15,23:26,29:32,38:40,42:45)

#Isolate returns
returns = punts %>%
  filter(specialTeamsResult == "Return")

#Split gunners
returns = separate(data = returns, col = gunners, into = c('gunner1','gunner2','gunner3'), sep = "\\;")

#Create indicator for whether gunner was tackler
returns = returns %>%
  mutate(gunnertackle1 = ifelse(tackler==gunner1,1,0)) %>%
  mutate(gunnertackle2 = ifelse(tackler==gunner2,1,0)) %>%
  mutate(gunnertackle3 = ifelse(tackler==gunner3,1,0))

#Replace NA's with 0's
returns = returns %>%
  mutate(gunnertackle1 = replace_na(gunnertackle1,0)) %>%
  mutate(gunnertackle2 = replace_na(gunnertackle2,0)) %>%
  mutate(gunnertackle3 = replace_na(gunnertackle3,0))

#Create indicator for whether gunner was assist tackler
returns = returns %>%
  mutate(gunneratackle1 = ifelse(assistTackler==gunner1,1,0)) %>%
  mutate(gunneratackle2 = ifelse(assistTackler==gunner2,1,0)) %>%
  mutate(gunneratackle3 = ifelse(assistTackler==gunner3,1,0))

#Replace NA's with 0's
returns = returns %>%
  mutate(gunneratackle1 = replace_na(gunneratackle1,0)) %>%
  mutate(gunneratackle2 = replace_na(gunneratackle2,0)) %>%
  mutate(gunneratackle3 = replace_na(gunneratackle3,0))

#Add values to get single tackler indicator
returns = returns %>%
  mutate(gunnertackle = gunnertackle1 + gunnertackle2 + gunnertackle3)

#Add values to get single assist tackler indicator
returns = returns %>%
  mutate(gunneratackle = gunneratackle1 + gunneratackle2 + gunneratackle3)

#Calculate % of tackles by gunners
gunnertcklper = mean(returns$gunnertackle)

#Calculate % of assist tackles by gunners
gunneratcklper = mean(returns$gunneratackle)

#Calculate % of plays where gunner either made tackle or assisted
gunnertotaltackleper = gunnertcklper + gunneratcklper

###TEST ANALYSIS WITH ONE PLAY###

#Isolate one play to understand tracking
play1989 = returns %>%
  filter(playId == 1989) %>%
  select(1:3,10:11,15,22:24,27:29,35:37,39:41,52:53)

#Isolate tracking for that play
play1989tracking = tracking %>%
  filter(playId == 1989)

#Create identifier for gunners
# Merge game data with team abbr's
play1989tracking = merge(play1989tracking, games, by = "gameId")

# Replace "home" or "away" w/ team abbr and "playerId"
play1989tracking = play1989tracking %>%
  mutate(teamname = ifelse(team == "home",homeTeamAbbr,visitorTeamAbbr)) %>%
  mutate(playerId = paste(teamname, jerseyNumber, sep=" "))

#Merge play data with tracking data
play1989tracking = merge(play1989tracking, play1989, by = "playId")

play1989tracking = play1989tracking %>%
  mutate(returnerind = ifelse(nflId == returnerId,1,0)) %>%
  mutate(gunnerind1 = ifelse(playerId == gunner1,1,0)) %>%
  mutate(gunnerind2 = ifelse(playerId == gunner2,1,0)) %>%
  mutate(gunnerind3 = ifelse(playerId == gunner3,1,0))

#Replace NA's with 0's
play1989tracking = play1989tracking %>%
  mutate(gunnerind1 = replace_na(gunnerind1,0)) %>%
  mutate(gunnerind2 = replace_na(gunnerind2,0)) %>%
  mutate(gunnerind3 = replace_na(gunnerind3,0))

#Create indicator for "player of interest"
play1989tracking = play1989tracking %>%
  mutate(poi = returnerind + gunnerind1 + gunnerind2 + gunnerind3)

#Filter out only returner and gunners
play1989tracking = play1989tracking %>%
  filter(poi == 1)

#Filter out plays of interest
play1989tracking = play1989tracking %>%
  filter(event != "None")

###FULL ANALYSIS###

#Filter only tracking data that has "punt_received" event
puntrecframes = tracking %>%
  filter(event == "punt_received")

#Merge game info
puntrecframes = merge(puntrecframes, games, by = "gameId")

#Replace "home" or "away" w/ team abbr and "playerId"
puntrecframes = puntrecframes %>%
  mutate(teamname = ifelse(team == "home",homeTeamAbbr,visitorTeamAbbr)) %>%
  mutate(playerId = paste(teamname, jerseyNumber, sep=" "))

#Merge play data with tracking data
puntrecframes = merge(puntrecframes, punts, by = c("gameId", "playId"))

#Split gunners and vises
puntrecframes = separate(data = puntrecframes, col = gunners, into = c('gunner1','gunner2','gunner3'), sep = "\\;")
puntrecframes = separate(data = puntrecframes, col = vises, into = c('vise1','vise2','vise3','vise4','vise5'), sep = "\\;")

#Create indicators for returner and tacklers
puntrecframes = puntrecframes %>%
  mutate(returnerind = ifelse(nflId == returnerId,1,0)) %>%
  mutate(tacklerind = ifelse(playerId == tackler,1,0))

#Create indicators for gunners and vises
puntrecframes = puntrecframes %>%
  mutate(gunnerind1 = ifelse(playerId == gunner1,1,0)) %>%
  mutate(gunnerind2 = ifelse(playerId == gunner2,1,0)) %>%
  mutate(gunnerind3 = ifelse(playerId == gunner3,1,0)) %>%
  mutate(viseind1 = ifelse(playerId == vise1,1,0)) %>%
  mutate(viseind2 = ifelse(playerId == vise2,1,0)) %>%
  mutate(viseind3 = ifelse(playerId == vise3,1,0)) %>%
  mutate(viseind4 = ifelse(playerId == vise4,1,0)) %>%
  mutate(viseind5 = ifelse(playerId == vise5,1,0))
  
# Replace NA's with 0's
puntrecframes = puntrecframes %>%
  mutate(gunnerind1 = replace_na(gunnerind1,0)) %>%
  mutate(gunnerind2 = replace_na(gunnerind2,0)) %>%
  mutate(gunnerind3 = replace_na(gunnerind3,0)) %>%
  mutate(viseind1 = replace_na(viseind1,0)) %>%
  mutate(viseind2 = replace_na(viseind2,0)) %>%
  mutate(viseind3 = replace_na(viseind3,0)) %>%
  mutate(viseind4 = replace_na(viseind4,0)) %>%
  mutate(viseind5 = replace_na(viseind5,0))

# Sum indicators
puntrecframes = puntrecframes %>%
  mutate(gunnerind = gunnerind1 + gunnerind2 + gunnerind3) %>%
  mutate(viseind = viseind1 + viseind2 + viseind3 + viseind4 + viseind5)
  
#Clean up columns to only those of interest
puntrecframes = puntrecframes %>%
  select(1:10,12:15,18,25:27,31,34,35,37:39,42:44,46:61,73:75,84:85)

###ANALYSIS OF DISTANCE TO RETURNER###

#Create table of returner x and y for each play at point of football catch
returnerloc = puntrecframes %>%
  filter(returnerind == 1) %>%
  select(1:2,4,5) %>%
  rename("returnerx" = "x") %>%
  rename("returnery" = "y")

#Merge table with all player locations at point of football catch
puntrecframes = merge(puntrecframes, returnerloc, by = c("gameId", "playId"))

#Add distance to punt returner
puntrecframes = puntrecframes %>%
  mutate(disttoreturner = sqrt((returnery - y)^2 + (returnerx - x)^2)) %>%
  select(1:36,41:51) %>%

#Filter out the football
puntrecframes = puntrecframes %>%  
  filter(displayName != "football")

#Filter only defenders
puntdefenders = puntrecframes %>%
  filter(teamname == possessionTeam) %>%
  mutate(gameplay = paste(gameId, playId))

#Aggregate minimum distance of defender at time of punt catch
mindist = aggregate(disttoreturner ~ gameplay, data = puntdefenders, min) %>%
  rename("closestdefender" = "disttoreturner")

#Aggregate average distance of defender at time of punt catch
meandist = aggregate(disttoreturner ~ gameplay, data = puntdefenders, mean) %>%
  rename("meandefdist" = "disttoreturner")

#Get coordinates for 2 closest defenders one each play for triangle area
closestdef = puntdefenders %>%
  group_by(gameplay) %>%
  filter(disttoreturner == min(disttoreturner))

secclosestdef = puntdefenders %>%
  group_by(gameplay) %>%
  filter(disttoreturner != min(disttoreturner)) %>%
  arrange(gameplay, disttoreturner) %>%
  slice(1)

#Combine xy's of 2 closest defenders and returner into one df
areaofcontrol = merge(closestdef, secclosestdef, by = "gameplay") %>%
  select(1,5:15,18,48,52:60,62,65,69,76:79,81:83,93:95)

#Calculate area of triangle between 3 and total distance to closest 2
areaofcontrol = areaofcontrol %>%
  mutate(triarea = abs((x.x * (y.y - returnery.y) + x.y * (returnery.y - y.x) + returnerx.y * (y.x - y.y))/2)) %>%
  mutate(disttoclosest2 = disttoreturner.x + disttoreturner.y) %>%
  select(1,37,38)

#Merge calculated stats into one table
returns = returns %>%
  mutate(gameplay = paste(gameId, playId)) %>%
  select(1:3,7,10:11,13:14,22:29,54)

returns = merge(returns, mindist, by = "gameplay")
returns = merge(returns, meandist, by = "gameplay")
returns = merge(returns, areaofcontrol, by = "gameplay")

###Multiple regression model building###

# Individual scatterplots against Y (Return Yardage)
attach(returns)

plot(closestdefender, kickReturnYardage, pch = 16, xlab = "Distance to Closest Defender", ylab = "Kick Return Yardage")
abline(lm(kickReturnYardage ~ closestdefender), lty=2, col="red")

plot(meandefdist, kickReturnYardage, pch = 16, xlab = "Average Defender Distance", ylab = "Kick Return Yardage")
abline(lm(kickReturnYardage ~ meandefdist), lty=2, col="red")

plot(triarea, kickReturnYardage, pch = 16, xlab = "Open Field Area", ylab = "Kick Return Yardage")
abline(lm(kickReturnYardage ~ triarea), lty=2, col="red")

plot(disttoclosest2, kickReturnYardage, pch = 16, xlab = "Total Distance to 2 closest defenders", ylab = "Kick Return Yardage")
abline(lm(kickReturnYardage ~ disttoclosest2), lty=2, col="red")

#Model Building
# Fit the model - 4 predictors
linefit <- lm(kickReturnYardage ~ closestdefender + meandefdist + triarea + disttoclosest2 + kickLength + operationTime + hangTime)
# Summary statistics
summary(linefit)
coefficients(linefit)
# ANOVA - Partitioning of the variance
anova(linefit)

confint(linefit, level = .95)

# Model Statistics
# Coefficients: Beta-hats
coefficients(linefit)
# Coefficient of determination: R-squared
summary(linefit)$r.squared
# Standard error of the residuals = sqrt(MSE) = (s sub epsilon-hat)
summary(linefit)$sigma

#####Model not that helpful####

####Starting analysis of all punts for fair-catch percentages####
tracking = tracking %>%
  mutate(gameplay = paste(gameId, playId))

puntplays = punts %>%
  select(1,4)

punttracking = merge(tracking,puntplays,by = "gameplay")
punttracking = punttracking %>%
  filter(specialTeamsPlayType == "Punt")

events = unique(punttracking$event)

events

# Get all frames where the ball is caught, fair caught, or landed
puntframes = punttracking %>%
  filter(event %in% c("punt_received","fair_catch","kick_received","punt_land")) %>%
  select(1,3:15,16:18)


#Get game data
teams = games %>%
  select(1,6,7)

# Merge game data with team abbr's
puntframes = merge(puntframes, teams, by = "gameId")

# Replace "home" or "away" w/ team abbr and "playerId"
puntframes = puntframes %>%
  mutate(teamname = ifelse(team == "home",homeTeamAbbr,visitorTeamAbbr)) %>%
  mutate(playerId = paste(teamname, jerseyNumber, sep=" ")) %>%
  select(1:14,16,17,20,21)

#Clean punts to relevant info before merging with frames
punts = punts %>%
  select(1:3,5:7,8:13,15:24)

puntframes = merge(puntframes, punts, by = "gameplay")

#Identify gunners for each play
puntframes = separate(data = puntframes, col = gunners, into = c('gunner1','gunner2','gunner3','gunner4'), sep = "\\;")

#Create indicators for returner and tacklers
puntframes = puntframes %>%
  mutate(returnerind = ifelse(nflId == returnerId,1,0)) %>%
  mutate(tacklerind = ifelse(playerId == tackler,1,0))

#Create indicators for gunners
puntframes = puntframes %>%
  mutate(gunnerind1 = ifelse(playerId == gunner1,1,0)) %>%
  mutate(gunnerind2 = ifelse(playerId == gunner2,1,0)) %>%
  mutate(gunnerind3 = ifelse(playerId == gunner3,1,0)) %>%
  mutate(gunnerind4 = ifelse(playerId == gunner4,1,0))

#Remove NA's
puntframes = puntframes %>%
  mutate(gunnerind1 = replace_na(gunnerind1,0)) %>%
  mutate(gunnerind2 = replace_na(gunnerind2,0)) %>%
  mutate(gunnerind3 = replace_na(gunnerind3,0)) %>%
  mutate(gunnerind4 = replace_na(gunnerind3,0)) %>%
  mutate(returnerind = replace_na(returnerind,0)) %>%
  mutate(tacklerind = replace_na(tacklerind,0))

#Sum indicator columns
puntframes = puntframes %>%
  mutate(gunnerind = gunnerind1 + gunnerind2 + gunnerind3 + gunnerind4)

#Clean up
puntframes = puntframes %>%
  select(1:44,49)

#Create table of returner x and y for each play at point of football catch
returnerloc = puntframes %>%
  filter(returnerind == 1) %>%
  select(1,3,4) %>%
  rename("returnerx" = "x") %>%
  rename("returnery" = "y")

#Merge table with all player locations at point of football catch
puntframes = merge(puntframes, returnerloc, by = "gameplay")

#Add distance to punt returner
puntframes = puntframes %>%
  mutate(disttoreturner = sqrt((returnery - y)^2 + (returnerx - x)^2))
  
#Filter out the football
puntframes = puntframes %>%  
  filter(displayName != "football")

###Analyze gunners###
# Get all gunner frames
gunnerframes = puntframes %>%
  filter(gunnerind == 1)

#Aggregate average distance to returner at time of reception/faircatch
meangunnerdist = aggregate(disttoreturner ~ displayName, data = gunnerframes, mean) %>%
  rename("avedisttoreturner" = "disttoreturner")

#Number of gunner plays
numgunnerplays = gunnerframes %>%
  count(displayName)

gunnerlist = numgunnerplays %>%
  filter(n >= 10)

gunnerlist = merge(gunnerlist, meangunnerdist, by = "displayName")
  
gunnerlist = gunnerlist %>%
  rename("NumberOfPlays" = "n") %>%
  arrange(avedisttoreturner)

#Create fair catch indicator
gunnerframes = gunnerframes %>%
  mutate(faircatch = ifelse(specialTeamsResult == "Fair Catch",1,0))

#Aggregate fair catch % by player
faircatchper = aggregate(faircatch ~ displayName, data = gunnerframes, mean) %>%
  rename("faircatchper" = "faircatch")

#Add fair catch % to gunner list
gunnerlist = merge(gunnerlist, faircatchper, by = "displayName")

#Aggregate tackle % by player
# Remove fair catches from gunner frames
gunnerreturns = gunnerframes %>%
  filter(specialTeamsResult != "Fair Catch")

tackleper = aggregate(tacklerind ~ displayName, data = gunnerreturns, mean) %>%
  rename("tackleper" = "tacklerind")

#Add tackle % to gunner list
gunnerlist = merge(gunnerlist, tackleper, by = "displayName")

#Add rankings for each metric and aggregate into gunnerlist
gunnerlist = gunnerlist %>%
  mutate(distrank = rank(avedisttoreturner)) %>%
  mutate(faircatchrank = rank(-faircatchper)) %>%
  mutate(tacklerank = rank(-tackleper))

gunnerlist = gunnerlist %>%
  mutate(overallrank = (distrank + faircatchrank + tacklerank)/3) %>%
  mutate(Rank = rank(overallrank)) %>%
  arrange(Rank)

