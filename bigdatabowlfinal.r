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

#Top 10 gunners by ave dist
top10avedist = gunnerlist %>%
  arrange(distrank) %>%
  select(1,2,3,6) %>%
  rename("PlayerName" = "displayName") %>%
  rename("Average Distance to Returner" = "avedisttoreturner") %>%
  rename("Rank" = "distrank")
  slice(1:10)

#Top 10 gunners by fair catch %
top10fc = gunnerlist %>%
  arrange(faircatchrank) %>%
  select(1,2,4,7) %>%
  rename("PlayerName" = "displayName") %>%
  rename("Fair Catch %" = "faircatchper") %>%
  rename("Rank" = "faircatchrank") %>%
  slice(1:10)

#Top 10 gunners by tackle %
top10tacklers = gunnerlist %>%
  arrange(tacklerank) %>%
  select(1,2,5,8) %>%
  rename("PlayerName" = "displayName") %>%
  rename("Tackle %" = "tackleper") %>%
  rename("Rank" = "tacklerank") %>%
  slice(1:10)

#Top 10 overall
top10 = gunnerlist %>%
  select(1:5,10) %>%
  rename("PlayerName" = "displayName") %>%
  rename("Tackle %" = "tackleper") %>%
  rename("Fair Catch %" = "faircatchper") %>%
  rename("Average Distance to Returner" = "avedisttoreturner") %>%
  slice(1:10)
  