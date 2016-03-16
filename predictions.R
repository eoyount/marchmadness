library(data.table)
library(dplyr)
library(tidyr)
library(reshape2)

# read and transform data
TourneySeeds <- read.csv("TourneySeeds.csv") %>% filter(Season == 2016) %>%
        mutate(SeedNum = as.numeric(gsub("[A-Z+a-z]", "", Seed))) %>% select(-Seed)
SampleSubmission <- read.csv("SampleSubmission.csv")
Seasons <- read.csv("Seasons.csv") %>% filter(Season == 2016)
Teams <- read.csv("Teams.csv")
TourneySlots <- read.csv("TourneySlots.csv") %>% filter(Season == 2016)
MasseyOrdinals <- read.csv("MasseyOrdinals2016ThruDay133_54systems.csv") %>% filter(rating_day_num == 133) %>%
        select(-season, -rating_day_num) %>%
        spread(sys_name, orank)

# prep data
games.to.predict <- cbind(SampleSubmission$Id, colsplit(SampleSubmission$Id, "_", c('season', 'team1', 'team2')))

# add tourney seeds
temp <- left_join(games.to.predict, TourneySeeds, by=c("season"="Season", "team1"="Team"))
games.to.predict <- left_join(temp, TourneySeeds, by=c("season"="Season", "team2"="Team"))
colnames(games.to.predict)[c(1,5:6)] <- c("Id", "team1seed", "team2seed")

# add team names
temp <- left_join(games.to.predict, Teams, by=c("team1"="Team_Id"))
games.to.predict <- left_join(temp, Teams, by=c("team2"="Team_Id"))
colnames(games.to.predict)[c(7:8)] <- c("team1name", "team2name")


