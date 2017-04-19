library(data.table)
library(dplyr)
library(tidyr)
library(reshape2)
# library(lubridate)

CurrentSeason <- 2017

# read and transform data
Teams <- read.csv("Teams.csv", stringsAsFactors = FALSE)
TourneySeeds <- read.csv("TourneySeeds.csv", stringsAsFactors = FALSE) %>%
    filter(Season == CurrentSeason) %>%
    select(-Season) %>%
    merge(Teams, by.x="Team", by.y="Team_Id", all.x=TRUE)
TourneySlots <- read.csv("TourneySlots.csv", stringsAsFactors = FALSE) %>%
    filter(Season == CurrentSeason)

Seasons <- read.csv("Seasons.csv", stringsAsFactors = FALSE) %>%
    mutate(DateZero = as.Date(Dayzero, format="%m/%d/%Y"))
RegularSeasonCompactResults <- read.csv("RegularSeasonCompactResults.csv") %>%
    # filter(Season %in% (CurrentSeason - 3):CurrentSeason) %>%
    merge(Seasons, by="Season", all.x=TRUE) %>%
    select(-Regionw, -Regionx, -Regiony, -Regionz, -Dayzero) %>%
    mutate(Date = DateZero + Daynum) %>%
    merge(Teams, by.x="Wteam", by.y="Team_Id", all.x=TRUE) %>%
    setnames("Team_Name", "WTeamName") %>%
    merge(Teams, by.x="Lteam", by.y="Team_Id", all.x=TRUE) %>%
    setnames("Team_Name", "LTeamName")

EloInput <- cbind(select(RegularSeasonCompactResults, c(Date, WTeamName, LTeamName)), result = 1) %>%
    mutate(Date = as.numeric(Date))


# ELO calculations
library(PlayerRatings)

elo <- elo(EloInput, history=TRUE)
# head(elo$ratings, 20)
plot(elo, players = c("Duke","North Carolina","NC State"), col = c("#0736A4", "#7BAFD4", "#CC0000"))
lnames = c("Duke","North Carolina","NC State")
legend('bottomright', lnames, col=1:3, lty=1)

# round 0 predictions
round0 <- merge(TourneySlots[1:4,], TourneySeeds, by.x="Strongseed", by.y="Seed", all.x=TRUE) %>%
    setnames("Team_Name", "StrongseedName") %>%
    merge(TourneySeeds, by.x="Weakseed", by.y="Seed", all.x=TRUE) %>%
    setnames("Team_Name", "WeakseedName")
round0 <- cbind(round0, probStrong = predict(elo, round0[,c(3,6,8)])) %>%
    mutate(Team = ifelse(probStrong>.5, Team.x, Team.y)) %>%
    mutate(Seed = Slot) %>%
    mutate(Team_Name = ifelse(probStrong>.5, StrongseedName, WeakseedName))
round0Winners <- round0[c("Team","Seed","Team_Name")]

# round 1
Seeds <- rbind(TourneySeeds, round0Winners)
round1 <- subset(TourneySlots, startsWith(Slot, "R1")) %>%
    merge(Seeds, by.x="Strongseed", by.y="Seed", all.x=TRUE) %>%
    setnames("Team_Name", "StrongseedName") %>%
    merge(Seeds, by.x="Weakseed", by.y="Seed", all.x=TRUE) %>%
    setnames("Team_Name", "WeakseedName")
round1 <- cbind(round1, probStrong = predict(elo, round1[,c(3,6,8)])) %>%
    mutate(Team = ifelse(probStrong>.5, Team.x, Team.y)) %>%
    mutate(Seed = Slot) %>%
    mutate(Team_Name = ifelse(probStrong>.5, StrongseedName, WeakseedName))
round1Winners <- round1[c("Team","Seed","Team_Name")]

round2 <- subset(TourneySlots, startsWith(Slot, "R2")) %>%
    merge(round1Winners, by.x="Strongseed", by.y="Seed", all.x=TRUE) %>%
    setnames("Team_Name", "StrongseedName") %>%
    merge(round1Winners, by.x="Weakseed", by.y="Seed", all.x=TRUE) %>%
    setnames("Team_Name", "WeakseedName")
round2 <- cbind(round2, probStrong = predict(elo, round2[,c(3,6,8)])) %>%
    mutate(Team = ifelse(probStrong>.5, Team.x, Team.y)) %>%
    mutate(Seed = Slot) %>%
    mutate(Team_Name = ifelse(probStrong>.5, StrongseedName, WeakseedName))
round2Winners <- round2[c("Team","Seed","Team_Name")]

round3 <- subset(TourneySlots, startsWith(Slot, "R3")) %>%
    merge(round2Winners, by.x="Strongseed", by.y="Seed", all.x=TRUE) %>%
    setnames("Team_Name", "StrongseedName") %>%
    merge(round2Winners, by.x="Weakseed", by.y="Seed", all.x=TRUE) %>%
    setnames("Team_Name", "WeakseedName")
round3 <- cbind(round3, probStrong = predict(elo, round3[,c(3,6,8)])) %>%
    mutate(Team = ifelse(probStrong>.5, Team.x, Team.y)) %>%
    mutate(Seed = Slot) %>%
    mutate(Team_Name = ifelse(probStrong>.5, StrongseedName, WeakseedName))
round3Winners <- round3[c("Team","Seed","Team_Name")]

round4 <- subset(TourneySlots, startsWith(Slot, "R4")) %>%
    merge(round3Winners, by.x="Strongseed", by.y="Seed", all.x=TRUE) %>%
    setnames("Team_Name", "StrongseedName") %>%
    merge(round3Winners, by.x="Weakseed", by.y="Seed", all.x=TRUE) %>%
    setnames("Team_Name", "WeakseedName")
round4 <- cbind(round4, probStrong = predict(elo, round4[,c(3,6,8)])) %>%
    mutate(Team = ifelse(probStrong>.5, Team.x, Team.y)) %>%
    mutate(Seed = Slot) %>%
    mutate(Team_Name = ifelse(probStrong>.5, StrongseedName, WeakseedName))
round4Winners <- round4[c("Team","Seed","Team_Name")]

round5 <- subset(TourneySlots, startsWith(Slot, "R5")) %>%
    merge(round4Winners, by.x="Strongseed", by.y="Seed", all.x=TRUE) %>%
    setnames("Team_Name", "StrongseedName") %>%
    merge(round4Winners, by.x="Weakseed", by.y="Seed", all.x=TRUE) %>%
    setnames("Team_Name", "WeakseedName")
round5 <- cbind(round5, probStrong = predict(elo, round5[,c(3,6,8)])) %>%
    mutate(Team = ifelse(probStrong>.5, Team.x, Team.y)) %>%
    mutate(Seed = Slot) %>%
    mutate(Team_Name = ifelse(probStrong>.5, StrongseedName, WeakseedName))
round5Winners <- round5[c("Team","Seed","Team_Name")]

round6 <- subset(TourneySlots, startsWith(Slot, "R6")) %>%
    merge(round5Winners, by.x="Strongseed", by.y="Seed", all.x=TRUE) %>%
    setnames("Team_Name", "StrongseedName") %>%
    merge(round5Winners, by.x="Weakseed", by.y="Seed", all.x=TRUE) %>%
    setnames("Team_Name", "WeakseedName")
round6 <- cbind(round6, probStrong = predict(elo, round6[,c(3,6,8)])) %>%
    mutate(Team = ifelse(probStrong>.5, Team.x, Team.y)) %>%
    mutate(Seed = Slot) %>%
    mutate(Team_Name = ifelse(probStrong>.5, StrongseedName, WeakseedName))
round6Winners <- round6[c("Team","Seed","Team_Name")]
