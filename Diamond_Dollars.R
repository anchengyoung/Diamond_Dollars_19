library(tidyverse)

fields <- read_csv("R_Baseball/master/data/fields.csv")
data2018 <- read_csv("R_Baseball/download.folder/unzipped/all2018.csv",
                     col_names = pull(fields, Header), na = character())
Hitter <- read_csv("Downloads/hitters.csv")
vLHP <- read_csv("Downloads/vLHP.csv")
vRHP <- read_csv("Downloads/vRHP.csv")

IDMAP <- read_csv("Downloads/IDMAP.csv")

IDMAP <- IDMAP %>% select("IDFANGRAPHS","RETROID")
names(IDMAP) <- c("playerid","BAT_ID")

#Correlation between Win% and Leverage Runs
LevRuns <- read_csv("Downloads/3-Yr_LevRuns.csv")

library(ggplot2)
ggplot(LevRuns, aes(x = WinPct)) +
  geom_point(aes(y = HighR, color = "red")) +
  geom_smooth(method = "lm", se = F, aes(y = HighR), color = "red") +
  geom_point(aes(y = MidR, color = "blue")) +
  geom_smooth(method = "lm", se = F, aes(y = MidR), color = "blue") +
  geom_point(aes(y = LowR, color = "green")) +
  geom_smooth(method = "lm", se = F, aes(y = LowR), color = "green") +
  ylab("Total Runs Allowed") + xlab("Winning Percentage") +
  ggtitle("Winning Percentage vs. RA in Different Leverages") +
  scale_color_identity(name = "Leverage", guide = "legend",
                       labels = c("Medium","Low","High"))

  
#Hitter Table------------------------
Hitter <- left_join(Hitter, vRHP, by = "playerid")
Hitter <- left_join(Hitter, vLHP, by = "playerid")

for (n in 1:nrow(Hitter)){
  Hitter$wRC[n]/100 -> Hitter$wRC[n]
  Hitter$wRCR[n]/100 -> Hitter$wRCR[n]
  Hitter$wRCL[n]/100 -> Hitter$wRCL[n]
}

Hitter <- left_join(Hitter, IDMAP, by = "playerid") 

retroID <- read_csv("Downloads/retroID.csv")
for (n in 1:nrow(Hitter)){
  if (Hitter$playerid[n] == 17169){
    Hitter$BAT_ID[n] = "castj007"
  } else if (is.na(Hitter$BAT_ID[n])){
    retroID %>%
      filter(Last == Hitter$Last[n], First == Hitter$First[n]) %>%
      pull(ID) -> Hitter$BAT_ID[n]
  }
}


#All PA data from 2018-------------------------------
data2018 <- data2018 %>%
  filter(BAT_EVENT_FL == TRUE) %>%
  select("GAME_ID","AWAY_TEAM_ID","INN_CT","BAT_HOME_ID","OUTS_CT",
         "AWAY_SCORE_CT","HOME_SCORE_CT","BAT_ID","BAT_HAND_CD",
         "RESP_BAT_ID","RESP_BAT_HAND_CD","BASE1_RUN_ID","BASE2_RUN_ID",
         "BASE3_RUN_ID","LEADOFF_FL","BAT_FLD_CD", "BAT_LINEUP_ID",
         "BAT_EVENT_FL","BATTEDBALL_CD","GAME_NEW_FL","GAME_END_FL","EVENT_ID")

data2018 <- left_join(data2018, Hitter, by = "BAT_ID")

for (n in 1:nrow(data2018)){
  if (data2018$BAT_FLD_CD[n] == 1){
    data2018$wRC[n] = -0.25
    data2018$wRCR[n] = -0.25
    data2018$wRCL[n] = -0.25
  } else if (data2018$PA[n] <= 150){
      if (data2018$BAT_HAND_CD[n] == "R"){
        data2018$wRC[n] = 0.7
        data2018$wRCR[n] = 0.66
        data2018$wRCL[n] = 0.74
      } else if (data2018$BAT_HAND_CD[n] == "L"){
        data2018$wRC[n] = 0.7
        data2018$wRCR[n] = 0.805
        data2018$wRCL[n] = 0.595
      }
  }
}

data2018 <- data2018 %>%
  mutate(vR = wRC * 0.75 + wRCR * 0.25,
         vL = wRC * 0.75 + wRCL * 0.25)

#Separating home and road teams-----------------------------
homePA <- data2018 %>% filter(BAT_HOME_ID == 1)
roadPA <- data2018 %>% filter(BAT_HOME_ID == 0)

homePA$R3 <- NA
homePA$L3 <- NA
roadPA$R3 <- NA
roadPA$L3 <- NA

for (n in 1:nrow(homePA)){
  if (n %in% c(nrow(homePA), nrow(homePA)-1)){
    homePA$R3[n] = (homePA$vR[n] + homePA$vR[n-8] + homePA$vR[n-7]) / 3
    homePA$L3[n] = (homePA$vL[n] + homePA$vL[n-8] + homePA$vL[n-7]) / 3
  } else if (homePA$EVENT_ID[n] > homePA$EVENT_ID[n+2]){
    homePA$R3[n] = (homePA$vR[n] + homePA$vR[n-8] + homePA$vR[n-7]) / 3
    homePA$L3[n] = (homePA$vL[n] + homePA$vL[n-8] + homePA$vL[n-7]) / 3
  } else {
    homePA$R3[n] = (homePA$vR[n] + homePA$vR[n+1] + homePA$vR[n+2]) / 3
    homePA$L3[n] = (homePA$vL[n] + homePA$vL[n+1] + homePA$vL[n+2]) / 3
  }
}

for (n in 1:nrow(roadPA)){
  if (n %in% c(nrow(roadPA), nrow(roadPA)-1)){
    roadPA$R3[n] = (roadPA$vR[n] + roadPA$vR[n-8] + roadPA$vR[n-7]) / 3
    roadPA$L3[n] = (roadPA$vL[n] + roadPA$vL[n-8] + roadPA$vL[n-7]) / 3
  } else if (roadPA$EVENT_ID[n] > roadPA$EVENT_ID[n+2]){
    roadPA$R3[n] = (roadPA$vR[n] + roadPA$vR[n-8] + roadPA$vR[n-7]) / 3
    roadPA$L3[n] = (roadPA$vL[n] + roadPA$vL[n-8] + roadPA$vL[n-7]) / 3
  } else {
    roadPA$R3[n] = (roadPA$vR[n] + roadPA$vR[n+1] + roadPA$vR[n+2]) / 3
    roadPA$L3[n] = (roadPA$vL[n] + roadPA$vL[n+1] + roadPA$vL[n+2]) / 3
  }
}

homePA <- homePA %>% mutate(Hand = round(R3 - L3, 2))
roadPA <- roadPA %>% mutate(Hand = round(R3 - L3, 2))

data2018 <- rbind(homePA, roadPA) %>%
  arrange(EVENT_ID) %>% arrange(GAME_ID)

#Leverage in each PA-----------------------------------
Leverage <- read_csv("Downloads/LeveragePA.csv")

#Fix Date Column
Leverage <- separate(data = Leverage, col = Date,
                     into = c("Date","DH"), sep = " ")

for (n in 1:nrow(Leverage)){
  if (is.na(Leverage$DH[n])){
    Leverage$DH[n] = 0
  } else if (Leverage$DH[n] == "(1)"){
    Leverage$DH[n] = 1
  } else if (Leverage$DH[n] == "(2)"){
    Leverage$DH[n] = 2
  } 
}

Leverage$DH <- as.numeric(Leverage$DH)

a <- as.Date(Leverage$Date, format = "%m/%d/%y")
b <- as.Date(Leverage$Date, format = "%Y-%m-%d")
a[is.na(a)] <- b[!is.na(b)]
Leverage$Date <- a

Leverage <- separate(data = Leverage, col = Date,
                     into = c("Y","M","D"), sep = "-")

#Split Home/Road
library(tibble)
Leverage <- add_column(Leverage, BAT_HOME_ID = NA, .after = "Opp")
Leverage <- add_column(Leverage, HOME = NA, .before = "Y")

for (n in 1:nrow(Leverage)){
  if (nchar(Leverage$Opp[n]) == 4){
    Leverage$BAT_HOME_ID[n] = 0
    Leverage$HOME[n] = substr(Leverage$Opp[n], 2, 4)
  } else {
    Leverage$BAT_HOME_ID[n] = 1
    Leverage$HOME[n] = Leverage$Tm[n]
  }
}

#For some reason BRef had negative LI on 192 occasions
Leverage$LI <- abs(Leverage$LI)

#Check non-matching Team ID
retroTm <- data2018 %>%
  distinct(AWAY_TEAM_ID) %>% arrange(AWAY_TEAM_ID)
BRefTm <- Leverage %>%
  distinct(HOME) %>% arrange(HOME)

for (n in 1:30){
  if (!(BRefTm$HOME[n] %in% retroTm$AWAY_TEAM_ID)){
    print(BRefTm$HOME[n])
  }
}

for (n in 1:nrow(Leverage)){
  if (Leverage$HOME[n] == "CHC"){
    Leverage$HOME[n] = "CHN"}
  if (Leverage$HOME[n] == "CHW"){
    Leverage$HOME[n] = "CHA"}
  if (Leverage$HOME[n] == "KCR"){
    Leverage$HOME[n] = "KCA"}
  if (Leverage$HOME[n] == "LAA"){
    Leverage$HOME[n] = "ANA"}
  if (Leverage$HOME[n] == "LAD"){
    Leverage$HOME[n] = "LAN"}
  if (Leverage$HOME[n] == "NYM"){
    Leverage$HOME[n] = "NYN"}
  if (Leverage$HOME[n] == "NYY"){
    Leverage$HOME[n] = "NYA"}
  if (Leverage$HOME[n] == "SDP"){
    Leverage$HOME[n] = "SDN"}
  if (Leverage$HOME[n] == "SFG"){
    Leverage$HOME[n] = "SFN"}
  if (Leverage$HOME[n] == "STL"){
    Leverage$HOME[n] = "SLN"}
  if (Leverage$HOME[n] == "TBR"){
    Leverage$HOME[n] = "TBA"}
  if (Leverage$HOME[n] == "WSN"){
    Leverage$HOME[n] = "WAS"}
}

Leverage %>% distinct(HOME) %>% arrange(HOME)

#Create GAME_ID
Leverage <- add_column(Leverage, GAME_ID = NA, .before = "HOME")

for (n in 1:nrow(Leverage)){
  paste(Leverage$HOME[n], Leverage$Y[n], Leverage$M[n],
        Leverage$D[n], Leverage$DH[n], sep = "") -> Leverage$GAME_ID[n]
}


#Fix Event_ID in data2018
for (n in 2:nrow(data2018)){
  for (x in 2:15){
    if (data2018$EVENT_ID[n] == data2018$EVENT_ID[n-1]+x){
      data2018$EVENT_ID[n] = data2018$EVENT_ID[n] - (x-1)
    }
  }
}

#Check if EVENT_ID are all continuous
for (n in 2:nrow(data2018)){
  if (data2018$EVENT_ID[n] >= (data2018$EVENT_ID[n-1]+2)){
    print(c(data2018$GAME_ID[n], data2018$EVENT_ID[n]))
  }
}

colnames(Leverage)[colnames(Leverage) == "Yr#"] <- "PA_ID"
colnames(Leverage)[colnames(Leverage) == "Gm#"] <- "EVENT_ID"
for (n in 2:nrow(Leverage)){
  if (Leverage$EVENT_ID[n] >= (Leverage$EVENT_ID[n-1]+2)){
    print(c(Leverage$GAME_ID[n], Leverage$EVENT_ID[n]))
  }
}

#Select data we want from Leverage
LevPA <- Leverage %>%
  select("PA_ID","EVENT_ID","GAME_ID","HOME","LI")


#Combine LevPA with data2018
data2018 <- left_join(data2018, LevPA, by = c("GAME_ID","EVENT_ID"))

colnames(data2018)[colnames(data2018) == "AWAY_TEAM_ID"] <- "AWAY"
data2018 <- data2018 %>%
  select("PA_ID","GAME_ID","EVENT_ID","HOME",everything()) %>%
  arrange(PA_ID)

#Create New Leverage Index
data2018 <- data2018 %>%
  mutate(LI_P = wRC * LI,
         LI_R = vR * LI,
         LI_L = vL * LI,
         LI_R3 = R3 * LI,
         LI_L3 = L3 * LI)

data2018 <- data2018 %>%
  mutate_at(vars(vR, vL, R3, L3, LI_P, LI_R,
                 LI_L, LI_R3, LI_L3), funs(round(., 4)))

Bucket <- data.frame(Leverage = c("Very High","High","Medium",
                                       "Low","Total"))

Bucket$RHP <- 0
Bucket$RPct <- 0
Bucket$Neutral <- 0
Bucket$NPct <- 0
Bucket$LHP <- 0
Bucket$LPct <- 0
Bucket$Total <- 0
Bucket$TPct <- 0

for (n in 1:nrow(data2018)){
  if (data2018$LI_P[n] >= 3){
    if (data2018$Hand[n] < 0){
      Bucket$RHP[1] = Bucket$RHP[1] + 1}
    if (data2018$Hand[n] == 0){
      Bucket$Neutral[1] = Bucket$Neutral[1] + 1}
    if (data2018$Hand[n] > 0){
      Bucket$LHP[1] = Bucket$LHP[1] + 1}
  } else if (data2018$LI_P[n] < 0.8){
    if (data2018$Hand[n] < 0){
      Bucket$RHP[4] = Bucket$RHP[4] + 1}
    if (data2018$Hand[n] == 0){
      Bucket$Neutral[4] = Bucket$Neutral[4] + 1}
    if (data2018$Hand[n] > 0){
      Bucket$LHP[4] = Bucket$LHP[4] + 1}
  } else if (data2018$LI_P[n] >= 0.8 & data2018$LI_P[n] < 1.6){
    if (data2018$Hand[n] < 0){
      Bucket$RHP[3] = Bucket$RHP[3] + 1}
    if (data2018$Hand[n] == 0){
      Bucket$Neutral[3] = Bucket$Neutral[3] + 1}
    if (data2018$Hand[n] > 0){
      Bucket$LHP[3] = Bucket$LHP[3] + 1}
  } else if (data2018$LI_P[n] >= 1.6 & data2018$LI_P[n] < 3){
    if (data2018$Hand[n] < 0){
      Bucket$RHP[2] = Bucket$RHP[2] + 1}
    if (data2018$Hand[n] == 0){
      Bucket$Neutral[2] = Bucket$Neutral[2] + 1}
    if (data2018$Hand[n] > 0){
      Bucket$LHP[2] = Bucket$LHP[2] + 1}
  }
}

Bucket$RHP[5] = sum(Bucket$RHP[1:4])
Bucket$Neutral[5] = sum(Bucket$Neutral[1:4])
Bucket$LHP[5] = sum(Bucket$LHP[1:4])
Bucket$Total[5] = Bucket$RHP[5] + Bucket$Neutral[5] + Bucket$LHP[5]

for (n in 1:5){
  Bucket$Total[n] = Bucket$RHP[n] + Bucket$Neutral[n] + Bucket$LHP[n]
  Bucket$RPct[n] = round(Bucket$RHP[n] / Bucket$Total[5], 4)
  Bucket$NPct[n] = round(Bucket$Neutral[n] / Bucket$Total[5], 4)
  Bucket$LPct[n] = round(Bucket$LHP[n] / Bucket$Total[5], 4)
  Bucket$TPct[n] = round(Bucket$Total[n] / Bucket$Total[5], 4)
}


#Mutate new ID for Team Plate Appearance in Game
data2018 <- add_column(data2018, TeamPA = NA, .after = "BAT_HOME_ID")

homeline <- data2018 %>% filter(BAT_HOME_ID == 1)
roadline <- data2018 %>% filter(BAT_HOME_ID == 0)
homeline$TeamPA[1] <- 1
roadline$TeamPA[1] <- 1

for (n in 2:nrow(homeline)){
  if (homeline$GAME_ID[n] == homeline$GAME_ID[n-1]){
    homeline$TeamPA[n] = homeline$TeamPA[n-1] + 1
  } else {homeline$TeamPA[n] = 1}
}

for (n in 2:nrow(roadline)){
  if (roadline$GAME_ID[n] == roadline$GAME_ID[n-1]){
    roadline$TeamPA[n] = roadline$TeamPA[n-1] + 1
  } else {roadline$TeamPA[n] = 1}
}

data2018 <- rbind(homeline, roadline) %>%
  arrange(PA_ID)

occur <- data2018 %>% group_by(TeamPA) %>%
  summarize(LI_P = mean(LI_P),
            LI_R3 = mean(LI_R3),
            LI_L3 = mean(LI_L3),
            N = n())

#Split graph by League
data2018 <- add_column(data2018, Lg = NA, .before = "HOME")
AL <- c("BAL","BOS","NYA","TBA","TOR","CHA","CLE","DET","KCA","MIN","HOU","ANA","OAK","SEA","TEX")
NL <- c("ATL","MIA","NYN","PHI","WAS","CHN","CIN","MIL","PIT","SLN","ARI","COL","LAN","SDN","SFN")

for (n in 1:nrow(data2018)){
  if (data2018$HOME[n] %in% AL){
    data2018$Lg[n] = "AL"
  } else if (data2018$HOME[n] %in% NL){
    data2018$Lg[n] = "NL"
  }
}

occur_lg <- data2018 %>% group_by(TeamPA, Lg) %>%
  summarize(LI_P = mean(LI_P),
            LI_R3 = mean(LI_R3),
            LI_L3 = mean(LI_L3),
            N = n())

ggplot(occur, aes(TeamPA, LI_P)) + geom_line() +
  xlab("Team Plate Appearance in Game") +
  ylab("LI+") +
  ggtitle("Average LI+ by Team PA in Game") +
  scale_x_continuous(limits = c(0, 40)) +
  scale_y_continuous(limits = c(-0.25, 1.5))

ggplot(occur_lg, aes(TeamPA, LI_P, colour = Lg)) + geom_line() +
  xlab("Team Plate Appearance in Game") + ylab("LI+") +
  ggtitle("Average LI+ by Team PA in Game") +
  scale_x_continuous(limits = c(0, 40)) +
  scale_y_continuous(limits = c(-0.25, 1.5))


#Cubs Pitching Staff Distribution
cubs <- Bucket 
cubs$RHP <- as.numeric(NA)
cubs$Neutral <- as.numeric(NA)
cubs$LHP <- as.numeric(NA)
cubs$Total <- as.numeric(NA)

for (n in 1:5){
  cubs$RHP[n] = as.integer(cubs$RPct[n] * 6264)
  cubs$Neutral[n] = as.integer(cubs$NPct[n] * 6264)
  cubs$LHP[n] = as.integer(cubs$LPct[n] * 6264)
  cubs$Total[n] = as.integer(cubs$TPct[n] * 6264)
}

