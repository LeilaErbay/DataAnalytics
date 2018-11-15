#STATTLESHIP
#5-7 graphs
library(stattleshipR)
library(pracma)
library(DataCombine)
# library(EnvStats)
library(DescTools)
library(graphics)
library(modeest)
library(ggplot2)
library(zoo)
library(dplyr)
library(BSDA)
library(directlabels)
# https://bleacherreport.com/articles/113144-cracking-the-code-how-to-calculate-hollingers-per-without-all-the-mess


setwd("/Users/LeilaErbay/Desktop/LevelNeu2018/XCase")
set_token("1da5666961aa1cf1262cc2f5e6cdc4a7")


league <- "nba"
sport <- "basketball"

### Q: 3 players are needed to give teams a higher chance of getting to the NBA finals?
### Q2: can coaches seek out college players with the attributes of all stars?

#### IMPORTING DATA ############################# 16-17 Regular Season
## ALL PLAYERS -- BASIC INFO
players <- ss_get_result(sport = sport, league = league, ep = "players",
                         query = list(), version = 1, walk = TRUE)
players_df <- unique(do.call("rbind", lapply(players, function(x) x$players)))


# ##OVER ARCHING INFO ON SEASONS INFO
# seasons <- ss_get_result(sport=sport, league=league, ep="seasons", query=list(per_page=40),version=1, walk=T)
# seasons_df <- do.call("rbind", lapply(seasons, function(x) x$seasons))


## GAME LOGS OF 16-17 season <--- rerun
game_logs <- ss_get_result(sport=sport, league=league, ep="game_logs", query=list(per_page=40,interval_type="regularseason", 
                                                                                  season_id="nba-2016-2017"),version=1, walk=T)
game_logs_df <- do.call("rbind", lapply(game_logs, function(x) x$game_logs))



#Games 16-17
games <-ss_get_result(sport=sport, league=league, ep="games", query=list(per_page=40,interval_type="regularseason",
                                                                             season_id="nba-2016-2017"),version=1, walk=T)
games_df <- do.call("rbind", lapply(games, function(x) x$games))
 

# #PLAYER STATS
# playerStats <- ss_get_result(sport=sport, league=league, ep="player_season_stats", query=list(per_page=40,interval_type="regularseason",
#                                                                                 season_id="nba-2016-2017"),version=1, walk=T)
# playerStats_df <- do.call("rbind", lapply(playerStats, function(x) x$player_season_stats))


# #SCOREBOARDS
# scoreboards <-ss_get_result(sport=sport, league=league, ep="scoreboards", query=list(per_page=40,interval_type="regularseason",
#                                                                                              season_id="nba-2016-2017"),version=1, walk=T)
# scoreboards_df <- do.call("rbind", lapply(scoreboards, function(x) x$scoreboards))
# 
# 
# #Team Game Logs
# team_game_logs <- ss_get_result(sport=sport, league=league, ep="team_game_logs", query=list(per_page=40,interval_type="regularseason",
#                                                                                          season_id="nba-2016-2017"),version=1, walk=T)
# team_game_logs_df <- do.call("rbind", lapply(team_game_logs, function(x) x$team_game_logs))


# 
# #Team Season stats
# team_season_stats <- ss_get_result(sport=sport, league=league, ep="team_season_stats", query=list(per_page=40,interval_type="regularseason",
#                                                                                                season_id="nba-2016-2017"),version=1, walk=T)
# team_season_stats_df <- do.call("rbind", lapply(team_season_stats, function(x) x$team_season_stats))

#TEAMS
teams <-  ss_get_result(sport=sport, league=league, ep="teams", query=list(per_page=40,interval_type="regularseason",
                                                                                       season_id="nba-2016-2017"),version=1, walk=T)
teams_df <-  do.call("rbind", lapply(teams, function(x) x$teams))



###### CLEANING #########
aggregated_logs_df <- merge(game_logs_df, players_df,by.y="id",by.x="player_id",all.x=T)
aggregated_logs_df <- subset(aggregated_logs_df, select=-c(created_at.y, updated_at.y,
                                                          active, bats, captain, city, country,draft_overall_pick,
                                                          draft_round,draft_season,draft_team_name, handedness,height,
                                                          high_school,humanized_salary, mlbam_id, nickname ))
aggregated_logs_df <- subset(aggregated_logs_df, select=c(1:57))


updated_df <- merge(aggregated_logs_df, teams_df, by.y = "id", by.x= "team_id.x", all.x= T)
updated_df <- subset(updated_df, select=-c( opponent_id, color, colors, hashtag, hashtags,
                                           location,latitude,longitude, 
                                           team_id.x, player_id, id, created_at.x,
                                           updated_at.x, home_team_score, away_team_score, slug,
                                           home_team_outcome, away_team_outcome, division_id, league_id, created_at, 
                                           updated_at, abbreviation))

missing_df <- updated_df[!complete.cases(updated_df),]
updated_df <- updated_df[complete.cases(updated_df),]
rownames(updated_df) <- NULL
rownames(missing_df) <- NULL

plyrs_team<-unique(updated_df[,c("name.x", "name.y","nickname")])
rownames(plyrs_team) <- NULL


##compare player_name in missing to updated and fill in team name, add back into updated df, order updated by game_id
for(i in 1:nrow(missing_df)){
  for(j in 1:nrow(plyrs_team)){
    if (strcmp(missing_df[i,"name.x"], plyrs_team[j,"name.x"]) & is.na(missing_df[i,"name.y"] )){
      missing_df[i,"name.y"] <- plyrs_team[j,"name.y"]
      missing_df[i,"nickname"] <- plyrs_team[j,"nickname"]
    }
  }
}

updated_df <- rbind(updated_df, missing_df)
updated_df<-updated_df[order(updated_df$game_id),]
names(updated_df)[names(updated_df) == "name.x"] <- "full_name"
names(updated_df) [names(updated_df) == "name.y"] <- "team_name"
names(updated_df) [names(updated_df) == "nickname"] <- "team_nickname"

#addings in date of game
updated_df <- merge(updated_df, games_df[,c("id","on","started_at" )],by.x = "game_id", by.y="id", all.x = T)


####################################### ADD IN WEIGHTS FROM PER ######################################
weights_df <- data.frame(matrix(NA, nrow = 1, ncol = 12))
row.names(weights_df)[1] <- "Weight"
colnames(weights_df) <- c("FGM", "Stl", "Blk","Off_Reb","Ast","Def_Reb", "TO")
weights_df[,"FGM"] <-1.591
weights_df[, "Stl"] <- 0.998
weights_df[,"Blk"] <- .726
weights_df[,"Off_Reb"] <- 0.726
weights_df[, "Ast"] <- 0.642
weights_df[, "Def_Reb"] <- 0.272
weights_df[, "TO"] <- -0.998
weights_df[, "3PTM"] <- 0.958
weights_df[, "FTM"] <- 0.868
weights_df[, "Foul"] <- -0.318
weights_df[, "FT_Miss"] <- -0.372
weights_df[, "FG_Miss"] <- -0.726




######################################### ADDING IN QUALIFYING SCORE: ######################################
updated_df$qualifying_score <- weights_df[,"FGM"]*updated_df[,"field_goals_made"] +
                               weights_df[,"Stl"]*updated_df[, "steals"] +
                               weights_df[, "Blk"]*updated_df[, "blocks"]+
                               weights_df[, "Off_Reb"]*updated_df[,"rebounds_offensive"] + 
                               weights_df[,"Ast"]*updated_df[,"assists"] +
                               weights_df[,"Def_Reb"]*updated_df[, "rebounds_defensive"] +
                               weights_df[,"TO"] *updated_df[,"turnovers"] +
                               weights_df[, "3PTM"]* updated_df[,"three_pointers_made"] +
                               weights_df[, "FTM"]*updated_df[,"free_throws_made"]+
                               weights_df[,"Foul"]*(updated_df[,"personal_fouls"]+updated_df[,"technical_fouls"])+
                               weights_df[,"FT_Miss"]*(updated_df[,"free_throws_attempted"]-updated_df[,"free_throws_made"]) +
                               weights_df[,"FG_Miss"]*(updated_df[,"field_goals_attempted"]-updated_df[,"field_goals_made"])*(60/updated_df[,"time_played_total"]) 

#Players who played on court > 0 seconds : 10160 accounts of players not playing

######################################### RELEVANT PLAYERS = IMPORTANT DF ######################################
relevant_players <- updated_df[updated_df$time_played_total!=0,]
relevant_players$date <- substr(relevant_players$started_at, 1, 10)

############# GGPLOT OF DATA BEFORE NORMALIZE #######################
p_b4_norm<- ggplot(data = relevant_players)+
            geom_histogram(aes(x=qualifying_score),color = "black", fill="red3", bins=40 )+
            xlab("Qualifying Score of Each Player per Game")+
            ylab("Frequency")+
            labs(title="Histogram of Game Log Data Before Normalizing")+
            theme(plot.title = element_text(hjust = 1))+
            theme_bw()

print(p_b4_norm)



#################### NORMALIZE <--- removes .056 % of data ########################
relevant_players$sqrt_qs <- sqrt(relevant_players$qualifying_score)
relevant_players <- relevant_players[complete.cases(relevant_players$sqrt_qs),]


p_aft_norm<- ggplot(data = relevant_players)+
  geom_histogram(aes(x=sqrt_qs),color = "black", fill="red3", bins=40 )+
  xlab("Square Root Qualifying Score of Each Player per Game")+
  ylab("Frequency")+
  labs(title="Histogram of Game Log Data After Normalizing")+
  theme(plot.title = element_text(hjust = 1))+
  theme_bw()

print(p_aft_norm)

####################################### WIN_LOSS PROPORTION BY TEAM ######################################
Win_loss <- aggregate(sqrt_qs~date+team_name+team_outcome, data=relevant_players, FUN =sum)
total_games_per_team <- aggregate(date~team_name, data=Win_loss, FUN= length)
colnames(total_games_per_team) <- c("team_name","num_games")
total_games_per_team$win_prop <- NA
for(i in 1:nrow(total_games_per_team)){
  total_games_per_team[i, "win_prop"] <- nrow(Win_loss[(Win_loss$team_name==total_games_per_team[i,"team_name"] & Win_loss$team_outcome=="win"),])/total_games_per_team[i,"num_games"]
}


######################################### QS by PLAYER NAME ######################################
QS_by_player <- aggregate(sqrt_qs~full_name,data = relevant_players, FUN = sum )
QS_by_player_team <- aggregate(sqrt_qs~full_name+team_name,data = relevant_players, FUN = sum )
QS_by_player_more <- aggregate(sqrt_qs~full_name+team_name+date,data = relevant_players, FUN = sum )
QS_by_player_more$date <- as.Date(QS_by_player_more$date,"%Y-%m-%d")


mean_QS <- mean(QS_by_player$sqrt_qs)

######################################### GGPLOT OF QS vs FREQ for EACH PLAYER ######################################
steph <- QS_by_player[QS_by_player$full_name=="Stephen Curry", "sqrt_qs"]
james <- QS_by_player[QS_by_player$full_name=="LeBron James", "sqrt_qs"]
meanQS <- mean(QS_by_player$sqrt_qs)

p <- ggplot(data=QS_by_player, aes(x=sqrt_qs))+
     geom_freqpoly(size=1, bins = 25) +
     scale_x_continuous(name="Square Root Qualifying Score by Player", breaks=seq(0,500, 50)) +
     scale_y_continuous(name= "Frequency", breaks=seq(0,60,5)) +
    geom_vline(data=data.frame(type="Mean", col="Mean"),
               aes(linetype=type, colour="Mean", xintercept=meanQS), size = 1)+
       geom_vline(data=data.frame(type="Steph Curry, LeBron James", col="Steph Curry"),
                  aes(linetype="Steph Curry, LeBron James", colour="Steph Curry", xintercept=steph), size=1)+
        geom_vline(data=data.frame(type="LeBron James", col="LeBron James"),
                   aes(linetype='Steph Curry, LeBron James', colour="LeBron James", xintercept=james), size=1 )+
       labs(title="Frequency Plot of Square Root Qualifying Score")+
    theme(plot.title = element_text(hjust = 0.5))+ 
     theme_bw()

print(p)


######################################### QS by MONTH_Date_Year ######################################
QS_by_month_date_year <- aggregate(sqrt_qs~started_at, data=relevant_players, FUN =sum)
QS_by_month_date_year$date <- substr(QS_by_month_date_year$started_at, 1, 10)
QS_by_month_date_year$date<- as.Date(QS_by_month_date_year$date,"%Y-%m-%d")


######################################## QS by MONTH_YEAR  ######################################
QS_by_month_date <- aggregate(sqrt_qs~date, data=QS_by_month_date_year, FUN =sum)
QS_by_month_date$month <- substr(QS_by_month_date$date, 6, 7)
QS_by_month <- aggregate(sqrt_qs~month, data=QS_by_month_date, FUN=sum)


######################################## QS by TEAM and DATE  ######################################
QS_by_team_month <- aggregate(sqrt_qs~date+team_name, data= relevant_players, FUN=sum)
QS_by_team_month$date<- as.Date(QS_by_team_month$date,"%Y-%m-%d")
QS_by_team_month$month <- substr(QS_by_team_month$date, 1, 7)

QS_by_team <- aggregate(sqrt_qs~month+team_name, data= QS_by_team_month, FUN=sum)
QS_by_team$month <- as.yearmon(QS_by_team$month,"%Y-%m")

QS_by_team$id <- NA
for(i in 1:nrow(QS_by_team)){
  if(QS_by_team[i,"team_name"] != "Golden State" &QS_by_team[i,"team_name"] != "Cleveland" ) {
    QS_by_team[i,"id"] <- "Rest of League"
  }
  else if (QS_by_team[i,"team_name"] == "Golden State"){
    QS_by_team[i,"id"] <- "Golden State"
  } 
  else  QS_by_team[i,"id"] <- "Cleveland Cavaliers"
}


###################################### PLOT OF League vs GS and CAVS OVER TIME and QS (all teams) ########################
p2 <- ggplot(data=QS_by_team) + 
      geom_smooth(aes(x=month, y=sqrt_qs, color=id), se=F)+
    
      scale_x_yearmon(format="%b-%Y",n=20) +
  scale_color_manual(values=c('red','blue','green3'))+
      labs(title ="Time Series of Team Qualifying Score") +
      xlab("Time")+
      ylab("Qualifying Score of Team")+
  
      theme_bw()

print(p2)

########################################## PERCENTILES OF TEAM and QS ##########################
pct <- data.frame(quantile(QS_by_player$sqrt_qs, c( .1, .5,.75, .9,.95,.975, .99)) )

player_pct <- list()
#ALL STARS
player_pct[[1]] <- QS_by_player[ QS_by_player$sqrt_qs >= pct["97.5%",],]
player_pct[[1]]["pct"] <- "97.5%-100%"
#NEAR ALL STARS
player_pct[[2]] <- QS_by_player[ QS_by_player$sqrt_qs >= pct["95%",] &QS_by_player$sqrt_qs < pct["97.5%",],]
player_pct[[2]]["pct"] <- "95%-97.5%"
#NECESSARY PLAYERS
player_pct[[3]] <- QS_by_player[ QS_by_player$sqrt_qs >= pct["90%",] &QS_by_player$sqrt_qs < pct["95%",],]
player_pct[[3]]["pct"] <- "90%-95%"
player_pct[[4]]<- QS_by_player[ QS_by_player$sqrt_qs >= pct["99%",],]
player_pct[[4]]["pct"] <- "99%"
player_pct[[5]] <- QS_by_player[ QS_by_player$sqrt_qs >= pct["75%",]&QS_by_player$sqrt_qs < pct["90%",],]
player_pct[[5]]["pct"] <- "75%-90%"
player_pct[[6]] <- QS_by_player[ QS_by_player$sqrt_qs >= pct["50%",]&QS_by_player$sqrt_qs < pct["75%",],]
player_pct[[6]]["pct"] <- "50% - 75%"
player_pct[[7]] <- QS_by_player[ QS_by_player$sqrt_qs >= pct["50%",],]
player_pct[[7]]["pct"] <- "50%+"

############################################ GRAPHS OF TOP PLAYERS' QS OVER TIME #########################
df <- data.frame(player_pct[[1]])
QS_top_plyrs <- subset(QS_by_player_more, full_name %in% df$full_name)
QS_top_plyrs <- QS_top_plyrs[complete.cases(QS_top_plyrs),]
QS_top_plyrs$date <- as.Date(QS_top_plyrs$date,"%Y-%m-%d")
QS_top_plyrs$date <- substr(QS_top_plyrs$date, 1, 7)
QS_top_plyrs$date <- as.yearmon(QS_top_plyrs$date,"%Y-%m")
QS_top_plyrs <- aggregate(sqrt_qs~full_name+date+team_name, data = QS_top_plyrs, FUN = sum)

for(i in 1:nrow(QS_top_plyrs)){
  if(QS_top_plyrs[i,"team_name"] != "Golden State" &QS_top_plyrs[i,"team_name"] != "Cleveland" ) {
    QS_top_plyrs[i,"id"] <- "Top Players"
  }
  else if (QS_top_plyrs[i,"team_name"] == "Golden State"){
    QS_top_plyrs[i,"id"] <- "Stephen Curry"
  } 
  else QS_top_plyrs[i,"id"] <- "LeBron James"
}


p3 <- ggplot(data=QS_top_plyrs) + 
  geom_smooth(aes(x=date, y=sqrt_qs, color=id), se=F) +
  scale_x_yearmon(format="%b-%Y", n=13) + 
  scale_color_manual(values=c('red','blue','green4'))+
  labs(title ="Time Series of Top Players Qualifying Score") +
  xlab("Month-Year")+
  ylab("Qualifying Score of Top Players")+
  theme_bw() 

print(p3)

################################## CORRELATION bw TEAM QS and TEAM WINNING PROP########################
QS_teams <- aggregate(sqrt_qs~team_name, data=relevant_players, FUN=sum)
QS_teams <- cbind(QS_teams, total_games_per_team$win_prop)
colnames(QS_teams)[3]<-"win_prop"

fit<- lm(win_prop~sqrt_qs, data=QS_teams)

gs_info <- QS_teams[QS_teams$team_name=="Golden State",]
cav_info <- QS_teams[QS_teams$team_name=="Cleveland",]
p4 <- ggplot(data=QS_teams) +
    
      geom_smooth(aes(x=sqrt_qs, y= win_prop),color='green3', method=lm)+
      geom_point(aes(x=sqrt_qs, y=win_prop))+
      geom_point( aes(x=QS_teams[QS_teams$team_name=="Golden State","sqrt_qs"],
                      y=QS_teams[QS_teams$team_name=="Golden State","win_prop"], col='Golden State'), color = 'blue', size=2)+
      geom_point(aes(x=QS_teams[QS_teams$team_name=="Cleveland","sqrt_qs"],
                     y=QS_teams[QS_teams$team_name=="Cleveland","win_prop"], col='Cleveland Cavaliers'), color='red', size=2)+

  
 
      labs(title="Correlation between Team Quality Score and Team Win Proportion")+
      xlab("Quality Score") +
      ylab( "Win Proportion") +
      theme_bw()

  
print(p4)


summary_fit <- summary(fit)
r_sq<- summary_fit$r.squared
corr_co <- sqrt(r_sq) # 0.497499 no relationship

corr_co
r_sq
################################### INDIVIDUALS : STEPH, LEBRON ###################
QS_by_player_more$month <- NA
QS_by_player_more$month<- substr(QS_by_player_more$date, 1, 7)
QS_by_player_more$month <- as.yearmon(QS_by_player_more$month,"%Y-%m")

league_wo_Steph <- QS_by_player_more[QS_by_player_more$full_name != "Stephen Curry",]
league_wo_Lebron <- QS_by_player_more[QS_by_player_more$full_name != "Lebron James",]

steph_C <- QS_by_player_more[QS_by_player_more$full_name == "Stephen Curry",]
lebron_J <- QS_by_player_more[QS_by_player_more$full_name == "LeBron James",]



############################################# STEPH T TEST #############################
steph_league_t <- t.test(steph_C$sqrt_qs, alternative= "greater",
       mu= mean(QS_by_player_more$sqrt_qs),
       var.equal=var.test(steph_C$sqrt_qs,
                         QS_by_player_more$sqrt_qs, alternative="two.sided"))

p_val_steph <- steph_league_t[["p.value"]] #### 2.592e-40 <0.05
###STEPH IS SIGNIFCANTLY GREATER THAN LEAGUE 

################################## PLOT STEPH VS LEAGUE T PLOT t test and time series ###################
steph_v_league <- QS_by_player_more
steph_v_league$id <- NA
for(i in 1:nrow(steph_v_league)){
  if(steph_v_league[i,"full_name"] == "Stephen Curry") {
    steph_v_league[i,"id"] <- "Stephen Curry"
  }
  else steph_v_league[i,"id"] <- "League"
}

meansS<- aggregate(sqrt_qs~id,data= steph_v_league, FUN=mean)


      ##BOXPLOT --- T TEST
pSvLg <- ggplot(data= steph_v_league, aes(x=id, y = sqrt_qs))+
        geom_boxplot(aes( color = id)) +
        scale_color_manual(values=c('green4','blue'))+
        stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3) +
        geom_text(data = meansS, aes(label = round(sqrt_qs, 3), y = sqrt_qs +.6))+
        xlab("Identifier")+
        ylab("Qualifying Score")+
        labs(title= "T - Test Display Between Steph Curry and the League ")
        
        
print(pSvLg)

      ##LINE --- TIME SERIES
pSvLg_time <- ggplot(data= steph_v_league)+
  geom_line(aes(x=date, y = sqrt_qs, color = id))+
  scale_color_manual(values=c('green4','blue'))+
  xlab("Time")+
  ylab("Qualifying Score")+
  labs(title= "Stephen Curry QS vs League QS over Season")

print(pSvLg_time)

############################################# LEBRON T TEST #############################
lebron_league_t <- t.test(lebron_J$sqrt_qs, alternative= "greater",
                         mu= mean(QS_by_player_more$sqrt_qs),
                         var.equal=var.test(lebron_J$sqrt_qs,
                                            QS_by_player_more$sqrt_qs, alternative="two.sided"))

p_val_lebron <- lebron_league_t[["p.value"]] #### 85.418.962e-42 < 0.05
### LEBRON IS SIG GREATER THAN LEAGUE
######################################### PLOT: LEBRON vs LEAGUE t test and time series ###############################

lebron_v_league <- QS_by_player_more
lebron_v_league$id <- NA
for(i in 1:nrow(lebron_v_league)){
  if(lebron_v_league[i,"full_name"] == "LeBron James") {
    lebron_v_league[i,"id"] <- "LeBron James"
  }
  else lebron_v_league[i,"id"] <- "League"
}

meansL <- aggregate(sqrt_qs~id, lebron_v_league, FUN=mean)

        ##BOXPLOT --- T TEST
pLvLg <- ggplot(data= lebron_v_league, aes(x=id, y = sqrt_qs))+
  geom_boxplot(aes( color = id))+
  scale_color_manual(values=c('green4','red'))+
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3) +
  geom_text(data = meansL, aes(label = round(sqrt_qs, 3), y = sqrt_qs +.6))+
  xlab("Identifier")+
  ylab("Qualifying Score")+
  labs(title= "T - Test Display Between LeBron James and the League")

print(pLvLg)


  #### LINE --- TIME
pLvLg_time <- ggplot(data= lebron_v_league)+
  geom_line(aes(x=date, y = sqrt_qs, color = id))+
  scale_color_manual(values=c('green4','red'))+
  xlab("Time")+
  ylab("Qualifying Score")+
  labs(title= "LeBron James QS and League QS over Season")

print(pLvLg_time)

###################################### LBERON VS STEPH ##############################################
var_t_L_S <- var.test(steph_C$sqrt_qs,lebron_J$sqrt_qs, alternative="two.sided")

lebron_v_steph_t <- t.test(lebron_J$sqrt_qs,steph_C$sqrt_qs, alternative= "two.sided",
                           mu= (mean(lebron_J$sqrt_qs) -mean(steph_C$sqrt_qs)) ,
                           var.equal=ifelse(var_t_L_S$p.value*2 < 0.05, F, T))

lebron_v_steph_p <- lebron_v_steph_t[["p.value"]]*2 
#### 2 !< .05 not reject Ho thus NOT SIG DIFFERENT


#################### PLOT: LEBRON VS STEPH: T and Time #######################################
lebron_v_steph_df <- rbind(steph_C, lebron_J)
means<- aggregate(sqrt_qs~full_name, data=lebron_v_steph_df, FUN=mean)

pLS <- ggplot(data= lebron_v_steph_df, aes(x = full_name, y = sqrt_qs))+
      geom_boxplot(aes(color=full_name))+
  scale_color_manual(values=c('red','blue'))+
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3) +
  geom_text(data = means, aes(label = round(sqrt_qs, 5), y = sqrt_qs -.2))+
  xlab("Identifier")+
  ylab("Qualifying Score")+
  labs(title= "T- Test Display: LeBron James  vs Stephen Curry")

print(pLS)

p_LS_time <- ggplot(data= lebron_v_steph_df)+
  geom_line(aes(x=date, y = sqrt_qs, color = team_name))+
  scale_color_manual(values=c('red','blue'))+
  xlab("Time")+
  ylab("Qualifying Score")+
  labs(title= " LeBron James QS vs Stephen Curry QS over Season")+
  theme_bw()
print(p_LS_time)

#######################################################  CAVS VS GS #######################################
gs_df <- QS_by_player_more[QS_by_player_more$team_name=="Golden State",]
cavs_df<- QS_by_player_more[QS_by_player_more$team_name=="Cleveland",]

gs_cavs_z <- z.test(gs_df$sqrt_qs, cavs_df$sqrt_qs, alternative = "greater",
                    mu= mean(gs_df$sqrt_qs)-mean(cavs_df$sqrt_qs), sigma.x=sd(gs_df$sqrt_qs),
                    sigma.y=sd(gs_df$sqrt_qs))

gs_cavs_z$p.value
### P value = .5 !< 0.05 do not reject null hypothesis <--- not necessarily Greater
#GS > CAVS
## less? p value = .5 !< 0.05
## = ? yes: p val =2

#################### PLOT GS vs CAVS : T test and  TIME #######################################
gs_cavs_df <- rbind(gs_df, cavs_df)
means1 <- aggregate(sqrt_qs~team_name, data= gs_cavs_df, FUN=mean)

        #T test
p_gs_cavs <- ggplot(data=gs_cavs_df, aes(x=team_name, y = sqrt_qs)) +
            geom_boxplot(aes( color=team_name))+
            scale_color_manual(values=c('red','blue'))+
            stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3) +
            geom_text(data = means1, aes(label = round(sqrt_qs, 3), y = sqrt_qs -.5))+
            xlab("Identifier")+
            ylab("Qualifying Score")+
            labs(title= "Z-test: Golden State  vs Cleveland Cavaliers")

print(p_gs_cavs)

        ## TIME
p_gs_cavs_time <- ggplot(data= gs_cavs_df)+
  geom_smooth(aes(x=date, y = sqrt_qs, color = team_name),se=F)+
  scale_color_manual(values=c('red','blue'))+
  xlab("Time")+
  ylab("Qualifying Score")+
  labs(title= "Golden State QS vs Cleveland Cavaliers QS over Season")
print(p_gs_cavs_time)

############################### Top 3 of GS vs LEBRON: 3 v 1 ###########################################
gs_top3 <- QS_by_player_more[QS_by_player_more$full_name =="Stephen Curry" |QS_by_player_more$full_name =="Kevin Durant" | QS_by_player_more$full_name =="Klay Thompson", ]
rownames(gs_top3)<- NULL

var_test_leb_top3 <- var.test(gs_top3$sqrt_qs,lebron_J$sqrt_qs, alternative="two.sided")


lebron_v_top3_z <-  z.test(gs_top3$sqrt_qs, lebron_J$sqrt_qs, alternative = "greater",
                           mu= abs(mean(gs_top3$sqrt_qs)-mean(lebron_J$sqrt_qs)), sigma.x=sd(gs_top3$sqrt_qs),
                           sigma.y=sd(lebron_J$sqrt_qs))

lebron_v_top3_z$p.value
### GS top 3 <= Lebron -- not nec greater do not reject Ho
## sig less:  3.40658e-12

#################################### PLOT LEBRON VS TOP 3 of GS: 3 vs 1 ##################################
lebron_v_top3_df <- rbind(lebron_J, gs_top3)
means2 <- aggregate(sqrt_qs~team_name, data=lebron_v_top3_df, FUN=mean)

p_l_top3_all <- ggplot(data=lebron_v_top3_df, aes(x=team_name, y = sqrt_qs, color=team_name)) +
  geom_boxplot( aes(x=team_name, y = sqrt_qs, color=team_name))+
  scale_color_manual(values=c('red','blue'))+
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3) +
  geom_text(data = means2, aes(label = round(sqrt_qs, 3), y = sqrt_qs -.2), color='black')+
  xlab("Identifier")+
  ylab("Qualifying Score")+
  labs(title= "Z-test: LeBron James  vs Top 3 of Golden State")

print(p_l_top3_all)


#################################### TOP 2 of GS vs LEBRON: 2 vs 1 ##########################################
gs_agg <- aggregate(sqrt_qs~full_name, data=gs_df, FUN = sum)
top_gs <- subset(gs_agg, full_name %in%  player_pct[[1]]$full_name )
gs_top<- subset(gs_agg, full_name %in%  player_pct[[7]]$full_name ) ## TOP 5%
gs_top <- gs_top[order(gs_top$sqrt_qs, decreasing= T),]

gs_top2 <- QS_by_player_more[QS_by_player_more$full_name == "Stephen Curry" | QS_by_player_more$full_name == "Kevin Durant",]

lebron_v_top2_z <- z.test(gs_top2$sqrt_qs, lebron_J$sqrt_qs, alternative = "greater",
                          mu= abs(mean(gs_top3$sqrt_qs)-mean(lebron_J$sqrt_qs)), sigma.x=sd(gs_top3$sqrt_qs),
                          sigma.y=sd(lebron_J$sqrt_qs))

lebron_v_top2_z$p.value  #### do not reject Ho GS top 2 < Lebron
#GS top 2 <= LEBRON
#sig less: p value  2.780161e-05

##################################### PLOT 2 v 1 #####################################
lebron_v_top2 <- rbind(lebron_J, gs_top2)
means3 <- aggregate(sqrt_qs~team_name,data=lebron_v_top2, FUN=mean)


p_l_top2  <- ggplot(data=lebron_v_top2, aes(x=team_name, y = sqrt_qs)) +
  geom_boxplot(aes( color=team_name))+
  scale_color_manual(values=c('red','blue'))+
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3) +
  geom_text(data = means3, aes(label = round(sqrt_qs, 3), y = sqrt_qs +.2),color='black')+
  xlab("Identifier")+
  ylab("Qualifying Score")+
  labs(title= "Z-test: LeBron James  vs Top 2 of Golden State")
print(p_l_top2)
##################################### TOP 3 of GS vs TOP 2 of CAVS: 3 vs 2 #####################################
cavs_agg <- aggregate(sqrt_qs~full_name, data= cavs_df, FUN=sum)
cav_top <-  subset(cavs_agg, full_name %in%  player_pct[[7]]$full_name)
cav_top <- cav_top3[order(cav_top3$sqrt_qs, decreasing=T),]

cavs_top2 <- QS_by_player_more[QS_by_player_more$full_name== "LeBron James" | QS_by_player_more$full_name == "Kyrie Irving",]

top3gs_top2cav_z <- z.test(gs_top3$sqrt_qs, cavs_top2$sqrt_qs, alternative = "greater",
                          mu= abs(mean(gs_top3$sqrt_qs)-mean(cavs_top2$sqrt_qs)), sigma.x=sd(gs_top3$sqrt_qs),
                          sigma.y=sd(cavs_top2$sqrt_qs))

top3gs_top2cav_z$p.value
#GS TOP 3 <= CAVS TOP 2
## significantly less p value: 5.257373e-06

##################################### PlOT 3 v 2 #####################################
gsTop3_cavsTop2 <- rbind(cavs_top2, gs_top3)
means4 <- aggregate(sqrt_qs~team_name, data=gsTop3_cavsTop2, mean)

p_top3_top2 <- ggplot(data=gsTop3_cavsTop2, aes(x=team_name, y = sqrt_qs)) +
                geom_boxplot(aes(color=team_name)) +
                scale_color_manual(values=c('red','blue'))+
                stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3) +
                geom_text(data = means4, aes(label = round(sqrt_qs, 3), y = sqrt_qs -.3), color='black')+
              xlab("Identifier")+
              ylab("Qualifying Score")+
              labs(title= "Z-test:Top 2 of Cleveland Cavaliesr  vs Top 3 of Golden State")

print(p_top3_top2)




################################################ PLAY OFF TEAMS  ##############################################
playoff_teams <- t(data.frame("Golden State","San Antonio","Houston","L.A. Clippers","Utah","Oklahoma","Memphis","Portland",
                      "Boston", "Cleveland","Toronto","Washington","Milwaukee","Chicago","Atlanta","Indiana"))

