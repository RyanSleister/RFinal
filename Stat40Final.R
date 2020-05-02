#Ryan Sleister and Colton Sleister
#R and SAS- STAT 040
#Prof. Aguilar
#May 1, 2020


#data workup for project proposal

nfl_data = read.csv("nfl_elo.csv")
nfl_data = data.frame(nfl_data)

na_count <-sapply(nfl_data, function(y) sum(length(which(is.na(y)))))
na_count

team_name = summary(nfl_data$team1)

summary(nfl_data$score1)


#description of project
#Each team should also submit a finalized .R or .SAS files. Exclude earlier versions of your work.
#. The program files should contain section headers and other comments to help with readability. The comments should make the reader aware of the main tasks associated with each
#piece of code. The project code is going to be divided as follows:
#  - Data Cleansing.
#- Data aggregation if needed.
#- Data summarization.
#- Data visualization.




##Data Cleansing##

#To clean our data, we want to first eliminate the columns detailing quarterback data as this will not be included
#in our analysis. We redeclare the dataset using only the non-QB column names.
nfl_data = nfl_data[,c('date','season','neutral','playoff','team1','team2','elo1_pre',
                       'elo2_pre','elo_prob1','elo_prob2','elo1_post','elo2_post',
                       'score1','score2')]

##Adding New Variables: score difference (margin), winning team(winning_team), pre-game elo difference
nfl_data$score_margin = nfl_data$score1-nfl_data$score2 #positive margin is team1 winning, negative is team2 winning

nfl_data$winning_team = nfl_data$score_margin #recode copy of score_margin to winning team
nfl_data$winning_team = ifelse(nfl_data$winning_team >0, 1, (ifelse(nfl_data$winning_team<0, 2, 0))) #0=tie, 1=team1, 2=team2

nfl_data$elo_difference_pre = nfl_data$elo1_pre-nfl_data$elo2_pre #difference in elo pre-game, positive is greater team1 elo
plot(team_name, xlab='nth most common team', ylab='games played')

#Make a variable tracking which row is which, useful for sorting by date later.
nfl_data$row_num = 1:length(nfl_data$date)

#remove games including teams playing fewer than 50 total games
#this is implemented by making a summary of team1, subsetting only >50, and using row names to subset the nfl_elo dataframe
#We want to do this because teams that only play a few games may not have enough games played
#to properly calibrate their elo (all teams start at 1300 Elo)

team_sizes1 = summary(nfl_data$team1) 
team_sizes1.df = data.frame(team_sizes1)
team_sizes1.df.big = subset(team_sizes1.df, team_sizes1>50)

#repeat the process for games with minor teams as team2.
team_sizes2 = summary(nfl_data$team2)
team_sizes2.df = data.frame(team_sizes2)
team_sizes2.df.big = subset(team_sizes2.df, team_sizes2>50)

nfl_data.big = subset(nfl_data, team1 %in% row.names(team_sizes1.df.big) 
                      & team2 %in% row.names(team_sizes2.df.big), select = colnames(nfl_data))
print('we removed this many games involving teams which had short NFL tenures:')
print(length(nfl_data[,1])-length(nfl_data.big[,1]))



summary(nfl_data.big$team1)
summary(nfl_data$team1) #The summaries show the number of teams playing a small number of games which
                        #now are not included in our dataset.


## Winning Streak Analysis

## For this, we really need to look at one team at a time.
## Subsetting function for a given team's games:
team_subset = function(nfl_frame, team_name){
  sub = data.frame(nfl_frame)
  sub = subset(sub, team1 == team_name | team2 == team_name)
  sub$is.team1 = sub$team1
  sub$is.team1 = ifelse(sub$is.team1 == team_name, 1,0) ##1/0 is boolean for team 1, 1=team1, 0=team2
  
  for(i in 1:length(sub$team1)){ #when team1 is not team_name, we rearrange variables to make team1=team_name, makes future analysis easier
    if(sub$is.team1[i]==0){
      sub$score_margin[i] = -sub$score_margin[i] #reverse margin
      
      swap = sub$team1[i]
      sub$team1[i] = sub$team2[i]
      sub$team2[i] = swap #make sure team_name is team1, use is.team1 to track home vs. away games
      
      swap = sub$score1[i]
      sub$score1[i] = sub$score2[i]
      sub$score2[i] = swap #swap scores 
      
      
      swap = sub$elo1_pre[i]
      sub$elo1_pre[i] = sub$elo2_pre[i]
      sub$elo2_pre[i] = swap #swap elo_pre
      
      swap = sub$elo_prob1[i]
      sub$elo_prob1[i] = sub$elo_prob2[i]
      sub$elo_prob2[i] = swap #swap elo_prob
      
      swap = sub$elo1_post[i]
      sub$elo1_post[i] = sub$elo2_post[i]
      sub$elo2_post[i] = swap #swap elo_post
      
      sub$elo_difference_pre[i] = -sub$elo_difference_pre[i] #swap elo difference
      ifelse(sub$winning_team[i] == 1, 2, ifelse(sub$winning_team[i] == 2, 1, 0)) #swap winning_team values
    }
  }
  return(sub)
}







# Data Summarization
chicago.bears = team_subset(nfl_data, 'CHI')

generate_streaks= function(team_set){
  team_set = team_set[order(team_set$row_num),] #order chronologically
  
  team_set$streak[1] = ifelse(team_set$winning_team[1]==2, -1, team_set$winning_team[1])
  
  for(i in 2:length(team_set$winning_team)){
    if(team_set$winning_team[i] == 1){
      team_set$streak[i] = max(team_set$streak[i-1], 0) + 1
    }
    else if (team_set$winning_team[i] ==2){
      team_set$streak[i] = min(team_set$streak[i-1], 0) -1
    }
    else{team_set$streak[i] = team_set$streak[i-1]}
    team_set$streak_pre = team_set$streak[i-1] #streak before game starts
  }
  return(team_set)
}

chicago.bears = generate_streaks(team_subset(nfl_data.big, 'CHI'))
lm1 = lm(score_margin ~ streak, data = chicago.bears)
print('the regression between Chicago Bears margin of victory and winning streak, see R squared value.')
summary(lm1)  #this is a linear regression of win streak and margin of next game, for 
              #Chicago Bears. This is plotted in the data visualization section as well.

ny.giants = generate_streaks(team_subset(nfl_data.big, 'NYG')) #streak data for NYG
lm2  =lm(score_margin ~ streak, data= ny.giants)
print('the regression between NY Giants margin of victory and winning streak, see R squared value.')
summary(lm2)  #this is an regression of margin vs. streak for another team, NY Giants.
              #It also shows an insignificant R squared value.

chicago.bears.wins = subset(chicago.bears, streak>1) #what if we only look at bona-fide winning streaks?
lm3 = lm(score_margin ~ streak, data = chicago.bears.wins)
print('the regression between Chicago Bears margin of victory and winning streak for games which streak > 1')
print('Pay special note to the insignificant R squared value.')
summary(lm3)  #this is a linear regression of win streak and margin of next game, for 
#Chicago Bears. This is plotted in the data visualization section as well.

cb = subset(chicago.bears, streak >0) #just games where streak is positive for ease of implementation
cb.win.count= c(0,0,0,0,0,0,0,0,0)   #chicago bears wins counter by streak
cb.loss.count = c(0,0,0,0,0,0,0,0,0)    #chicago bears loss counter by streak
cb.elo_prob.count = c(0,0,0,0,0,0,0,0,0)#chicago bears sum of elo probability

for(i in 1:9){
  s = cb$streak[i]
  if(cb$score_margin[i] > 0){cb.win.count[s] = cb.win.count[s] +1}
  else if (cb$score_margin[i] < 0){cb.loss.count[s] = cb.loss.count[s] + 1}
  
  if(cb$score_margin[i] != 0) {cb.elo_prob.count[s] = cb.elo_prob.count[s] + cb$elo_prob1[i]}
}

cb_sum = data.frame(1:9, cb.win.count, cb.loss.count, cb.elo_prob.count)
names(cb_sum) = c('streak', 'wins', 'losses', 'elo_prob_sum')
cb_sum$win_rate = cb_sum$wins/(cb_sum$wins + cb_sum$losses)
cb_sum$pred_win_rate = cb_sum$elo_prob_sum/(cb_sum$wins + cb_sum$losses)
write.table(cb_sum, file = 'streak_win_rate.csv',row.names = F, sep = ",")
print('')

lm4 = lm(score_margin ~ elo_prob1, nfl_data)
summary(lm4) #model to analyze effect of winning prediction on score margin


lm5 = lm(elo_prob1 ~ elo_difference_pre, nfl_data)
summary(lm5) #the R squared value of only 0.9858 shows that there is some
#adjustments made to predict the winner, such as home team advantage or QB effects




# General stats of the dataset - looking at how the score compares for home vs. away team
summary(nfl_data$elo1_pre)
plot(nfl_data$elo1_pre, nfl_data$score1) # Plotting correlation between elo and score in a game

summary(nfl_data$elo2_pre)
plot(nfl_data$elo2_pre, nfl_data$score2) 

summary(nfl_data$score1)
summary(nfl_data$score2) # Comparing the scores of the home vs. visiting team - found statistically significant difference in mean


T_Test = function(sdee, num) # personal T test function, probably not necessary lol
{
  return (sdee/sqrt(num))
}



# Data Visualization

# early charts of the teams
pie(team_name, main='Team Name Codes')
plot(team_name, xlab='nth most common team', ylab='games played')

hist(nfl_data$elo1_pre) # hist. for scores in original dataset


plot(team_name, xlab='nth most common team', ylab='games played') # Plot of elo


plot(nfl_data$elo_difference_pre, nfl_data$score_margin, cex=0.3, pch=16,
     xlab= 'elo difference entering game', ylab = 'score margin',
     main = 'Figure 3- Elo Difference and Match Results', cex.main = 1.0) 
#elo generally correlates with margin but variance is high
lm6 = lm(nfl_data.big$score_margin ~ nfl_data.big$elo_difference_pre)
#lm6 is a linear regression between elo difference between two teams and the score margin of game
abline(lm6, lwd= 2)
summary(lm6)

## WARNING- THIS CODE IS YET TO BE FINISHED##
plot(nfl_data$elo1_pre, nfl_data$score2, cex=0.3, pch=16,
     xlab= 'elo difference entering game', ylab = 'score margin',
     main = 'Figure 3- Elo Difference and Match Results', cex.main = 1.0) 
#elo generally correlates with margin but variance is high
lm6 = lm(nfl_data.big$score2 ~ nfl_data.big$elo1_pre)
#lm6 is a linear regression between elo difference between two teams and the score margin of game
abline(lm6, lwd= 2)
summary(lm6)




##END WORK IN PROGRESS ZONE##

#lm1 plot- Chicago Bears winning streak. NOTE: insignifant R^2 of this regression
plot(chicago.bears$streak,chicago.bears$score_margin, pch=16, cex=0.5, cex.main = 1.0, 
     xlab = '# consecutive games won', ylab = 'margin of victory',
     main="Figure 1- Chicago Bears Streak Fallacy"); grid();
abline(lm(score_margin ~ streak, data = chicago.bears))

#lm3 plot- Chicago Bears winning streak regression for only streak > 1 
#NOTE: insignifant R^2 of this regression
plot(chicago.bears.wins$streak, chicago.bears.wins$score_margin, pch = 16, cex=0.5, cex.main = 1.0,
     xlab = '# consecutive games won', ylab='margin of victory',
     main='Figure 2- Chicago Bears Streak Fallacy')
abline(lm3)

lm1 = lm(score_margin ~ streak, data = chicago.bears)
summary(lm1) #linear model for best fit line in Figure 

