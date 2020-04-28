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
pie(team_name, main='Team Name Codes')
plot(team_name, xlab='nth most common team', ylab='games played')
summary(nfl_data$score1)
hist(nfl_data$elo1_pre)

#description of project
#Each team should also submit a finalized .R or .SAS files. Exclude earlier versions of your work.
#. The program files should contain section headers and other comments to help with readability. The comments should make the reader aware of the main tasks associated with each
#piece of code. The project code is going to be divided as follows:
#  - Data Cleansing.
#- Data aggregation if needed.
#- Data summarization.
#- Data visualization.

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


##Data Cleansing##
#To clean our data, we want to first eliminate the columns detailing quarterback data as this will not be included
#in our analysis. We can do this by dropping columns with missing values.
nfl_data = nfl_data[,c('date','season','neutral','playoff','team1','team2','elo1_pre',
                        'elo2_pre','elo_prob1','elo_prob2','elo1_post','elo2_post',
                        'score1','score2')]

##Adding New Variables: score difference (margin), winning team(winning_team), 
nfl_data$score_margin = nfl_data$score1-nfl_data$score2 #positive margin is team1 winning, negative is team2 winning

nfl_data$winning_team = nfl_data$score_margin #recode copy of score_margin to winning team
nfl_data$winning_team = ifelse(nfl_data$winning_team >0, 1, (ifelse(nfl_data$winning_team<0, 2, 0))) #0=tie, 1=team1, 2=team2

nfl_data$elo_difference_pre = nfl_data$elo1_pre-nfl_data$elo2_pre #difference in elo pre-game, positive is greater team1 elo
plot(team_name, xlab='nth most common team', ylab='games played')

#Make a variable tracking which row is which, useful for sorting by date later.
nfl_data$row_num = 1:length(nfl_data$date)

#remove teams playing fewer than 20 games
#this is implemented by making a summary of team1, subsetting only >25, and using row names to subset the nfl_elo dataframe

team_sizes1 = summary(nfl_data$team1) 
team_sizes1.df = data.frame(team_sizes1)
team_sizes1.df.big = subset(team_sizes1.df, team_sizes1>50)

team_sizes2 = summary(nfl_data$team2)
team_sizes2.df = data.frame(team_sizes2)
team_sizes2.df.big = subset(team_sizes2.df, team_sizes2>50)

nfl_data.big = subset(nfl_data, team1 %in% row.names(team_sizes1.df.big) 
                      & team2 %in% row.names(team_sizes2.df.big), select = colnames(nfl_data))

plot(nfl_data$elo_difference_pre, nfl_data$score_margin) #elo generally correlates with margin but variance is high

summary(nfl_data.big$team1)
summary(nfl_data$team1)


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
chicago.bears = generate_streaks(chicago.bears)
plot(chicago.bears$streak,chicago.bears$score_margin)

lm1 = lm(score_margin ~ streak, data = chicago.bears)
summary(lm1) #linear regression of score margin (y) vs. winning streak (x) for CHI Bears

nyg = team_subset(nfl_data, 'NYG') #linear regression using NY Giants
nyg = generate_streaks(nyg)
lm2 = lm(streak_pre ~ streak, data = nyg)
summary(lm2)
summary(nyg$score2)

lm3 = lm(score_margin ~ elo_prob1, nfl_data)
summary(lm3) #model to analyze effect of winning prediction on score margin


lm4 = lm(elo_prob1 ~ elo_difference_pre, nfl_data)
summary(lm4) #the R squared value of only 0.9858 shows that there is some
#adjustments made to predict the winner, such as home team advantage or QB effects




