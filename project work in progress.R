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


##Data Cleansing##
#To clean our data, we want to first eliminate the columns detailing quarterback data as this will not be included
#in our analysis. We can do this by dropping columns with missing values.
colnames = names(nfl_data)
colnames

##Adding New Variables: score difference (margin), winning team(winning_team), 
nfl_data$score_margin = nfl_data$score1-nfl_data$score2 #positive margin is team1 winning, negative is team2 winning

nfl_data$winning_team = nfl_data$score_margin
ifelse(nfl_data$winning_team >0, 1, ifelse(nfl_data$winning_team<0, 2, 0)) #0=tie, 1=team1, 2=team2
nfl_data$elo_difference_pre = nfl_data$elo1_pre-nfl_data$elo2_pre #difference in elo pre-game, positive is greater team1 elo
plot(team_name, xlab='nth most common team', ylab='games played')

#remove teams playing fewer than 20 games
#this is implemented by making a summary of team1, subsetting only >25, and using row names to subset the nfl_elo dataframe
#

team_sizes1 = summary(nfl_data$team1) 
team_sizes1.df = data.frame(team_sizes1)
team_sizes1.df.big = subset(team_sizes1.df, team_sizes1>25)

team_sizes2 = summary(nfl_data$team2)
team_sizes2.df = data.frame(team_sizes2)
team_sizes2.df.big = subset(team_sizes2.df, team_sizes2>25)

nfl_data.big = subset(nfl_data, team1 %in% row.names(team_sizes1.df.big) 
                      & team2 %in% row.names(team_sizes2.df.big), select = colnames(nfl_data))

plot(nfl_data$elo_difference_pre, nfl_data$score_margin) #elo generally correlates with margin but variance is high

