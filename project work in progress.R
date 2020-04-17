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
nfl_data_c = nfl_data

