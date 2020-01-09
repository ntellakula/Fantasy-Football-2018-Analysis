# -*- coding: utf-8 -*-
"""
Created on Wed Jan  8 19:38:58 2020

@author: NTellaku
"""

# -*- coding: utf-8 -*-
"""
Created on Wed Jan  8 14:22:40 2020

@author: NTellaku
"""

import requests
import pandas as pd
import numpy as np
import scipy.stats as stats
import seaborn as sns
import matplotlib.pyplot as plt

def team_win_loss(dataset, i):
    """
    Adding the win-loss record
    dataset: which dataset to use?
    i: iterator
    """
    
    team = dataset[dataset["Team"] == team_ids[i]]
    team_wins = sum(n > 0 for n in team["Margin"])
    team_ties = sum(n == 0 for n in team["Margin"])
    team_loss = sum(n < 0 for n in team["Margin"])
    points = np.sum(team.PointsFor)
    wl_info = pd.DataFrame([[team_ids[i], team_wins, team_loss,
                             team_ties, points]],
                           columns = ["Team", "Wins", "Losses", "Ties", "Points"])
    return wl_info

def matchup(home_team, away_team, week):
    """
    Input:  home_team: home team number
            away_team: away team number
            week: which week for the matchup
            
    calculates who wins each individual matchup.
    assuming each team scores assumes a gaussian distribution
    """
    
    #getting the summaries for each team
    summary_stats = (pd.DataFrame([analysis.sort_values(by = ["Team"])
                                           .Team
                                           .unique(),
                                   analysis.groupby("Team")
                                           .mean()["PointsFor"],
                                   analysis.groupby("Team")
                                           .std()["PointsFor"]]).T)
    
    #cleaning the data set
    colnames = ["Team", "AvgPoints", "StDPoints"]
    summary_stats.columns = colnames
    
    #find each team's scores
    home = np.random.normal(summary_stats[summary_stats.Team == home_team].AvgPoints,
                            summary_stats[summary_stats.Team == away_team].StDPoints,
                            1) #only one score "Any given Sunday"
    away = np.random.normal(summary_stats[summary_stats.Team == home_team].AvgPoints,
                            summary_stats[summary_stats.Team == away_team].StDPoints,
                            1)
    
    #make same format as analysis data set
    home_row = np.array([week, home_team, home[0], (home - away)[0], away[0]])
    away_row = np.array([week, away_team, away[0], (away - home)[0], home[0]])
    
    #apped these rows to the analysis data set
    extra_matchup = (pd.DataFrame([home_row, away_row],
                                  columns = analysis.columns))
    
    return extra_matchup

#team number to name Dictionary
mapping = {1: "Mount",
           2: "Alec",
           3: "Sirpi",
           4: "Oatman",
           5: "Babcock",
           9: "Jordan",
           11: "Casey",
           12: "Badillo",
           13: "Naki",
           14: "Kooper"}


league_id = 298982
year = 2018


url = "https://fantasy.espn.com/apis/v3/games/ffl/leagueHistory/" + str(league_id) + "?seasonId=" + str(year)
r = requests.get(url, params = {"view": "mMatchup"})
d = r.json()[0]

maximum = max(pd.DataFrame(d["schedule"]).index.tolist()) #how many obs?
length_df = pd.DataFrame([[d["schedule"][i]["winner"]] for i in range(maximum)])
length_df = length_df[length_df[0] != "UNDECIDED"]
length = range(len(length_df))


#Selecting weeks, points, data
source = pd.DataFrame([[d["schedule"][i]["matchupPeriodId"],
                        d["schedule"][i]["home"]["teamId"],
                        d["schedule"][i]["home"]["totalPoints"],
                        d["schedule"][i]["away"]["teamId"],
                        d["schedule"][i]["away"]["totalPoints"]] for i in length],
                        columns = ["Week", "Team1", "Score1", "Team2", "Score2"])

#add margin of defeat/victory
margins = source.assign(Margin1 = source["Score1"] - source["Score2"],
                        Margin2 = source["Score2"] - source["Score1"])

#only first 9 weeks of data
source_9 = margins[margins.Week < 10]

#transform from wide to long
source_long = (source_9.loc[:, ("Week", "Team1", "Score1", "Margin1")]
                         .rename(columns = {"Team1": "Team",
                                            "Score1": "Score",
                                            "Margin1": "Margin"})
                         .append(source_9.loc[:, ("Week", "Team2", "Score2", "Margin2")]
                             .rename(columns = {"Team2": "Team",
                                                "Score2": "Score",
                                                "Margin2": "Margin"})))

source_long = (source_long.assign(PA = source_long.Score - source_long.Margin)
                          .rename(columns = {"Score": "PointsFor",
                                             "PA": "Points Against"}))


#simming 1000 times
sim_standings = pd.DataFrame()

for l in range(1000):

    analysis = source_long.copy()
    
    #Outer loop to create the weekly object
    for i in range(10, 14):
        weekly_matchups = margins[margins.Week == i].loc[:, ["Team1", "Team2"]]
        week = i
        
        #inner loop to do the matchup
        for j in range(5):
            home_team = weekly_matchups.iloc[j, 0]
            away_team = weekly_matchups.iloc[j, 1]
            result = matchup(home_team, away_team, i)
            analysis = analysis.append(result)
            
            
    #creating record from values
    team_ids = source_long.Team.unique()
    
    #initialize an empty dataframe to append to
    win_loss = []
    
    #loop through all the teams and have the rows append
    for j in range(len(team_ids)):
        row = team_win_loss(analysis, j)
        win_loss.append(row)
    
    win_loss = (pd.concat(win_loss)
                  .sort_values(by = ["Wins", "Ties", "Points"], ascending = False)
                  .assign(Standing = np.arange(1, 11))
                  .sort_values(by = ["Team"])
                  .reset_index(drop = True))
    
    sim_standings[l] = win_loss.Standing

#keeping a separate copy to avoid computation time
sim_standings2 = sim_standings.copy()    

sim_standings.insert(loc = 0, value = win_loss.Team, column = "Team")

#adding team name, making it the index
final = sim_standings.replace({"Team": mapping}).set_index("Team")

#how many at each rank?
final.loc["Mount", :].value_counts()
counts = pd.DataFrame([final.loc[i, :].value_counts() for i in mapping.values()])

#replace NA with 0
counts.fillna(0, inplace = True)

#where are the teams most likely to place based on the sim?
predicted_ranks = (pd.DataFrame([counts.max(), counts.idxmax()])
                        .T
                        .assign(Rank = np.arange(1, 11)))
colnames = ["Count", "Team", "Rank"]
predicted_ranks.columns = colnames
predicted_ranks = predicted_ranks.set_index("Team")


#Observed ranks
#transform from wide to long
source_13 = margins[margins.Week <= 13]
margins_long = (source_13.loc[:, ("Week", "Team1", "Score1", "Margin1")]
                         .rename(columns = {"Team1": "Team",
                                            "Score1": "PointsFor",
                                            "Margin1": "Margin"})
                         .append(source_13.loc[:, ("Week", "Team2", "Score2", "Margin2")]
                             .rename(columns = {"Team2": "Team",
                                                "Score2": "PointsFor",
                                                "Margin2": "Margin"})))

#initialize an empty dataframe to append to
final_win_loss = []

#loop through all the teams and have the rows append
for j in range(len(team_ids)):
    row = team_win_loss(margins_long, j)
    final_win_loss.append(row)

final_win_loss = (pd.concat(final_win_loss)
                    .sort_values(by = ["Wins", "Ties", "Points"], ascending = False)
                    .assign(Standing = np.arange(1, 11))
                    .sort_values(by = ["Team"])
                    .reset_index(drop = True))

comparison = (final_win_loss.replace({"Team": mapping})
                            .set_index("Team")
                            .merge(how = "right",
                                   right = predicted_ranks,
                                   left_index = True,
                                   right_index = True)
                            .loc[:, ["Rank", "Standing"]]
                            .rename(columns = {"Rank": "Predicted",
                                               "Standing": "Observed"}))
tau, p_value = stats.kendalltau(comparison.Predicted,
                                comparison.Observed)


#heatmap of the percentages
counts2 = counts.reindex(predicted_ranks.index) / 10
cmap = sns.diverging_palette(10, 150, as_cmap = True)
sns.heatmap(counts2, cmap = cmap, cbar = False,
            annot = counts2, linewidth = 0.5)
plt.title("1000 Simulations - Percentage of Each Final Standing")