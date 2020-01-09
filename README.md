# Fantasy-Football-2018-Draft-Analysis
Exploring the FF league's 2018 Draft and generally, the league. What went wrong, how did we do, and where can I find a competitive advantage for the next year. How did the year go in the league, what can we analyze, and what can we predict, and how can we compare those.

Drafted Players sheets contain the lists of all drafted players, auction style, in our Fantasy Football League. Our drafts are done offline, and thus there is no ESPN or automate-able data to retrieve all the draft information beyond a manual compilation. The .csv contains the mass list of all the players. The Excel workbook contains a sheet of all the players and separate sheets for each year as well.

10 Guys One Cup Draft Trends.ipynb is the R Jupyter Notebook that contains the Data Frames, graphics, and analysis. Trends, prices, future moves, etc. ff draft trends.R is the corresponding raw script file without the Jupyter Format. These two files are stored within the "2018 Draft Trends" folder.

The "2018 Running Backs Analysis" Folder is an analysis on Running back performance for 2018. For the price of the auctions, how did each player perform? Who was the best/worst picks, etc. This folder contains the raw script file 2018 Running Backs Analysis.R and 2018 Running Backs Analysis.ipynb, the Jupyter Notebook.

"2018 MCMC" is a Markov Chain Monte Carlo regular season predictor. It was done week 9 of the 2018 regular season, meaning the simulation predicted the outcome of the last 3 games (weeks 10 to 13) to predict seeds going into the playoffs. It contains the description, the raw script monte_carlo_sim.py and the corresponding Jupyter Notebook 2018 MCMC for Playoff Standings.ipynb.
