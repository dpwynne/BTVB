# BTVB
Bradley-Terry rankings for NCAA volleyball

Code in this repository is used to create rankings for NCAA D1 volleyball teams based on the Bradley-Terry (Bradley-Terry-Luce) method for ranking paired comparisons.

References to Bradley-Terry model go here.

1. Functions to obtain the list of teams and their unique IDs on the NCAA website
2. Functions to obtain the schedule for a team, sport, and year: two versions (get_team_schedule for games played, and get_team_future_schedule for all games this season, played or not, listed on NCAA website)
3. Functions to scrape the play-by-play data from the NCAA website: play-by-play data is not consistent so there is a wrapper function that will "guess" which format the play-by-play data is in
4. Functions to identify which team is serving
5. Functions to summarize each combination of serving team, opponent, and home/away/neutral site
6. Computing the rankings using the BradleyTerry2 package
7. Scaling the rankings to have mean 1500 and standard deviation 500

R code still needs to be cleaned, moved to the correct folder, and uploaded.

## Model Questions

1. Does this just work on wins/losses?

No. Each serve is modeled as a separate point, with an appropriate offset estimated for the serving team. Essentially, even if two teams win 3-0, the team that serves better and sideouts better will be rewarded more.
