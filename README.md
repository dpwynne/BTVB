# BTVB
## Bradley-Terry rankings for NCAA volleyball

Code in this repository is used to create rankings for NCAA D1 volleyball teams based on the Bradley-Terry (Bradley-Terry-Luce) method for ranking paired comparisons.

We use the [BradleyTerry2](https://cran.r-project.org/web/packages/BradleyTerry2/vignettes/BradleyTerry.pdf) package in R to create the rankings, once the appropriate data have been obtained.

Obtaining the data requires five steps:

1. Obtain the list of teams and their unique IDs from the NCAA website. The relevant function in this repository is `team_mapping`.
2. Obtain the schedule for each team. There are two functions in this repository that do this: `get_team_schedule` obtains all games played by a team for which the NCAA website has a box score link, and `get_team_future_schedule` obtains all games listed for a team on the NCAA website, whether there is a box score link or not.
3. Find the links to the box score and (if it exists) play-by-play information. The relevant function in this repository is `pbp_boxscore_links`.
4. Scrape and parse the play-by-play data for all games that contain this information. Note that not all games have play-by-play data, and the NCAA website is not consistent in how the play-by-play data is formatted. I have identified two different formats for the play-by-play data, which can be scraped using `vb_play_by_play_ncaa` or `vb_play_by_play_ncaa2`. The `vb_play_by_play` function tries to scrape data in the first format, and automatically tries the second format if it fails.
5. Fix misspelled or inconsistent team names (e.g., `fix_NCAA_names`) and do other necessary data prep.

We then summarize the data, run the Bradley-Terry model to rank the teams and obtain the appropriate adjustments for serving and home-court, and scale the ratings to have mean 1500 and standard deviation 500.

The overall sequence of steps can be found in the `BT_workflow` file.

## Model Questions

1. Does this just work on wins/losses?

No. Each serve is modeled as a separate point, with an appropriate offset estimated for the serving team. Essentially, even if two teams win 3-0, the team that serves better and sideouts better will be rewarded more.

2. Why does Texas A&M-Corpus Christi have a raw rating of 0?

The Bradley-Terry model, like any regression model that includes categorical variables, requires a reference level that does not appear in the model. In the Bradley-Terry model, the rating for the reference level is hard-coded to 0. The BradleyTerry2 package, by default, uses the first level as the reference level. In the dataset, this corresponds to the team that comes first alphabetically according to the NCAA website team names, which happens to be "A&M-Corpus Christi."
