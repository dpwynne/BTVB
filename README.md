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

## Frequently Anticipated Questions

1. Is this the Pablo rankings?

No. I could probably code an approximation of the Pablo rankings, but without knowing the appropriate home-court advantage and game-recency weights, I don't know how accurate the approximation would be.

2. Is this better than the Pablo rankings?

I don't know. Part of the reason we're releasing the rankings is to figure out how good they are.

3. So what's the difference between this and the Pablo rankings?

Pablo, as far as I can tell, uses an inverse normal transformation of point percentage. We use an inverse logit transformation of sideout percentage. (For stats nerds, it's the difference between probit and logistic regression.) We also use the [volleysim](https://openvolley.github.io/volleysim/) package to estimate our win probabilities directly.

4. Does this just work on wins/losses?

No. Each serve is modeled as a separate point, with an appropriate offset estimated for the serving team. Essentially, even if two teams win 3-0, the team that serves better and sideouts better will be rewarded more.

5. How do you break the ties between two teams that have the same rating?

First, they don't have the same rating. The ratings are scaled and then rounded to the nearest integer. Second, if you're concerned about quantifying the uncertainty in the ratings, that makes two of us.

6. Where do you get your data?

You can find all sorts of goodies by looking in the NCAA website's [Extended Stats](https://stats.ncaa.org/rankings/change_sport_year_div) section. Most games have a box score, and most games with a box score (at least at the D1 level) have a play-by-play. We use the play-by-play data, when it's available.

7. We won both our matches last week! Why did our rating go down?

There could be several reasons for this. First, one or both games might not have play-by-play results up, in which case the match won't contribute to the ratings. Second, you might not have won by as much as the model expected you to. Third, some of your past wins might not look so good anymore based on the results of your opponents' games.

8. Why are we ranked behind a team that we beat?

There could be several reasons for this. First, it could have been a genuine upset. I don't think anyone actually believes that Alabama State should be rated higher than Auburn, [and yet](https://stats.ncaa.org/contests/2121952/box_score)...that's why they play the games. Second, they could have played much better than you across the entire season-to-date. If they're competitive against Big Ten teams and you're struggling against Big West teams, they're probably going to be ranked higher. Third, especially early in the season, ratings can be highly dependent on one or two results. Fourth, transitivity is a weird thing, and as far as I know, no one has even tested whether the basic assumptions of most rating systems hold for NCAA volleyball.

9. My team sideouts well but we don't serve well (or vice versa). Can you account for this?

Not at the moment. There are a few extensions of paired rating systems that can do this, but I still need to look into their properties and assumptions. The basic Bradley-Terry model has been around a long time for a reason; the only real questions are whether the model assumptions are met and whether the model is properly calibrated.

10. Why does Texas A&M-Corpus Christi have a raw rating of 0?

The Bradley-Terry model, like any regression model that includes categorical variables, requires a reference level that does not appear in the model. In the Bradley-Terry model, the rating for the reference level is hard-coded to 0. The BradleyTerry2 package, by default, uses the first level as the reference level. In the dataset, this corresponds to the team that comes first alphabetically according to the NCAA website team names, which happens to be "A&M-Corpus Christi."

11. Are you planning to do anything more with the ratings?

Yes. First, if they suck, we're going to tweak the model to suck less, but we'll probably do that after the season when (1) we have an entire season's worth of data to use for model validation and (2) I have time to explore how far back the NCAA play-by-play data goes. Second, even if they don't suck, I want to develop a couple of modified versions to compare: one that uses separate serve and reception ratings, and one that incorporates preseason rankings via a weakly informative (Bayesian) prior.
