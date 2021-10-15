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

You should be able to run everything from obtaining the data to making new game predictions by running the code in the `full_workflow` file.

## Frequently Anticipated Questions

1. Is this the Pablo rankings?

No. I could probably code an approximation of the Pablo rankings, but without knowing the appropriate home-court advantage and game-recency weights, I don't know how accurate the approximation would be.

2. Is this better than the Pablo rankings?

I don't know. Part of the reason we're releasing the rankings is to figure out how good they are.

3. So what's the difference between this and the Pablo rankings?

There are some minor mathematical differences (Pablo, as far as I can tell, uses an inverse normal transformation of point percentage. We use an inverse logit transformation of sideout percentage).

The big conceptual difference is that while Pablo directly predicts the probability that a team wins a match, we attempt to predict that the probability that a team *scores a point* (i.e., predict the point-scoring percentage and sideout percentage for each team). We then feed those predicted sideout percentages into the [volleysim](https://openvolley.github.io/volleysim/) package to estimate each team's probability of winning the match as well as the likelihood the match goes 3, 4 or 5 games.

4. Where can I find the actual team rankings?

See [Chad's blog](https://volleydork.blog/volleydork-weekly-rankings-match-predictions/).

5. Does this just work on wins/losses?

No. Each serve is modeled as a separate point, with an appropriate offset estimated for the serving team. Essentially, even if two teams win 3-0, the team that serves better and sideouts better will be rewarded more.

6. How do you break the ties between two teams that have the same rating?

First, they don't have the same rating. The ratings are scaled and then rounded to the nearest integer. Second, if you're concerned about quantifying the uncertainty in the ratings, that makes two of us.

7. Where do you get your data?

You can find all sorts of goodies by looking in the NCAA website's [Extended Stats](https://stats.ncaa.org/rankings/change_sport_year_div) section. Most games have a box score, and most games with a box score (at least at the D1 level) have a play-by-play. We use the play-by-play data, when it's available.

8. We won both our matches last week! Why did our rating go down?

There could be several reasons for this. First, one or both games might not have play-by-play results up, in which case the match won't contribute to the ratings. Second, you might not have won by as much as the model expected you to. Third, some of your past wins might not look so good anymore based on the results of your opponents' games.

9. Why are we ranked behind a team that we beat?

There could be several reasons for this. First, it could have been a genuine upset. I don't think anyone actually believes that Alabama State should be rated higher than Auburn, [and yet](https://stats.ncaa.org/contests/2121952/box_score)...that's why they play the games. Second, they could have played much better than you across the entire season-to-date. If they're competitive against Big Ten teams and you're struggling against Big West teams, they're probably going to be ranked higher. Third, especially early in the season, ratings can be highly dependent on one or two results. Fourth, transitivity is a weird thing, and as far as I know, no one has even tested whether the basic assumptions of most rating systems hold for NCAA volleyball.

10. My team sideouts well but we don't serve well (or vice versa). Can you account for this?

Not at the moment. There are a few extensions of paired rating systems that can do this, but I still need to look into their properties and assumptions. The basic Bradley-Terry model has been around a long time for a reason; the only real questions are whether the model assumptions are met and whether the model is properly calibrated.

11. Are you going to do this for Division II/Division III/NAIA?

Not at this time. If *you* want to do it for Division II or Division III, the code for you to do it is literally right here. All you need to do is change the conferences in lines 40-71 of the `full_workflow` file.

12. Are you going to do this for men's volleyball/international vollebyall/etc.?

In theory, if we had play-by-play data, we could do this for any level of indoor volleyball. I'm pretty sure the NCAA website has play-by-play data to do it for men's, but I haven't looked at anything else yet.

13. Are you going to do this for beach volleyball?

Not until a couple of obstacles are solved. The easy obstacle to overcome is fixing the volleysim package to work with the beach format, assuming no wind advantage. The more difficult obstacle to overcome is the lack of play-by-play data, especially for NCAA Women's Beach Volleyball where the match-end condition is not consistent (sometimes they play all 5 games, sometimes they only play until someone wins 3).

14. What is this nonsense with "shrunk ratings"?

In our initial public runs of the model, we found that we were too confident that the favorites would win. This suggested that our scaling factor was too small. Further investigation found that logistic regression produces biased estimates when the number of predictor variables is large (and we have 300+ in the model, considering that each team needs its own predictor variable). The most obvious consequence of this bias was that using the raw rating estimates consistently overestimated the sideout chances of each team.

The "shrunk ratings" shrink the coefficient ratings toward the mean so that when two teams of equal strength play on a neutral court, the model will predict that both teams sideout at the average sideout rate in the full dataset. I'm not sure this is the optimal way to fix this, but it's a start.

15. Are you planning to do anything more with the ratings?

Yes. First, if they suck, we're going to tweak the model to suck less, but we'll probably do that after the season when (1) we have an entire season's worth of data to use for model validation and (2) I have time to explore how far back the NCAA play-by-play data goes. Second, even if they don't suck, I want to develop a couple of modified versions to compare: one that uses separate serve and reception ratings, and one that incorporates preseason rankings via a weakly informative (Bayesian) prior.
