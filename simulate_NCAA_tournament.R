library(dplyr)
library(readr)
library(volleysim)

NCAA_tournament_teams <- read_csv("NCAA_tournament_2021b.csv")

## Import ratings file
ratings <- read_csv("ratings_2021-11-28.csv")

serve_tournament <- ratings %>% filter(Team %in% NCAA_tournament_teams$Team | Team %in% c("Serve Adjustment", "Home Court Adjustment", "Scaling Factor"))

serve_adj2 <- serve_tournament$Rating_Raw[65]
serve_hc <- serve_tournament$Rating_Raw[66]

predict_game <- function(home_sideout,away_sideout){
  game_tribble <- tibble::tribble(
    ~team, ~sideout,
    "home", home_sideout,
    "away", away_sideout
  )
  
  game_result <- vs_simulate_match_theor(game_tribble, process_model = "sideout")
  
  return(game_result)
}


### Round 1

round1_opp <- NCAA_tournament_teams %>% mutate(r1_game = seq(0,63) %/% 2, r1_win = 0)

for (k in 0:31){  # 32 Round 1 pods
 pod_teams <- round1_opp %>% filter(r1_game == k) %>% mutate(
   hc_adj = sum(Home.R12) * (2*Home.R12 - 1) * serve_hc
 )
 team_ratings <- pod_teams %>% left_join(serve_tournament, by = "Team") %>% pull(Rating_Raw)
 
 bottom_so <- 1/(1 + exp(team_ratings[1] - team_ratings[2] + serve_adj2 + pod_teams$hc_adj[1]))
 top_so <- 1/(1 + exp(team_ratings[2] - team_ratings[1] + serve_adj2 + pod_teams$hc_adj[2]))
 game_prediction <- predict_game(top_so, bottom_so)
 team_win <- c(game_prediction$pwin, 1 - game_prediction$pwin)
 round1_opp$r1_win[which(round1_opp$r1_game == k)] <- team_win
}

round2_opp <- round1_opp %>% mutate(r2_game = seq(0, 63) %/% 4, r2_win = 0)

for (k in 0:15){  # Round 2 pods
  pod_teams <- round2_opp %>% filter(r2_game == k)

  pod_matches <- data.frame(
    Home = rep(pod_teams$Team[1:2], 2),
    Away = rep(pod_teams$Team[3:4], each = 2),
    chance = c(pod_teams$r1_win[1]*pod_teams$r1_win[3],
               pod_teams$r1_win[2]*pod_teams$r1_win[3],
               pod_teams$r1_win[1]*pod_teams$r1_win[4],
               pod_teams$r1_win[2]*pod_teams$r1_win[4])
  ) %>%
    left_join(serve_tournament %>% select(Team, Rating_Raw), by = c("Home" = "Team")) %>%
    rename(Home_Rating_Raw = Rating_Raw) %>%
    left_join(serve_tournament %>% select(Team, Rating_Raw), by = c("Away" = "Team")) %>%
    rename(Away_Rating_Raw = Rating_Raw) %>%
    left_join(pod_teams %>% select(Team, Home.R12), by = c("Home" = "Team")) %>%
    rename(Home.HCA = Home.R12) %>%
    left_join(pod_teams %>% select(Team, Home.R12), by = c("Away" = "Team")) %>%
    rename(Away.HCA = Home.R12) %>% mutate(
      hc_adj = (Home.HCA + Away.HCA)*(Home.HCA - Away.HCA) * serve_hc,
      Home_win = 0,
      Away_win = 0
    )
  
  
  for (j in 1:nrow(pod_matches)){
    team_ratings <- c(pod_matches$Home_Rating_Raw[j], pod_matches$Away_Rating_Raw[j])
    bottom_so <- 1/(1 + exp(team_ratings[1] - team_ratings[2] + serve_adj2 + pod_matches$hc_adj[j]))
    top_so <- 1/(1 + exp(team_ratings[2] - team_ratings[1] + serve_adj2 - pod_matches$hc_adj[j]))
    game_prediction <- predict_game(top_so, bottom_so)
    pod_matches$Home_win[j] = game_prediction$pwin
    pod_matches$Away_win[j] = 1 - game_prediction$pwin
  }
  
  home_advance <- pod_matches$Home_win*pod_matches$chance
  away_advance <- pod_matches$Away_win*pod_matches$chance
  
  pod_teams <- pod_teams %>% mutate(
    r2_win = c(sum(home_advance[c(1,3)]),
               sum(home_advance[c(2,4)]),
               sum(away_advance[c(1,2)]),
               sum(away_advance[c(3,4)])
    )
  )
  
  round2_opp$r2_win[which(round2_opp$r2_game == k)] <- pod_teams$r2_win
}

## Round 3 - no longer home court advantage

round3_opp <- round2_opp %>% mutate(r3_game = seq(0, 63) %/% 8, r3_win = 0)

for (k in 0:7){  # Round 2 pods
  pod_teams <- round3_opp %>% filter(r3_game == k)
  
  pod_chances <- as.numeric(pod_teams$r2_win[1:4] %*% t(pod_teams$r2_win[5:8]))
  
  pod_matches <- data.frame(
    Home = rep(pod_teams$Team[1:4], 4),
    Away = rep(pod_teams$Team[5:8], each = 4),
    chance = c(pod_chances)
  ) %>%
    left_join(serve_tournament %>% select(Team, Rating_Raw), by = c("Home" = "Team")) %>%
    rename(Home_Rating_Raw = Rating_Raw) %>%
    left_join(serve_tournament %>% select(Team, Rating_Raw), by = c("Away" = "Team")) %>%
    rename(Away_Rating_Raw = Rating_Raw) %>%
    left_join(pod_teams %>% select(Team, Seed), by = c("Home" = "Team")) %>%
    rename(Home.Seed = Seed) %>%
    mutate(Home.HCA = as.numeric(Home.Seed <= 4)) %>%
    left_join(pod_teams %>% select(Team, Seed), by = c("Away" = "Team")) %>%
    rename(Away.Seed = Seed) %>% mutate(
      Away.HCA = as.numeric(Away.Seed <= 4),
      hc_adj = (Home.HCA + Away.HCA)*(Home.HCA - Away.HCA) * serve_hc,
      Home_win = 0,
      Away_win = 0
    )
  
  for (j in 1:nrow(pod_matches)){
    team_ratings <- c(pod_matches$Home_Rating_Raw[j], pod_matches$Away_Rating_Raw[j])
    bottom_so <- 1/(1 + exp(team_ratings[1] - team_ratings[2] + serve_adj2 + pod_matches$hc_adj[j]))
    top_so <- 1/(1 + exp(team_ratings[2] - team_ratings[1] + serve_adj2 - pod_matches$hc_adj[j]))
    game_prediction <- predict_game(top_so, bottom_so)
    pod_matches$Home_win[j] = game_prediction$pwin
    pod_matches$Away_win[j] = 1 - game_prediction$pwin
  }

  home_advance <- matrix(pod_matches$Home_win*pod_matches$chance, 4, 4)
  away_advance <- matrix(pod_matches$Away_win*pod_matches$chance, 4, 4)
  
  pod_teams <- pod_teams %>% mutate(
    r3_win = c(apply(home_advance, 1, sum),
               apply(away_advance, 2, sum))
  )
  
  round3_opp$r3_win[which(round3_opp$r3_game == k)] <- pod_teams$r3_win
}

## Round 4

round4_opp <- round3_opp %>% mutate(r4_game = seq(0, 63) %/% 16, r4_win = 0)

for (k in 0:3){  # Round 4 pods
  pod_teams <- round4_opp %>% filter(r4_game == k)
  
  pod_chances <- as.numeric(pod_teams$r3_win[1:8] %*% t(pod_teams$r3_win[9:16]))
  
  pod_matches <- data.frame(
    Home = rep(pod_teams$Team[1:8], 8),
    Away = rep(pod_teams$Team[9:16], each = 8),
    chance = c(pod_chances)
  ) %>%
    left_join(serve_tournament %>% select(Team, Rating_Raw), by = c("Home" = "Team")) %>%
    rename(Home_Rating_Raw = Rating_Raw) %>%
    left_join(serve_tournament %>% select(Team, Rating_Raw), by = c("Away" = "Team")) %>%
    rename(Away_Rating_Raw = Rating_Raw) %>%
    left_join(pod_teams %>% select(Team, Seed), by = c("Home" = "Team")) %>%
    rename(Home.Seed = Seed) %>%
    mutate(Home.HCA = as.numeric(Home.Seed <= 4)) %>%
    left_join(pod_teams %>% select(Team, Seed), by = c("Away" = "Team")) %>%
    rename(Away.Seed = Seed) %>% mutate(
      Away.HCA = as.numeric(Away.Seed <= 4),
      hc_adj = (Home.HCA + Away.HCA)*(Home.HCA - Away.HCA) * serve_hc,
      Home_win = 0,
      Away_win = 0
    )
  
  for (j in 1:nrow(pod_matches)){
    team_ratings <- c(pod_matches$Home_Rating_Raw[j], pod_matches$Away_Rating_Raw[j])
    bottom_so <- 1/(1 + exp(team_ratings[1] - team_ratings[2] + serve_adj2 + pod_matches$hc_adj[j]))
    top_so <- 1/(1 + exp(team_ratings[2] - team_ratings[1] + serve_adj2 - pod_matches$hc_adj[j]))
    game_prediction <- predict_game(top_so, bottom_so)
    pod_matches$Home_win[j] = game_prediction$pwin
    pod_matches$Away_win[j] = 1 - game_prediction$pwin
  }
  
  
  home_advance <- matrix(pod_matches$Home_win*pod_matches$chance, 8, 8)
  away_advance <- matrix(pod_matches$Away_win*pod_matches$chance, 8, 8)
  
  pod_teams <- pod_teams %>% mutate(
    r4_win = c(apply(home_advance, 1, sum),
               apply(away_advance, 2, sum))
  )
  
  round4_opp$r4_win[which(round4_opp$r4_game == k)] <- pod_teams$r4_win
}

## Round 5

round5_opp <- round4_opp %>% mutate(r5_game = seq(0, 63) %/% 32, r5_win = 0)

for (k in 0:1){  # Round 5 pods
  pod_teams <- round5_opp %>% filter(r5_game == k)
  
  pod_chances <- as.numeric(pod_teams$r4_win[1:16] %*% t(pod_teams$r4_win[17:32]))
  
  pod_matches <- data.frame(
    Home = rep(pod_teams$Team[1:16], 16),
    Away = rep(pod_teams$Team[17:32], each = 16),
    chance = c(pod_chances)
  ) %>%
    left_join(serve_tournament %>% select(Team, Rating_Raw), by = c("Home" = "Team")) %>%
    rename(Home_Rating_Raw = Rating_Raw) %>%
    left_join(serve_tournament %>% select(Team, Rating_Raw), by = c("Away" = "Team")) %>%
    rename(Away_Rating_Raw = Rating_Raw)
  
  for (j in 1:nrow(pod_matches)){
    team_ratings <- c(pod_matches$Home_Rating_Raw[j], pod_matches$Away_Rating_Raw[j])
    bottom_so <- 1/(1 + exp(team_ratings[1] - team_ratings[2] + serve_adj2))
    top_so <- 1/(1 + exp(team_ratings[2] - team_ratings[1] + serve_adj2))
    game_prediction <- predict_game(top_so, bottom_so)
    pod_matches$Home_win[j] = game_prediction$pwin
    pod_matches$Away_win[j] = 1 - game_prediction$pwin
  }
  
  home_advance <- matrix(pod_matches$Home_win*pod_matches$chance, 16, 16)
  away_advance <- matrix(pod_matches$Away_win*pod_matches$chance, 16, 16)
  
  pod_teams <- pod_teams %>% mutate(
    r5_win = c(apply(home_advance, 1, sum),
               apply(away_advance, 2, sum))
  )
  
  round5_opp$r5_win[which(round5_opp$r5_game == k)] <- pod_teams$r5_win
  
}

## Round 6 - finals

round6_opp <- round5_opp %>% mutate(r6_win = 0)

  pod_chances <- as.numeric(round6_opp$r5_win[1:32] %*% t(round6_opp$r5_win[33:64]))
  
  pod_matches <- data.frame(
    Home = rep(round6_opp$Team[1:32], 32),
    Away = rep(round6_opp$Team[33:64], each = 32),
    chance = c(pod_chances)
  ) %>%
    left_join(serve_tournament %>% select(Team, Rating_Raw), by = c("Home" = "Team")) %>%
    rename(Home_Rating_Raw = Rating_Raw) %>%
    left_join(serve_tournament %>% select(Team, Rating_Raw), by = c("Away" = "Team")) %>%
    rename(Away_Rating_Raw = Rating_Raw)
  
  for (j in 1:nrow(pod_matches)){
    team_ratings <- c(pod_matches$Home_Rating_Raw[j], pod_matches$Away_Rating_Raw[j])
    bottom_so <- 1/(1 + exp(team_ratings[1] - team_ratings[2] + serve_adj2))
    top_so <- 1/(1 + exp(team_ratings[2] - team_ratings[1] + serve_adj2))
    game_prediction <- predict_game(top_so, bottom_so)
    pod_matches$Home_win[j] = game_prediction$pwin
    pod_matches$Away_win[j] = 1 - game_prediction$pwin
  }
  
  home_advance <- matrix(pod_matches$Home_win*pod_matches$chance, 32, 32)
  away_advance <- matrix(pod_matches$Away_win*pod_matches$chance, 32, 32)
  
  round6_opp <- round6_opp %>% mutate(
    r6_win = c(apply(home_advance, 1, sum),
               apply(away_advance, 2, sum))
  )
  
tournament_prediction_date <- "2021-11-30"
tourney_filename <- paste0("tournament_predictions_", tournament_prediction_date, ".csv")

tourney_df <- round6_opp %>% select(Team, Seed, r1_win, r2_win, r3_win, r4_win, r5_win, r6_win) %>%
  rename(`Round of 32` = r1_win,
         `Sweet Sixteen` = r2_win,
         `Elite Eight` = r3_win,
         `Final Four` = r4_win,
         `Championship Game` = r5_win,
         `National Champion` = r6_win) %>%
  arrange(desc(`National Champion`))

write_csv(tourney_df, tourney_filename)
