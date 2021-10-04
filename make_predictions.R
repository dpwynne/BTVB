all_games <- read_csv("wvb_schedule_2021f.csv")

library(lubridate)
sunday_date <- "2021-10-03"
games_week <- all_games %>% filter(Date >= (ymd(sunday_date) + ddays(1)), Date < (ymd(sunday_date) + ddays(8)))

current_ratings_file <- paste0("C:/Users/dpwyn/Documents/Volleyball Analytics/BTVB/ratings_", sunday_date, ".csv")

current_ratings <- read_csv(current_ratings_file)

current_ratings_teams <- current_ratings %>% select(Team, Rank, Rating)

games_predictions <- games_week %>% mutate(
  away = if_else(Opponent != Location, Opponent, Team),
  home = if_else(Opponent == Location, Opponent, Team)
) %>% left_join(
  current_ratings_teams, by = c("home" = "Team")
) %>% rename(Home_Rating = Rating, Home_Rank = Rank) %>%
  left_join(
    current_ratings_teams, by = c("away" = "Team")
  ) %>% rename(Away_Rating = Rating, Away_Rank = Rank)

scale_factor <- current_ratings_teams$Rating[current_ratings_teams$Team == "Scaling Factor"]
serve_adj <- current_ratings_teams$Rating[current_ratings_teams$Team == "Serve Adjustment"]
home_adj <- current_ratings_teams$Rating[current_ratings_teams$Team == "Home Court Adjustment"]

games_predictions2 <- games_predictions %>% mutate(
  Home_Adj = home_adj*(home == Location),
  away_sideout = 1/(1 + exp((Home_Rating - Away_Rating + serve_adj + Home_Adj)/scale_factor)),
  home_sideout = 1/(1 + exp((Away_Rating - Home_Rating + serve_adj - Home_Adj)/scale_factor))
)

# home point => home_rating - away_rating + serve_adj + home_adj
# away point => away_rating - home_rating + serve_adj - home_adj

predict_game <- function(home_sideout,away_sideout){
  game_tribble <- tibble::tribble(
    ~team, ~sideout,
    "home", home_sideout,
    "away", away_sideout
  )
  
  game_result <- volleysim::vs_simulate_match_theor(game_tribble, process_model = "sideout")

  return(game_result)
}

predictions_list <- map2(games_predictions2$home_sideout, games_predictions2$away_sideout, predict_game)
predictions_list2 <- transpose(predictions_list)

set_predictions <- transpose(predictions_list2$scores)

games_predictions3 <- games_predictions2 %>% mutate(
  home_win = unlist(predictions_list2$pwin),
  home_30 = unlist(set_predictions$`3-0`),
  home_31 = unlist(set_predictions$`3-1`),
  home_32 = unlist(set_predictions$`3-2`),
  away_32 = unlist(set_predictions$`2-3`),
  away_31 = unlist(set_predictions$`1-3`),
  away_30 = unlist(set_predictions$`0-3`),
  Prediction = if_else(home_win >= 0.5, home, away),
  Confidence = if_else(Prediction == home, home_win, 1 - home_win)
) %>% mutate(
  Win3 = if_else(Prediction == home, home_30, away_30),
  Win4 = if_else(Prediction == home, home_31, away_31),
  Win5 = if_else(Prediction == home, home_32, away_32),
  Lose3 = if_else(Prediction == home, away_30, home_30),
  Lose4 = if_else(Prediction == home, away_31, home_31),
  Lose5 = if_else(Prediction == home, away_32, home_32)
)

games_predictions_final <- games_predictions3 %>% transmute(
  Date = as.Date(Date),
  Home = home,
  Away = away,
  Location = Location,
  `Home Rank` = Home_Rank,
  `Away Rank` = Away_Rank,
  `Home Rating` = Home_Rating,
  `Away Rating` = Away_Rating,
  `Predicted Winner` = Prediction,
  Confidence = paste0(round(100*Confidence, 2), "%"),
  `Win in 3` = paste0(round(100*Win3, 2), "%"),
  `Win in 4` = paste0(round(100*Win4, 2), "%"),
  `Win in 5` = paste0(round(100*Win5, 2), "%"),
  `Lose in 3` = paste0(round(100*Lose3, 2), "%"),
  `Lose in 4` = paste0(round(100*Lose4, 2), "%"),
  `Lose in 5` = paste0(round(100*Lose5, 2), "%"),
  `Prediction Date` = as.Date(ymd(sunday_date) + ddays(1))
) %>%
  arrange(Date, `Home Rank`, `Away Rank`)

predictions_file <- paste0("C:/Users/dpwyn/Documents/Volleyball Analytics/BTVB/predictions_", sunday_date, ".csv")
write_csv(games_predictions_final, predictions_file)
