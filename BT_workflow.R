library(tidyverse)

### Start by getting all the teams - team_mapping() gets all teams so we have to filter
NCAA_teams <- team_mapping() %>% filter(Conference %in%
                                          c("Pac-12",
                                            "Big 12",
                                            "Big Ten",
                                            "ACC",
                                            "SEC",
                                            "Big East",
                                            "AAC",
                                            "America East",
                                            "ASUN",
                                            "Atlantic 10",
                                            "Big Sky",
                                            "Big West",
                                            "Big South",
                                            "CAA",
                                            "C-USA",
                                            "Horizon",
                                            "Ivy League",
                                            "MAAC",
                                            "MAC",
                                            "MEAC",
                                            "Mountain West",
                                            "MVC",
                                            "NEC",
                                            "OVC",
                                            "Patriot",
                                            "SoCon",
                                            "Southland",
                                            "Summit League",
                                            "Sun Belt",
                                            "SWAC",
                                            "WAC",
                                            "WCC"))


# Use the map2 function to get all the teams' schedules
NCAA_all <- map2(NCAA_teams$Name, NCAA_teams$ID, get_team_schedule, sport = "Women's Volleyball", year = 2022)

# Lots of warnings will pop up because several teams do not support Women's Volleyball

# Now we need to convert NCAA_all to a data frame and fix some issues
NCAA_games <- bind_rows(NCAA_all)

NCAA_all_games <- NCAA_games[-which(duplicated(NCAA_games$url)),] %>% filter(!is.na(url)) # remove duplicated games

NCAA_all_games2 <- NCAA_all_games %>% mutate(
  home = str_replace(home, "\\\\u0026", "&"),
  away = str_replace(away, "\\\\u0026", "&"),
  location = str_replace(location, "\\\\u0026", "&"),
)

# this code is optional
# if you only want games in a certain range (e.g., you're just updating a database or want regular season only),
# then run this code and replace "NCAA_all_games" with "NCAA_new_games" in the code afterwards
library(lubridate)
last_sunday_date <- "2021-09-26"
sunday_date <- "2021-10-03"
NCAA_new_games <- NCAA_all_games %>% filter(date > ymd(last_sunday_date), date <= ymd(sunday_date))


# Now we get the box score and play-by-play links
pbp_box_urls <- map_df(NCAA_all_games$url, pbp_boxscore_links)
NCAA_game_pbp <- bind_cols(NCAA_all_games, pbp_box_urls) %>% mutate(game_id = str_remove(play_by_play, "https://stats.ncaa.org/game/play_by_play/"))

NCAA_pbp_url <- NCAA_game_pbp$play_by_play[which(!is.na(NCAA_game_pbp$play_by_play))]


# Now get the play-by-play data

# vb_play_by_play is a wrapper function because NCAA is not consistent in how they markup the play-by-play
all_NCAA_pbp <- map(NCAA_pbp_url, vb_play_by_play)


# Now we create the play-by-play data frame
all_NCAA_pbp_df <- bind_rows(all_NCAA_pbp) %>% fill(away_score, home_score, .direction= "down") %>%
  filter(player_name != "Set end")
all_NCAA_pbp_df <- all_NCAA_pbp_df %>% mutate(id = seq(1, nrow(all_NCAA_pbp_df)))

# Now that we have all the play-by-play we remove some of the weirdness in the team name scraping
NCAA_team_regex <- NCAA_teams$Name %>% str_replace("\\(", "\\\\(") %>% str_replace("\\)", "\\\\)") %>% str_replace_all("\\.", "\\\\.")

## Old games, then bind
old_pbp_file <- paste0("pbp_", last_sunday_date, ".csv")
old_pbp <- read_csv(old_pbp_file)

NCAA_pbp_full <- all_NCAA_pbp_df %>% 
  left_join(NCAA_game_pbp %>% select(game_id, location, box_score, play_by_play), by = c("ncaa_match_id" = "game_id")) %>%
  mutate(ncaa_match_id = as.numeric(ncaa_match_id))

full_pbp <- bind_rows(old_pbp, NCAA_pbp_full)
full_pbp <- full_pbp %>% mutate(id = seq(1, nrow(full_pbp)))

new_pbp_file <- paste0("pbp_", sunday_date, ".csv")

write_csv(full_pbp, new_pbp_file)

old_box_file <- paste0("box_info_", last_sunday_date, ".csv")
old_box <- read_csv(old_box_file)

box_any_na <- old_box %>% filter(is.na(play_by_play))
box_missing_check <- box_any_na$box_score %>% str_replace("box_score", "play_by_play")

all_missing_old_pbp <- map(box_missing_check, vb_play_by_play)


# this takes care of all the ridiculous weird nonsense from people who can't spell team names correctly in the play-by-play
# it also filter so we only have serves
a2 <- full_pbp %>% filter(skill == "Serve") %>%
  mutate(serving_team = fix_NCAA_names(serving_team),
         team = fix_NCAA_names(team),
         point_won_by = fix_NCAA_names(point_won_by)) %>%
  mutate(serving_team = str_extract(serving_team, pattern = paste(NCAA_team_regex[order(-nchar(NCAA_team_regex))], collapse = "|")),
         opponent = if_else(serving_team == home_team, away_team, home_team),
         opponent = str_extract(opponent, pattern = paste(NCAA_team_regex[order(-nchar(NCAA_team_regex))], collapse = "|")))

# Now we create the serve summary data frame
a3 <- a2 %>% left_join(NCAA_all_games2, by = c("home_team" = "home", "away_team" = "away", "location")) %>%
  group_by(ncaa_match_id, serving_team, opponent) %>% 
  summarize(serves = sum(!is.na(point_won_by)), 
            points = sum(point_won_by == serving_team, na.rm = TRUE),
            serving_home = first(case_when(
              team == location ~ 1,
              opponent == location ~ -1,
              TRUE ~ 0
            ))) %>%
  mutate(bp_pct = points/serves, n_SO = serves - points) %>%
  ungroup() %>% group_by(serving_team, opponent, serving_home) %>%
  summarize(n_BP = sum(points), n_SO = sum(n_SO)) %>%
  filter(!is.na(serving_team), !is.na(opponent))


# Now we have to set up the data to run the Bradley-Terry model
serving_team2 <- a3 %>% ungroup() %>% select(serving_team, serving_home) %>% rename(team = serving_team) %>% 
  mutate(serving = 1,
         home = as.numeric(serving_home),
         team = as.factor(team))
receiving_team2 <- a3 %>% ungroup() %>% select(opponent, serving_home) %>% rename(team = opponent) %>% 
  mutate(serving = 0,
         home = -as.numeric(serving_home),
         team = as.factor(team))
  
BT_outcomes2 <- as.matrix(a3 %>% ungroup() %>% select(n_BP, n_SO))

# Run the model
library(BradleyTerry2)
BT_model <- BTm(outcome = BT_outcomes2, player1 = serving_team2, player2 = receiving_team2,
             formula  = ~ team + serving + home, id = "team")

# Now everything is about scaling the ratings properly

n <- length(coef(BT_model))
serve_abilities2 <- c(0, coef(BT_model)[-c(n, n-1)])
serve_adj2 <- coef(BT_model)[(n-1)]
serve_home2 <- coef(BT_model)[n]

names(serve_abilities2) <- c("A&M-Corpus Christi", stringr::str_remove(names(serve_abilities2)[-1], "team"))

serve_abilities_scaled <- (serve_abilities2 - mean(serve_abilities2))/sd(serve_abilities2) * 500 + 1500

scaling_factor <- 500/sd(serve_abilities2)

serve1 <- c(serve_abilities_scaled %>% sort(decreasing = TRUE) %>% round(0), round(serve_adj2/sd(serve_abilities2)*500, 0), round(serve_home2/sd(serve_abilities2)*500,0), round(scaling_factor, 0))
serve2 <- c(serve_abilities2 %>% sort(decreasing = TRUE), serve_adj2, serve_home2, 1)

# This is a cool thing to estimate how teams would serve against each opponent on neutral court
serve_matrix2 <- matrix(0, nrow = length(serve_abilities2), ncol = length(serve_abilities2))

for(i in 1:length(serve_abilities2)){
  for( j in 1:length(serve_abilities2))
    serve_matrix2[i,j] <- exp(serve_abilities2[i] - serve_abilities2[j] + serve_adj2)/(1 + exp(serve_abilities2[i] - serve_abilities2[j] + serve_adj2))
}

colnames(serve_matrix2) <- names(serve_abilities2)
rownames(serve_matrix2) <- names(serve_abilities2)

# This creates the dataframe

serve_df <- data.frame(Rank = c(seq(1, length(serve1)-3), NA_real_, NA_real_, NA_real_), Team = names(serve1), Rating = serve1, Rating_Raw = serve2)
serve_df$Team[length(serve1)-2] <- "Serve Adjustment"
serve_df$Team[length(serve1)-1] <- "Home Court Adjustment"
serve_df$Team[length(serve1)] <- "Scaling Factor"

serve_df %>% left_join(NCAA_teams %>% mutate(Name = str_replace(Name, "\\\\u0026", "&")) %>% select(Name, Conference), by = c("Team" = "Name")) %>%
  select(Rank, Team, Conference, Rating, Rating_Raw) -> serve_df2

new_ratings_df <- paste0("ratings_", sunday_date, ".csv")
write_csv(serve_df2, new_ratings_df)

## Now boxscore stuff
NCAA_box_url <- NCAA_game_pbp$box_score[which(!is.na(NCAA_game_pbp$box_score))]

all_NCAA_box <- suppressWarnings(map(NCAA_box_url, vb_boxscore))

NCAA_box_df <- (all_NCAA_box %>% transpose())$info %>% bind_rows()

a4 <- NCAA_box_df %>% left_join((NCAA_game_pbp %>% mutate(
  home = str_replace(home, "\\\\u0026", "&"),
  away = str_replace(away, "\\\\u0026", "&"),
  location = str_replace(location, "\\\\u0026", "&")
)) %>% select(game_id, location, box_score, play_by_play), by = c("game_id"))

a4_winner <- a4 %>% mutate(winner = if_else(away_sets > home_sets, away, home),
                           sets = away_sets + home_sets)

all_box_file <- paste0("C:/Users/dpwyn/Documents/Volleyball Analytics/BTVB/box_info_",sunday_date, ".csv")

sets_all <- bind_rows(old_box, a4_winner %>% mutate(game_id = as.numeric(game_id)))

write_csv(sets_all, all_box_file)



new_box <- bind_rows(old_box, NCAA_box_df)
