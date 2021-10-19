## Step 0: Load the necessary packages

library(tidyverse) # umbrella load several packages for data manipulation
library(httr)
library(rvest) # httr and rvest should cover all the web-scraping needs
library(BradleyTerry2)  # to run the model
library(volleysim)  # to run the match simulation

######## Step 1: Get the list of NCAA teams #########

#### Option 1a: use the team_mapping function to grab the list of NCAA teams ####

team_mapping <- function(){
  # get all the team names and corresponding IDs from the NCAA website
  
  # Step 1: get the giant character vector that matches teams and id
  url <- "https://stats.ncaa.org/team/search"
  all_teams_messy <- read_html(url) %>% html_node("body") %>% html_text()
  
  # Step 2: split the character vector
  all_teams_matrix <- str_split(all_teams_messy, ",") %>% unlist() %>% 
    matrix(ncol = 4, byrow = T)
  
  teams_and_conferences <- str_split_fixed(all_teams_matrix[,3], " - ", 2)
  
  team_names <- str_remove_all(teams_and_conferences[,1], '\\"label\\"\\:\\"') %>% str_remove_all('\\[') %>%
    str_replace_all('\\\\u0026', '&')
  team_conferences <- str_remove_all(teams_and_conferences[,2], '\\"')
  team_ids <- str_extract(all_teams_matrix[,2], "\\d+")
  
  # Step 3: reconstitute the teams/ids as a tibble
  teams_df <- tibble(
    Name = team_names,
    Conference = team_conferences,
    ID = team_ids
  )
}

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


#### Option 1b: store the list of teams in an Excel file and import ####
# NCAA_teams <- read_excel("NCAA_teams.xlsx") # for Excel file
# OR
# NCAA_teams <- read_csv("NCAA_teams.csv") # for CSV file

######## Step 2: Get the list of games #########

get_team_schedule <- function(team_name, team_id, sport, year){
  
  # This function is to get all of the games a team has played that the NCAA website has links to results
  # team_name is the name of the team on the NCAA website
  # team_id is the id of the team on the NCAA website
  # generally we get team_name and team_id from the team_mapping() function
  # sport is the name of the sport as written on the NCAA website
  # year is the academic year; note that for fall sports you need to add 1 to the sport year
  
  # Step 1: figure out if the sport exists  
  url <- paste0("https://stats.ncaa.org/team/", team_id)
  
  schedule_links <- read_html(url) %>% html_nodes("a")
  
  schedule_link_detect <- html_text(schedule_links) %>% str_detect(sport)
  
  if(!any(schedule_link_detect)){
    warning(paste0("Cannot find the requested sport.\nMake sure the sport is spelled and capitalized correctly and that ", team_name, " currently supports the sport."))
    url2 <- NA
  } else {
    schedule_link <- html_attr(schedule_links, "href")[schedule_link_detect]
    
    url2 <- paste0("https://stats.ncaa.org", schedule_link)
  }
  
  # Step 2: see if the team supported the sport in that year
  if(is.numeric(year)){  # if year is numeric, have to convert to academic year
    year <- paste0(year-1, "-", year %% 100)
  }
  
  if(!is.na(url2)){
    year_options <- read_html(url2) %>% html_nodes("option")
    
    year_detect <- html_text(year_options) %>% str_detect(year)
    
    if(!any(year_detect)){
      warning(paste("Cannot find the requested year.\nMake sure", team_name, "supported that sport in that year."))
      url3 <- NA
    } else {
      year_link <- html_attr(year_options, "value")[year_detect]
      
      url3 <- paste0("https://stats.ncaa.org/teams/", year_link)  # finally to the team
    }
    
  } else{
    url3 <- NA
  }
  
  # Step 3: find all games played by that team in that sport in that year
  if(!is.na(url3)){
    games <- read_html(url3) %>% html_nodes(".skipMask")
    
    games_detect <- html_attr(games, "target") == "BOX_SCORE_WINDOW"
    
    if(!any(games_detect)){
      warning(paste0(team_name, " does not appear to have played any games in ", year, "."))
      games_url <- NA_character_
      games_date <- NA_character_
      games_home <- NA_character_
      games_away <- NA_character_
      games_location <- NA_character_
      
    } else {
      games_links <- html_attr(games, "href")[games_detect]
      
      games_url <- paste0("https://stats.ncaa.org", games_links)
      
      ## The below three lines filter the table of games to include only matches with a link to the box score - other games are not counted
      games_table_table <- read_html(url3) %>% html_nodes("table:nth-child(2)")
      games_with_links <- which(str_detect(as.character(games_table_table %>% html_nodes("tr")), "contests"))
      games_table <- (games_table_table %>% html_table())[[1]][games_with_links - 1,]
      
      # Extract information about each game
      games_date <- games_table[[1]]
      games_home <- case_when(
        str_detect(games_table[[2]], "^\\@") ~ str_remove(games_table[[2]], "\\@ "),
        str_detect(games_table[[2]], ".+\\@")~ str_remove(games_table[[2]], "\\@.+") %>% str_trim(),
        TRUE ~ team_name)
      games_away <- if_else(str_detect(games_table[[2]], "@"), team_name, games_table[[2]])
      
      # The tricky part is to use regex to get the correct location for away and neutral games
      games_location <- case_when(
        str_detect(games_table[[2]], "\\@(?=[^ ])") ~ str_extract(games_table[[2]], "\\@(?=[^ ]).*") %>% str_remove("\\@"),
        str_detect(games_table[[2]], "\\@") ~ str_extract(games_table[[2]], "\\@.*$") %>% str_remove("\\@") %>% str_trim(),
        TRUE ~ team_name
      )
    }
    
  } else {
    games_url <- NA_character_
    games_date <- NA_character_
    games_home <- NA_character_
    games_away <- NA_character_
    games_location <- NA_character_
    
  }
  
  # Once we've matched as many games as we can, create the data frame
  games_df <- tibble(
    url = games_url,
    date = lubridate::parse_date_time(games_date, orders = c("m/d/Y", "m/d/Y(h)")),
    home = games_home,
    away = games_away,
    location = games_location
  )
  
  return(games_df)
}

# You have to run the line below every time because the NCAA website created new links to games recently played and this function finds the links

NCAA_all <- map2(NCAA_teams$Name, NCAA_teams$ID, get_team_schedule, sport = "Women's Volleyball", year = 2022)

# Lots of warnings will pop up because several teams do not support Women's Volleyball

NCAA_games <- bind_rows(NCAA_all)  # turns it into a data frame

NCAA_all_games <- NCAA_games[-which(duplicated(NCAA_games$url)),] %>% filter(!is.na(url)) # remove duplicated games

###### Step 3: Get the information for the games ######

pbp_boxscore_links <- function(url){
  all_links <- read_html(url) %>% html_nodes("a")
  box_score_link <- html_attr(all_links, "href")[str_detect(html_text(all_links), "Box Score") & (html_attr(all_links, "href") != "#")]
  pbp_link <- html_attr(all_links, "href")[str_detect(html_text(all_links), "Play by Play")]
  
  if(length(pbp_link) == 1){
    return_links <- paste0("https://stats.ncaa.org", c(box_score_link, pbp_link))
  } else {
    return_links <- c(paste0("https://stats.ncaa.org", box_score_link), NA_character_)
  }
  
  names(return_links) <- c("box_score", "play_by_play")
  
  return(return_links)
}


#### Option 3a: Get the information for all games in the dataset ####

pbp_box_urls <- map_df(NCAA_all_games$url, pbp_boxscore_links)
NCAA_game_pbp <- bind_cols(NCAA_all_games, pbp_box_urls) %>% mutate(game_id = str_remove(play_by_play, "https://stats.ncaa.org/game/play_by_play/"))

NCAA_pbp_url <- NCAA_game_pbp$play_by_play[which(!is.na(NCAA_game_pbp$play_by_play))]  # remove any missing play-by-play links

#### Option 3b: Get the information for only the new games in the dataset ####

## Generally this option works better if you already have a dataset of play-by-play for previous games

#library(lubridate)
#last_sunday_date <- "2021-10-03"
#sunday_date <- "2021-10-10"
#NCAA_new_games <- NCAA_all_games %>% filter(date > ymd(last_sunday_date), date <= ymd(sunday_date))  # finds only the new games
#pbp_box_urls <- map_df(NCAA_all_games$url, pbp_boxscore_links)
#NCAA_game_pbp <- bind_cols(NCAA_all_games, pbp_box_urls) %>% mutate(game_id = str_remove(play_by_play, "https://stats.ncaa.org/game/play_by_play/"))
#NCAA_pbp_url <- NCAA_game_pbp$play_by_play[which(!is.na(NCAA_game_pbp$play_by_play))]


###### Step 4: Get the play-by-play data for each (new) game ######

fix_NCAA_names <- function(x){
  case_when(x == "Americanan" ~ "American",
            x == "N.C. AT" ~ "N.C. A&T",
            str_detect(x, "Alabama AM") ~ "Alabama A&M",
            str_detect(x, "Texas AM") ~ "Texas A&M",
            str_detect(x, "Florida AM") ~ "Florida A&M",
            x == "LMU (CA) (CA)" | x == "LMU" ~ "LMU (CA)",
            x == "Binghamtonmton" ~ "Binghamton",
            x == "Saint Marys" | x == "St. Marys" ~ "Saint Mary's (CA)",
            x == "Gardner" ~ "Gardner-Webb",
            x == "St. Johns" ~ "St. John's (NY)",
            x == "William Mary" ~ "William & Mary",
            x == "Bethune" ~ "Bethune-Cookman",
            x == "Saint Francis" ~ "Saint Francis (PA)",
            x == "Saint Peters" ~ "Saint Peter's",
            TRUE ~ x)
} # This function fixes every team naming issue I've seen in the 2021 (spring and fall) data

vb_play_by_play <- function(pbp_url){
  
  pbp1 <- vb_play_by_play_ncaa(pbp_url)
  
  if(is.null(pbp1) || nrow(pbp1) == 0) {  # some weirdness with nulls
    pbp2 <- vb_play_by_play_ncaa2(pbp_url) # if pbp1 doesn't work try pbp2
    if(is.null(pbp2) || nrow(pbp2) == 0){
      return(NULL)  # if neither works return NULL
    } else {
      return(pbp2)
    }
  } else {
    return(pbp1)
  }
}  # Wrapper function because NCAA is not consistent with how the play-by-play is formatted

# This is the cleaner way the play-by-play is formatted
vb_play_by_play_ncaa <- function(pbp_url){
  
  game_info <- read_html(pbp_url) %>% html_nodes(".mytable") %>% html_table(fill = TRUE)
  match_id <- str_remove(pbp_url, "https://stats.ncaa.org/game/play_by_play/")
  
  sets <- bind_rows(game_info[-c(1:2)])
  
  if(nrow(sets) == 0){
    return(NULL)
  }   ## return NULL if there are no sets in the file, I'm hoping this fixes the issues
  
  teams <- c(sets$X1[1], sets$X3[1]) %>% str_squish()
  
  skills_regex <- "serve|Reception|Block|Set|Attack|Freeball|Dig"
  skills_removed <- " serves|Reception by |Block by |Set by |Attack by |Dig by "
  
  ## Step 1: Add set number, skill type, away and home scores, serving team  
  sets <- sets %>% mutate(
    set_number = cumsum(X1 == "Set started") + cumsum(X3 == "Set started"),
    skill = if_else(
      nchar(X1) > 0, str_extract(X1, skills_regex), str_extract(X3, skills_regex)
    ) %>% str_to_title(),
    team = if_else(
      nchar(X1) > 0, X1[1], X3[1]
    ),
    player_name = if_else(
      nchar(X1) > 0, str_remove(X1, skills_removed), str_remove(X3, skills_removed)
    ) %>% str_squish(),
    away_score = suppressWarnings(as.numeric(str_split_fixed(X2, "-", 2)[,1])),  ## Warning here since Score will be non-numeric
    home_score = suppressWarnings(as.numeric(str_split_fixed(X2, "-", 2)[,2])),
    serving_team = if_else(skill == "Serve", team, NA_character_),
    away_team = teams[1],
    home_team = teams[2],
    opponent = if_else(team == away_team, home_team, away_team)
  )
  
  # Step 2: add point won by and fill in all the missing information
  sets <- sets %>% tidyr::fill(away_score, home_score, .direction = "up") %>%
    mutate(point_won_by = case_when(
      away_score == 1 & home_score == 0 ~ X1[1],
      away_score == 0 & home_score == 1 ~ X3[1],
      away_score == (lag(away_score) + 1) ~ X1[1],
      home_score == (lag(home_score) + 1) ~ X3[1],
      TRUE ~ NA_character_
    )
    ) %>% tidyr::fill(point_won_by, serving_team, .direction = "down")
  
  # Step 3: filter to get skills  
  skills <- sets %>% filter(!is.na(skill), X1 != "Set started", X3 != "Set started", !str_detect(X1, "\\+|End of"), !str_detect(X3, "\\+|End of")) %>% select(-X1, -X2, -X3) %>%
    mutate(point_id = cumsum(skill == "Serve"),
           ncaa_match_id = match_id)
  
  return(skills)
}

# This is the messier way
vb_play_by_play_ncaa2 <- function(pbp_url){
  
  game_info <- read_html(pbp_url) %>% html_nodes(".mytable") %>% html_table(fill = TRUE)
  match_id <- str_remove(pbp_url, "https://stats.ncaa.org/game/play_by_play/")
  
  sets <- bind_rows(game_info[-c(1:2)])
  
  if(nrow(sets) == 0){
    return(NULL)
  }   ## return NULL if there are no sets in the file, I'm hoping this fixes the issues
  
  teams <- str_split(sets$X1[1], "-") %>% unlist() %>% str_squish() %>% fix_NCAA_names()
  # Problem here: if team has hyphenated name, we may not get the correct split
  
  set_starters <- list(starters_away = character((length(game_info) - 2)),
                       starters_home = character((length(game_info) - 2))
  )
  for(i in 3:length(game_info)){
    set_starters[[1]][(i-2)] <- game_info[[i]]$X2[2] %>% str_replace_all("3a", " ")
    set_starters[[2]][(i-2)] <- game_info[[i]]$X2[3] %>% str_replace_all("3a", " ")
  }
  set_starters <- unlist(set_starters)
  
  starters_away <- paste(set_starters[str_detect(set_starters, teams[1])], collapse = " ")
  starters_home <- paste(set_starters[str_detect(set_starters, teams[2])], collapse = " ")
  
  
  ## Step 1: Add set number, skill type, away and home scores, serving team  
  sets <- sets %>% filter(nchar(X1) > 0) %>% 
    mutate(
      set_number = cumsum(str_detect(X1, "End of")) + 1,
      skill = "Serve",
      player_name = str_extract(X2, ": \\(.+\\)\\s") %>% str_remove(": \\(") %>% str_remove("\\)") %>% str_squish(),
      away_score = suppressWarnings(as.numeric(str_split_fixed(X1, "-", 2)[,1] %>% str_squish())),
      home_score = suppressWarnings(as.numeric(str_split_fixed(X1, "-", 2)[,2] %>% str_squish())),
      point_won_by = str_extract(X2, "Point .+:") %>% str_remove("Point ") %>% str_remove(":") %>% str_remove(" \\(.+"),
      serving_team = lag(point_won_by),
      serving_team = if_else(
        is.na(serving_team),
        case_when(
          str_detect(starters_away, player_name) ~ teams[1],
          str_detect(starters_home, player_name) ~ teams[2],
          TRUE ~ NA_character_
        ),
        serving_team),
      serving_team = if_else(is.na(serving_team), 
                             first(serving_team[which(player_name == eval(player_name))] %>% na.omit()), 
                             serving_team),  # yes, we need 3 separate mutates here to deal with wackiness
      serving_team = fix_NCAA_names(serving_team),
      team = serving_team,
      point_won_by = fix_NCAA_names(point_won_by),
      away_team = teams[1],
      home_team = teams[2],
      opponent = if_else(team == away_team, home_team, away_team)
    ) %>%
    filter(!is.na(away_score))
  # Step 3: filter to get skills  
  
  skills <- sets %>% select(-X1, -X2, -X3) %>%
    mutate(point_id = cumsum(skill == "Serve"),
           ncaa_match_id = match_id)
  
  return(skills)
}


# Note: there may be some problems here with vb_play_by_play if a play-by-play exists but isn't formatted in either expected way
all_NCAA_pbp <- map(NCAA_pbp_url, vb_play_by_play)

all_NCAA_pbp_df <- bind_rows(all_NCAA_pbp) %>% fill(away_score, home_score, .direction= "down") %>%
  filter(!(player_name %in% c("Set end", "Set ended")))
all_NCAA_pbp_df <- all_NCAA_pbp_df %>% mutate(id = seq(1, nrow(all_NCAA_pbp_df)))

# Now we merge with the data frame containing the links
full_pbp <- all_NCAA_pbp_df %>% 
  left_join(NCAA_game_pbp %>% select(game_id, location, box_score, play_by_play), by = c("ncaa_match_id" = "game_id")) %>%
  mutate(ncaa_match_id = as.numeric(ncaa_match_id))


#### Only for people storing play-by-play files every week ####

#old_pbp_file <- paste0("pbp_", last_sunday_date, ".csv")
#old_pbp <- read_csv(old_pbp_file)
#full_pbp <- bind_rows(old_pbp, full_pbp)
#full_pbp <- full_pbp %>% mutate(id = seq(1, nrow(full_pbp)))
#new_pbp_file <- paste0("pbp_", sunday_date, ".csv")
#write_csv(full_pbp, new_pbp_file)

##### Step 5: Filter the play-by-play to only look at serves ######

# Now that we have all the play-by-play we remove some of the weirdness in the team name scraping
NCAA_team_regex <- NCAA_teams$Name %>% str_replace("\\(", "\\\\(") %>% str_replace("\\)", "\\\\)") %>% str_replace_all("\\.", "\\\\.")

# First we filter to just get the skills and fix the naming issues
a2 <- full_pbp %>% filter(skill == "Serve") %>%
  mutate(serving_team = fix_NCAA_names(serving_team),
         team = fix_NCAA_names(team),
         point_won_by = fix_NCAA_names(point_won_by)) %>%
  mutate(serving_team = str_extract(serving_team, pattern = paste(NCAA_team_regex[order(-nchar(NCAA_team_regex))], collapse = "|")),
         opponent = if_else(serving_team == home_team, away_team, home_team),
         opponent = str_extract(opponent, pattern = paste(NCAA_team_regex[order(-nchar(NCAA_team_regex))], collapse = "|")))

NCAA_all_games2 <- NCAA_all_games %>% mutate(
  home = str_replace(home, "\\\\u0026", "&"),
  away = str_replace(away, "\\\\u0026", "&"),
  location = str_replace(location, "\\\\u0026", "&"),
)  # fix the & issues in the team names in NCAA_all_games

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

###### Step 6: Set up the Bradley-Terry Model ######

serving_team2 <- a3 %>% ungroup() %>% select(serving_team, serving_home) %>% rename(team = serving_team) %>% 
  mutate(serving = 1,
         home = as.numeric(serving_home),
         team = as.factor(team))
receiving_team2 <- a3 %>% ungroup() %>% select(opponent, serving_home) %>% rename(team = opponent) %>% 
  mutate(serving = 0,
         home = -as.numeric(serving_home),
         team = as.factor(team))

BT_outcomes2 <- as.matrix(a3 %>% ungroup() %>% select(n_BP, n_SO))

###### Step 7: Run the Model ######
BT_model <- BTm(outcome = BT_outcomes2, player1 = serving_team2, player2 = receiving_team2,
                formula  = ~ team + serving + home, id = "team")

###### Step 8: Shrink the Coefficients ######

# We have found that this model consistently overestimates every team's sideout percentage
# We shrink the coefficients toward the mean so that the model will predict that when two equal teams play each other on neutral court,
# Each team will sideout at the average rate found in the dataset

n <- length(coef(BT_model))
serve_adj2 <- coef(BT_model)[(n-1)]  # raw (biased) serve adjustment

b_bar <- mean(coef(BT_model))  # average coefficient value

serve_prop <- mean(a2$serving_team == a2$point_won_by, na.rm = TRUE)  # true point scoring percentage for the whole dataset
serve_log <- log(serve_prop) - log(1 - serve_prop)

B <- (serve_log - b_bar)/(serve_adj2 - b_bar)
coef_shrunk <- b_bar + B*(coef(BT_model) - b_bar)

serve_abilities_shrunk <- c(b_bar*(1 - B), coef_shrunk[-c(n, n-1)])  # remove the adjustments for serve and home court

names(serve_abilities_shrunk) <- c("A&M-Corpus Christi", stringr::str_remove(names(serve_abilities_shrunk)[-1], "team"))

serve_adj_shrunk <- coef_shrunk[(n-1)]  # Shrunk serve adjustment
serve_home_shrunk <- coef_shrunk[n] # Shrunk home-court adjustment

# This appears to be overly conservative shrinking
# Preliminary results indicate that we tend to overestimate the sideout rate for underdogs
# And underestimate the sideout rate for favorites
# We're going to roll with this until I code a more accurate shrinkage method

###### Step 9: Scale the Coefficients ######

serve_abilities_scaled <- (serve_abilities_shrunk - mean(serve_abilities_shrunk))/sd(serve_abilities_shrunk) * 500 + 1500

scaling_factor <- 500/sd(serve_abilities_shrunk)

# Not sure if this shrinks the home court advantage too much, may play around with it a bit more
serve1 <- c(serve_abilities_scaled %>% sort(decreasing = TRUE) %>% round(0), round(serve_adj_shrunk/sd(serve_abilities_shrunk)*500, 0), round(serve_home_shrunk/sd(serve_abilities_shrunk)*500,0), round(scaling_factor, 0))
serve2 <- c(serve_abilities_shrunk %>% sort(decreasing = TRUE), serve_adj_shrunk, serve_home_shrunk, 1)

serve_df <- data.frame(Rank = c(seq(1, length(serve1)-3), NA_real_, NA_real_, NA_real_), Team = names(serve1), Rating = serve1, Rating_Raw = serve2)
serve_df$Team[length(serve1)-2] <- "Serve Adjustment"
serve_df$Team[length(serve1)-1] <- "Home Court Adjustment"
serve_df$Team[length(serve1)] <- "Scaling Factor"

serve_df2 <- serve_df %>% left_join(NCAA_teams %>% mutate(Name = str_replace(Name, "\\\\u0026", "&")) %>% select(Name, Conference), by = c("Team" = "Name")) %>%
  select(Rank, Team, Conference, Rating, Rating_Raw)

#### Only if you want to save the ratings for future predictions/evaluations ####
#library(lubridate)
#sunday_date <- "2021-10-10"
#new_ratings_df <- paste0("ratings_", sunday_date, ".csv")
#write_csv(serve_df2, new_ratings_df)

###### Step 11: Find the games to predict ######

get_team_future_schedule <- function(team_name, team_id, sport, year){
  
  # This function is to get all of the games a team has played that the NCAA website lists, whether or not there are results
  # This is good for getting the future games in the current year
  # team_name is the name of the team on the NCAA website
  # team_id is the id of the team on the NCAA website
  # generally we get team_name and team_id from the team_mapping() function
  # sport is the name of the sport as written on the NCAA website
  # year is the academic year; note that for fall sports you need to add 1 to the sport year
  
  url <- paste0("https://stats.ncaa.org/team/", team_id)
  
  schedule_links <- read_html(url) %>% html_nodes("a")
  
  schedule_link_detect <- html_text(schedule_links) %>% str_detect(sport)
  
  if(!any(schedule_link_detect)){
    warning(paste0("Cannot find the requested sport.\nMake sure the sport is spelled and capitalized correctly and that ", team_name, " currently supports the sport."))
    url2 <- NA
  } else {
    schedule_link <- html_attr(schedule_links, "href")[schedule_link_detect]
    
    url2 <- paste0("https://stats.ncaa.org", schedule_link)
  }
  
  if(is.numeric(year)){  # if year is numeric, have to convert to academic year
    year <- paste0(year-1, "-", year %% 100)
  }
  
  if(!is.na(url2)){
    year_options <- read_html(url2) %>% html_nodes("option")
    
    year_detect <- html_text(year_options) %>% str_detect(year)
    
    if(!any(year_detect)){
      warning(paste("Cannot find the requested year.\nMake sure", team_name, "supported that sport in that year."))
      url3 <- NA
    } else {
      year_link <- html_attr(year_options, "value")[year_detect]
      
      url3 <- paste0("https://stats.ncaa.org/teams/", year_link)  # finally to the team
    }
    
  } else{
    url3 <- NA
  }
  
  if(!is.na(url3)){
    games <- read_html(url3) %>% html_nodes("table")
    
    games_table <- games[2] %>% html_table() %>% .[[1]] %>% filter(nchar(Opponent) > 0)
    
    if(nrow(games_table) == 0){
      warning(paste0(team_name, " does not appear to have played any games in ", year, "."))
      games_df <- NULL
      
    } else {
      
      games_df <- games_table %>%
        mutate(Location = case_when(
          !str_detect(Opponent, "\\@") ~ team_name,
          str_detect(Opponent, "\\@ ") ~ str_trim(str_remove(Opponent, "\\@ ")),
          str_detect(Opponent, "\\@(?=[^ ])") ~ str_trim(str_extract(Opponent, "\\@(?=[^ ]).*") %>% str_remove("\\@"))
        ),
        Opponent = case_when(
          !str_detect(Opponent, "\\@") ~ str_trim(Opponent),
          str_detect(Opponent, "\\@ ") ~ str_trim(str_remove(Opponent, "\\@ ")),
          str_detect(Opponent, "\\@(?=[^ ])") ~ str_trim(str_remove(Opponent, "\\@(?=[^ ]).*"))
        ),
        Date = lubridate::parse_date_time(Date, orders = c("m/d/Y H:M p", "m/d/Y", "m/d/Y(H)"))
        )
      
      games_df <- games_df %>% mutate(Team = team_name,
                                      Attendance = if_else(is.na(as.numeric(str_remove(Attendance, ","))), NA_real_, as.numeric(str_remove(Attendance, ",")))) %>% 
        select(Date, Team, Opponent, Result, Attendance, Location)
      
    }
    
  } else {
    games_df <- NULL
  }
  
  return(games_df)
}

all_games <- map2(NCAA_teams$Name, NCAA_teams$ID, get_team_future_schedule, sport = "Women's Volleyball", year = 2022)

prediction_date <- "2021-10-17"
games_week <- all_games %>% bind_rows() %>% filter(Date >= (ymd(prediction_date) + ddays(1)), Date < (ymd(prediction_date) + ddays(8)))
## Need to add 1-8 days because it counts duration from 12 midnight on the prediction date

###### Step 12: Match each game's teams to their ratings ######

current_ratings_teams <- serve_df2 %>% select(Team, Rank, Rating)

## If you are running this code straight through

## If instead you have your ratings saved in a csv file and started a new session at Step 11:
#sunday_date <- "2021-10-10"
#current_ratings_file <- paste0("ratings_", sunday_date, ".csv")
#current_ratings <- read_csv(current_ratings_file)
#current_ratings_teams <- current_ratings %>% select(Team, Rank, Rating)

games_predictions <- games_week %>% mutate(
  away = if_else(Opponent != Location, Opponent, Team),
  home = if_else(Opponent == Location, Opponent, Team)
) %>% left_join(
  current_ratings_teams, by = c("home" = "Team")
) %>% rename(Home_Rating = Rating, Home_Rank = Rank) %>%
  left_join(
    current_ratings_teams, by = c("away" = "Team")
  ) %>% rename(Away_Rating = Rating, Away_Rank = Rank) %>%
  mutate(
    pred_id = paste0(Date, Location, apply(games_predictions %>% select(away, home), 1,
                    function(x) paste(str_sort(c(x[1], x[2])), collapse = "")
                      )
                    )  # there's probably a better way to do this but it works for now
  )

games_predictions <- games_predictions[!duplicated(games_predictions$pred_id),] %>% select(-pred_id)

###### Step 13: Predict the sideout rates for each team ######

scale_factor <- current_ratings_teams$Rating[current_ratings_teams$Team == "Scaling Factor"]
serve_adj <- current_ratings_teams$Rating[current_ratings_teams$Team == "Serve Adjustment"]
home_adj <- current_ratings_teams$Rating[current_ratings_teams$Team == "Home Court Adjustment"]

games_predictions2 <- games_predictions %>% mutate(
  Home_Adj = home_adj*(home == Location),
  away_sideout = 1/(1 + exp((Home_Rating - Away_Rating + serve_adj + Home_Adj)/scale_factor)),
  home_sideout = 1/(1 + exp((Away_Rating - Home_Rating + serve_adj - Home_Adj)/scale_factor))
)


###### Step 14: Simulate the matches ######

predict_game <- function(home_sideout,away_sideout){
  game_tribble <- tibble::tribble(
    ~team, ~sideout,
    "home", home_sideout,
    "away", away_sideout
  )
  
  game_result <- vs_simulate_match_theor(game_tribble, process_model = "sideout")
  
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

# Make predictions look pretty
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
  `Prediction Date` = as.Date(ymd(prediction_date) + ddays(1))
) %>%
  arrange(Date, `Home Rank`, `Away Rank`)

## You can save the predictions to a csv file if you want
#predictions_file <- paste0("predictions_", prediction_date, ".csv")
#write_csv(games_predictions_final, predictions_file)
