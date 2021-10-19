library(tidyverse)
library(httr)
library(rvest)

###### This script contains functions for getting a team's schedule off the NCAA website

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
