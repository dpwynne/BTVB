get_team_future_schedule <- function(team_name, team_id, sport, year){
  
  # This function is to get all of the games a team has played that the NCAA website lists, whether or not there are results
  # This is good for getting the future games in the current year, 
  # or games for sports that the NCAA website doesn't link to box scores (e.g., water polo)
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
    
    url2 <- paste0("https://stats.ncaa.org", schedule_link)[1]
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
