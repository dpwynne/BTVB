
# This is the wrapper function
# As far as I can tell, there are two different ways that the NCAA website stores play-by-play data
# This function tests the first way - which is more common - and tries the second one if the first way gives us nothing
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

# This is the messier one
vb_play_by_play_ncaa2 <- function(pbp_url){
  
  game_info <- read_html(pbp_url) %>% html_nodes(".mytable") %>% html_table(fill = TRUE)
  match_id <- str_remove(pbp_url, "https://stats.ncaa.org/game/play_by_play/")
  
  sets <- bind_rows(game_info[-c(1:2)])
  
  if(nrow(sets) == 0){
    return(NULL)
  }   ## return NULL if there are no sets in the file, I'm hoping this fixes the issues
  
    teams <- str_split(sets$X1[1], "\\s-\\s") %>% unlist() %>% str_squish() %>% fix_NCAA_names()
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

