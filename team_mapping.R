library(tidyverse)
library(httr)
library(rvest)

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
