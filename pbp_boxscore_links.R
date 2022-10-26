library(tidyverse)
library(httr)
library(rvest)

## This function finds and returns the links to the box score and play-by-play given the url to a game on NCAA website

pbp_boxscore_links <- function(url){
  
  # if(str_detect(url, "\\.html")){
  #   destination_file <- url
  # } else {
  #   destination_file <- paste0(url, ".html")
  # }
  # 
  # download.file(url, destfile = destination_file)
  # 
  all_links <- read_html(url) %>% html_nodes("a")
  box_score_link <- html_attr(all_links, "href")[str_detect(html_text(all_links), "1st Set")] %>% str_remove("\\?period_no=1")
  if(length(box_score_link) == 0){
    box_score_link <- html_attr(all_links, "href")[str_detect(html_text(all_links), "Box Score") & (html_attr(all_links, "href") != "#")]
  }
  pbp_link <- html_attr(all_links, "href")[str_detect(html_text(all_links), "Play by Play")]
  
  if(length(pbp_link) == 1){
    return_links <- paste0("https://stats.ncaa.org", c(box_score_link, pbp_link))
  } else {
    return_links <- c(paste0("https://stats.ncaa.org", box_score_link), NA_character_)
  }
  
  names(return_links) <- c("box_score", "play_by_play")
  
  return(return_links)
}
