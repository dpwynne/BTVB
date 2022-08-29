# This function fixes all of the nonsense from either inconsistent team names or nonsense from scraping
# Most of this is from missing &

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
}

fix_Miami <- function(df){
  df %>% mutate(
    team = case_when(
      team != "Miami" ~ team,
      away_team == "Miami (OH)" | home_team == "Miami (OH)" ~ "Miami (OH)",
      away_team == "Miami (FL)" | home_team == "Miami (FL)" ~ "Miami (FL)"
    ),
    serving_team = case_when(
      serving_team != "Miami" ~ serving_team,
      away_team == "Miami (OH)" | home_team == "Miami (OH)" ~ "Miami (OH)",
      away_team == "Miami (FL)" | home_team == "Miami (FL)" ~ "Miami (FL)"
    ),
    point_won_by = case_when(
      point_won_by != "Miami" ~ point_won_by,
      away_team == "Miami (OH)" | home_team == "Miami (OH)" ~ "Miami (OH)",
      away_team == "Miami (FL)" | home_team == "Miami (FL)" ~ "Miami (FL)"
    )
  )
}
