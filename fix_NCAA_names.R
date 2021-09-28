# This function fixes all of the nonsense from either inconsistent team names or nonsense from scraping
# Most of this is from missing &

fix_NCAA_names <- function(x){
  case_when(x == "Americanan" ~ "American",
            x == "N.C. AT" ~ "N.C. A&T",
            str_detect(x, "Texas AM") ~ "Texas A&M",
            x == "LMU (CA) (CA)" | x == "LMU" ~ "LMU (CA)",
            x == "Binghamtonmton" ~ "Binghamton",
            x == "Saint Marys" | x == "St. Marys" ~ "Saint Mary's (CA)",
            x == "Gardner" ~ "Gardner-Webb",
            x == "St. Johns" ~ "St. John's (NY)",
            x == "William Mary" ~ "William & Mary",
            TRUE ~ x)
}
