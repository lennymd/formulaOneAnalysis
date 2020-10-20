library(tidyverse)

# Read CSV file
df <- read_csv("https://raw.githubusercontent.com/lennymd/formulaOneAnalysis/main/data/race_results_statsf1.csv") %>% filter(year == 1974)

# Trying to calculate for the last place for a race
get_last <- function(some_race_id) {
  last <- df %>%
    select(race_id, position, driver) %>%
    filter(race_id == some_race_id) %>%
    filter(position != "&") %>%
    distinct(race_id,driver) %>%
    summarise(count=n()) %>%
    pull(count)
  return(last)
}

get_finish <- function(some_race_id, some_driver) {
  finish <- df %>%
            select(race_id, p_prelim, driver) %>%
            filter(race_id == some_race_id, driver == some_driver) %>%
            pull(p_prelim)
  return(as.integer(finish))
}

update_position_numeric <- function(some_race_id, some_driver, some_position) {
  retired <- some_position == "ab"
  shared_drive <- some_position == "&"
  mostly_completed <- some_position == "nc"
  numbered_finish <- !(some_position %in% c("ab", "nc", "np", "nq", "npq",
                                            "dsq","exc", "f", "tf", "&"))
  disqualified <- some_position == "dsq"
  last_place <- some_position == "np" | some_position == "f"
  ignored <- some_position %in% c("npq","nq", "tf", "exc")

  if(numbered_finish | shared_drive | mostly_completed | retired) {
    result <- get_finish(some_race_id, some_driver)
  } else if (disqualified) {
    result <- as.integer(get_last(race_id) + 1)
  } else if (last_place) {
    result <- get_last(race_id)
  } else if (ignored) {
    result <- as.integer(0)
  }
  return(result)
}



df2 <- df %>% rowwise() %>%
              mutate(position_numeric = case_when(
                position == "np" | position == "f" ~ get_last(race_id),
                position == "dsq" ~ as.integer(get_last(race_id) + 1),
                position %in% c("npq","nq", "tf", "exc") ~ as.integer(0),
                position %in% c("&","ab","nc") |
                  !(position %in% c("ab", "nc", "np", "nq", "npq", "dsq",
                                    "exc", "f", "tf")) ~
                  get_finish(race_id,driver)
              ))

for(row in 1:nrow(df)) {
  id <- df[row, "race_id"]
  p <- df[row, "position"]
  d <- df[row, "driver"]

}
