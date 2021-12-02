library(tidyverse)

ratings <- read_csv("data/IMDbratings.csv")

movies <- read_csv("data/IMDb movies.csv") %>%
  filter(year >= 1950)

movies_ratings <- left_join(movies, ratings, by = "imdb_title_id")

title_principles <- read_csv("data/IMDb title_principals.csv")

movies_ratings <- movies_ratings %>%
  separate(country, c("country1", "country2"), ", ") %>%
  pivot_longer(starts_with("country"), names_to = "temp",
               values_to = "country") %>%
  filter(!is.na(country)) %>%
  pivot_longer(
    cols = c(
      males_0age_avg_vote,
      males_18age_avg_vote,
      males_30age_avg_vote,
      males_45age_avg_vote,
      males_allages_avg_vote,
      females_0age_avg_vote,
      females_18age_avg_vote,
      females_30age_avg_vote,
      females_45age_avg_vote,
      females_allages_avg_vote
    ),
    names_to = "age_Cat",
    values_to = "rating_age"
  ) %>%
  mutate(
    voter_age = case_when(
      age_Cat == "males_0age_avg_vote" ~ "0-17",
      age_Cat == "males_18age_avg_vote" ~ "18-29",
      age_Cat == "males_30age_avg_vote" ~ "30-45",
      age_Cat == "males_45age_avg_vote" ~ "over_45",
      age_Cat == "males_allages_avg_vote" ~ "All",
      age_Cat == "females_0age_avg_vote "~ "0-17",
      age_Cat == "females_18age_avg_vote" ~ "18-29",
      age_Cat == "females_30age_avg_vote" ~ "30-45",
      age_Cat == "females_45age_avg_vote" ~ "over_45",
      age_Cat == "females_allages_avg_vote" ~ "All"
    ),
    voter_gender = case_when(
      age_Cat == "males_0age_avg_vote" ~ "M",
      age_Cat == "males_18age_avg_vote" ~ "M",
      age_Cat == "males_30age_avg_vote" ~ "M",
      age_Cat == "males_45age_avg_vote" ~ "M",
      age_Cat == "males_allages_avg_vote" ~ "M",
      age_Cat == "females_0age_avg_vote" ~ "F",
      age_Cat == "females_18age_avg_vote" ~ "F",
      age_Cat == "females_30age_avg_vote" ~ "F",
      age_Cat == "females_45age_avg_vote" ~ "F",
      age_Cat == "females_allages_avg_vote" ~ "F"
    )
  ) %>%
  select(voter_age,voter_gender,country,duration,avg_vote,budget,median_vote,
         director,mean_vote,year)


write_csv(movies_ratings,path = "data/age_gender_data.csv")
