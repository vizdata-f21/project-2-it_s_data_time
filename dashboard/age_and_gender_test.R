ratings <- read_csv("data/IMDbratings.csv")

movies <- read_csv("data/IMDb movies.csv") %>%
  filter(year >= 1950)

movies_ratings <- left_join(movies, ratings, by = "imdb_title_id")

movies_ratings %>%
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
      TRUE ~ "F"
    )
  ) %>%
  filter(!is.na(voter_age,voter_gender))

  select(age_Cat,rating_age,voter_age,voter_gender)

glimpse(movies_ratings)
