# Data

## Provenance

This data comes from [Kaggle](https://www.kaggle.com/stefanoleone992/imdb-extensive-dataset?select=IMDb+movies.csv). The motivation for curating this dataset was to investigate the aspects that make a movie successful. The `IMDbmovies.csv` dataset contains data on movies with more than 100 votes scraped from the IMDb website till 01/01/2020. The  dataset has 85855 observations and 22 variables.   For our secondary datasets, the `IMDbtitle_principals.csv` dataset has  has 835513 observations and 6 variable and the `IMDBratings` has  85855 observations and 49 variables.

## Primary Dataset
### IMDb movies.csv

|variable         |description |
|:----------------|:-----------|
|imdb_title_id| Title ID on IMDb|
|title| Title name on IMDb|
|original_title| Original film title|
|year| Year of release|
|date_published| Date of release|
|genre| Movie Genre|
|duration| Duration (in minutes)|
|country|  Country of Movie origin |
|language|movie language|
|director| Name of Directors |
|writer| Names of writers |
|production_company| production company|
|actors| actor names|
|description| Summary of plot|
|avg_vote| Average IMDb vote (from 1 to 10)|
|votes| number of votes received|
|budget| Movie budget|
|usa_gross_income| USA gross income |
|worlwide_gross_income| worldwide gross income|
|metascore| metascore rating|
|reviews_from_users| number of reviews from users|
|reviews_from_critics| number of reviews from critics |

## Secondary dataset
For the secondary datasets, we have provided the data dictionarys for a few variables we think might be relevant to our research.

### IMDb title_principals.csv


|variable         |description |
|:----------------|:-----------|
|imdb_title_id| Title ID on IMDb|
|ordering| order of importance in the movie|
|imdb_name_id| cast member name ID on IMDb|
|category| category of job done by the cast member|
|job| specific job done by the cast member|
|characters| name of the character played|

### IMDbratings.csv

|variable         |description |
|:----------------|:-----------|
|imdb_title_id| Title ID on IMDb|
|weighted_average_vote| total weighted average rating|
|total_votes| total votes received|
|mean_vote| total mean vote|
|median_vote| total median vote|
|votes_10| number of votes with rating equal to 10|
|votes_9| number of votes with rating equal to 9|
|votes_8| number of votes with rating equal to 8|
|votes_7| number of votes with rating equal to 7|
|votes_6| number of votes with rating equal to 6|


### Sources

1. Description of Variables come from  [Stefano Leone on Kaggle](https://www.kaggle.com/stefanoleone992/imdb-extensive-dataset?select=IMDb+movies.csv) 

