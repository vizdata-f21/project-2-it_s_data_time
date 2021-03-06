Proposal
================
It’s Data Time: Raffay Rana,Davis Jones, Martha Aboagye，Krystal Hu

## High Level Goal

Our high level goal is to build a dashboard that allows users to
interact with movie data from an IMDb dataset of movies and cast
members.

## Description of Goals and Motivations

We would like to create a tool for users to easily explore movie data.
There are many movies out there, and we figured that building a
dashboard with varying functionalities is appropriate to view this data,
as it allows users to interact with the data in whatever way they
choose. We wanted to build a dashboard with movies because we believe it
is an interesting subject, and we were able to find a large, complete
dataset with various different variables. We will use the “movies”
dataset from IMBd for this project. It includes three different CSVs,
one with movie data, one with movie ratings, and one with casting
information. We plan to join the three different CSVs together in our
analysis, based on common variables such as movie titles and move ids.

We do not have a particular question or questions we would like to
answer yet. Due to the size of our data, we will have to work with it
and test its functionality in the dashboard to generate questions that
are feasible to answer with this tool. However, upon our initial review
of the data, we are confident that we would like to further explore
movie ratings, cast members, and genres. More specifically, it could be
interesting to explore the relationships between movie ratings and
actors in those movies in an effort to gauge the popularity of actors.
It may also be interesting to see how the frequency of certain genres
have changed over time. Based on the design of our dashboard and initial
exploration of the data, we will further refine these areas of interest
to come up with specific objectives for our dashboard.

## Dataset

This data comes from
[Kaggle](https://www.kaggle.com/stefanoleone992/imdb-extensive-dataset?select=IMDb+movies.csv).
We have three main csv files, `IMDbmovies.csv`,
`IMDbtitle_principals.csv`, and `IMDbtitle_principals.csv`.

The `IMDbmovies.csv` dataset contains data on movies with more than 100
votes scraped from the IMDb website uptill 01/01/2020. The dataset has
`85855` observations and `22` variables. For our secondary datasets, the
`IMDbtitle_principals.csv` dataset has has `835513` observations and `6`
variable and the `IMDBratings` has `85855` observations and `49`
variables.

## Weakly Plan of Attack

| Week   | Activity description                       | Breakdown of tasks                                                                                                                                                                                                                                                     |
|:-------|:-------------------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Week 3 | Finish project proposal by friday          | Data dictionary/dataset inline code- Martha.Read me- Raffay Davis- Proposal writeup. Krystal - Final review before submission on friday.                                                                                                                               |
| Week 4 | Peer Review and submit an updated proposal | Submit peer reviews- individual assignments. Start working on shiny tutorial (<https://shiny.rstudio.com/tutorial/>) - All. Data exploration/come up with initial ideas for visualization. Join Data Sets by `imdb_title_id` to consolidate data in one major dataset. |
| Week 5 | Finalize shiny apps for dashboard          | Finalize ideas for shiny apps and assign each member an app according to their interests. Primary areas of focus: aggregate metrics of rating and revenue across Country, Genre, Language. - All Krystal–close issues on proposal.                                     |
| Week 6 | Dashboard and Presentation                 | Everyone: Choose a dashboard framework and combine shiny apps. Divide up and create presentation slides. Review and practice final presentation                                                                                                                        |
