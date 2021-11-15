# Load packages -----------------------------------------------------
library(shiny)
library(tidyverse)

# Load data ---------------------------------------------------------

ratings <- read_csv("../data/IMDbratings.csv")

movies <- read_csv("../data/IMDb movies.csv") %>%
  filter(year >= 1950)

movies_ratings <- left_join(movies, ratings, by = "imdb_title_id")

title_principles <- read_csv("../data/IMDb title_principals.csv")

# Shiny UI
ui <- navbarPage(inverse = TRUE, "Analysis of Movies",
                 #Page 1.

                 tabPanel("Gengres"),
                 tabPanel(
                   title = "Raffay Tab",
                   DT::dataTableOutput(outputId = "datatb")
                   ),
                 tabPanel("Davis Tab"),
                 tabPanel("Martha Tab")
)


# SHINY SERVER

 server <- function(input, output,session) {

   movie_dt <- reactive(movies_ratings)

   output$datatb<- DT::renderDataTable({
     # Remind Shiny it is a reactive objective
     # DO NOT FORGET THIS
     movie_dt()
   })

 }



# Run application.

shinyApp(ui = ui, server = server)
