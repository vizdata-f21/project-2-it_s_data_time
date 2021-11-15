# Load packages -----------------------------------------------------
library(shiny)
library(tidyverse)

# Load data ---------------------------------------------------------

ratings <- read_csv("../data/IMDbratings.csv")
movies <- read_csv("../data/IMDb movies.csv")
title_principles <- read_csv("../data/IMDb title_principals.csv")

# Shiny UI
ui <- navbarPage(inverse = TRUE, "Analysis of Movies",
                 #Page 1.

                 tabPanel("Genres or Krystals Tab"),
                 tabPanel("Scores Tab"),
                 tabPanel("Peoples tab.")


)


# SHINY SERVER

 server <- function(input, output) {}



# Run application.

shinyApp(ui = ui, server = server)
