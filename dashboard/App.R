# Load packages -----------------------------------------------------
library(shiny)
library(tidyverse)
library(scales)

# Load data ---------------------------------------------------------

ratings <- read_csv("../data/IMDbratings.csv")
movies <- read_csv("../data/IMDb movies.csv")
title_principles <- read_csv("../data/IMDb title_principals.csv")

# Shiny UI
ui <- navbarPage(inverse = TRUE, "Analysis of Movies",
                 #Page 1.
                 tabPanel("Genres or Krystals Tab",  includeCSS("styles.css")),
                 tabPanel("Scores Tab"),
                 tabPanel("Peoples tab.",
                          fluidPage(h1("Analysis of People involved in the film making. ")

                                    )
                          )


)


# SHINY SERVER

 server <- function(input, output) {}



# Run application.

shinyApp(ui = ui, server = server)
