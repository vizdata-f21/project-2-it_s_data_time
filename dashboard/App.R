# Load packages -----------------------------------------------------
library(shiny)
library(tidyverse)
library(scales)


# Load data ---------------------------------------------------------

ratings <- read_csv("../data/IMDbratings.csv")
movies <- read_csv("../data/IMDb movies.csv")
title_principles <- read_csv("../data/IMDb title_principals.csv")

# Shiny UI
ui <- navbarPage(
  inverse = TRUE, "Analysis of Movies",
  # Page 1.
  tabPanel("Genres or Krystals Tab", includeCSS("css/styles.css")),
  tabPanel("Scores Tab"),
  tabPanel(
    "Peoples tab.",
    fluidPage(sidebarLayout(
      position = "right",
      sidebarPanel(
        style = "background: black",
        wellPanel(
          style = "background: #2D708E; color: white",
          selectInput("century",
            "Select A Time Period: ",
            choices = c("The 2000s" = "1", "The 1900s" = "0"),
            selected = "1"
          )
        ),
        wellPanel(
          style = "background: #2D708E; color: white",
          selectInput("start", "Select initial node of connection: ",
            choices = c("Directors" = "directors", "Writers" = "writers", "Actors" = "actors"),
            selected = "directors"
          ),
        ),
        wellPanel(
          style = "background: #2D708E; color: white",
          p("Select Further Connections to Initial Node to  Explore"),
          uiOutput("furtherOptions")
        )
      ),
      mainPanel(
        h1(strong(em("\"Filmmaking is a chance to live many lifetimes.\""), "- Robert Altman")),
        textOutput("timeperiod")
      )
    ))
  )
)

# SHINY SERVER

server <- function(input, output) {





  # Peoples Tab Output.

  output$timeperiod <- renderText({
    if (input$century == "1") {
      paste("Analysing trends in the 21st Century: ")
    } else {
      paste("Analysing trends in the 20th Century: ")
    }
  })


  output$furtherOptions <- renderUI({
    if (input$start == "directors") {
      list(
      textInput("drector", "Director name contains (e.g., Spike Lee)"),
      checkboxGroupInput("connections",
        "What connections do you want to visualize:",
        choices = c("Writers" = "writers", "Actors" = "actors"),
        selected = "actors",
        inline = TRUE
      ),
      if (input$century == "1") {
      sliderInput("year", "Year released", 2000, 2020, value = c(2000, 2020),
                  sep = "")
      } else {
        sliderInput("year", "Year released", 1900,  1999, value = c(1900, 1999),
                    sep = "")
      }
      )
    } else {
      if (input$start == "writers") {
        list(
        textInput("writers", "Write name contains (e.g., Tyler Perry)"),
        checkboxGroupInput("connections",
                           "What connections do you want to visualize:",
                           choices = c("Directors" = "directors", "Actors" = "actors"),
                           selected = "actors",
                           inline = TRUE
        ),
        if (input$century == "1") {
          sliderInput("year", "Year released", 2000, 2020, value = c(2000, 2020),
                      sep = "")
        } else {
          sliderInput("year", "Year released", 1900,  1999, value = c(1900, 1999),
                      sep = "")
        }
        )
      } else {
        list(textInput("actors", "Actor name contains (e.g., Brad Pitt)"),
             checkboxGroupInput("connections",
                                "What connections do you want to visualize:",
                                choices = c("Writers" = "writers", "Directors" = "directors"),
                                selected = "actors",
                                inline = TRUE
             ),
             if (input$century == "1") {
               sliderInput("year", "Year released", 2000, 2020, value = c(2000, 2020),
                           sep = "")
             } else {
               sliderInput("year", "Year released", 1900,  1999, value = c(1900, 1999),
                           sep = "")
             }

             )
      }
    }
  })
}



# Run application.

shinyApp(ui = ui, server = server)
