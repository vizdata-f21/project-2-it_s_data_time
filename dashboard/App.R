# Load packages -----------------------------------------------------
library(shiny)
library(tidyverse)
library(scales)
library(DT)



# Load data ---------------------------------------------------------

ratings <- read_csv("../data/IMDbratings.csv")
movies <- read_csv("../data/IMDb movies.csv")
title_principles <- read_csv("../data/IMDb title_principals.csv")
movies <- movies %>%
  mutate(century = case_when(
    year >= 2000 ~ "1",
    year >= 900 & year < 2000 ~ "0"
  ))


## Function for People Tab Data Filtering.

connections <- function(centurysel, initialnode, name = "Spike Lee", connection) {
  if (initialnode == "directors") {
    movies %>%
      filter(century == centurysel) %>%
      filter(str_detect(director, name)) %>%
      select(connection) -> A
    top10 <- unnest_tokens(A, split, names(A), token = str_split, pattern = ",") %>%
      group_by(split) %>%
      count() %>%
      arrange(desc(n)) %>%
      head(10)
    top10$node <- replicate(10, name)
    top10 <- top10 %>%
      rename(weight = n)
    top10 <- top10 %>%
      select(node, split, weight)
  } else {
    if (initialnode == "writer") {
      movies %>%
        filter(century == centurysel) %>%
        filter(str_detect(writer, name)) %>%
        select(connection) -> A
      top10 <- unnest_tokens(A, split, names(A), token = str_split, pattern = ",") %>%
        group_by(split) %>%
        count() %>%
        arrange(desc(n)) %>%
        head(10)
      top10$node <- replicate(10, name)
      top10 <- top10 %>%
        rename(weight = n)
      top10 <- top10 %>%
        select(node, split, weight)
    } else {
      movies %>%
        filter(century == centurysel) %>%
        filter(str_detect(actors, name)) %>%
        select(connection) -> A
      top10 <- unnest_tokens(A, split, names(A), token = str_split, pattern = ",") %>%
        group_by(split) %>%
        count() %>%
        arrange(desc(n)) %>%
        head(10)
      top10$node <- replicate(10, name)
      top10 <- top10 %>%
        rename(weight = n)
      top10 <- top10 %>%
        select(node, split, weight)
    }
  }
  return(top10)
}


# Shiny UI
ui <- navbarPage(
  inverse = TRUE, "I 💙 Movies",
  # Page 1.
  tabPanel(
    "Guess the Genre", includeCSS("css/styles.css"),
    p("lol our changes are on individual branches and not merged yet.
             check out the peoples tab for a little progress ")
  ),
  tabPanel("Everyone's A Critic"),
  tabPanel(
    "A Net of Stars ",
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
            choices = c("Directors" = "directors", "Writers" = "writer", "Actors" = "actors"),
            selected = "directors"
          )
        ),
        wellPanel(
          style = "background: #2D708E; color: white",
          p("Select Further Connections to Initial Node to  Explore"),
          uiOutput("furtherOptions")
        )
      ),
      mainPanel(
        h1(strong(em("\"Filmmaking is a chance to live many lifetimes.\""), "- Robert Altman")),
        h3(textOutput("timeperiod")),
        br(),
        plotOutput("NetworkPlot"),
        br(), br(), br(), br(), br(), br(),
        br(), br(), br(), br(),
        h3(textOutput("analyzeConnection")),
        br(), br(),
        br(),
        dataTableOutput("connectTable")
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
        textInput("name", "Director name contains (e.g., Spike Lee)"),
        radioButtons("connections",
          "What connections do you want to visualize:",
          choices = c("Writers" = "writer", "Actors" = "actors"),
          selected = "actors",
          inline = TRUE
        ),
        if (input$century == "1") {
          sliderInput("year", "Year released", 2000, 2020,
            value = c(2000, 2020),
            sep = ""
          )
        } else {
          sliderInput("year", "Year released", 1900, 1999,
            value = c(1900, 1999),
            sep = ""
          )
        },
        actionButton("do", "Generate Plot")
      )
    } else {
      if (input$start == "writer") {
        list(
          textInput("name", "Writer name contains (e.g., Tyler Perry)"),
          radioButtons("connections",
            "What connections do you want to visualize:",
            choices = c("Directors" = "director", "Actors" = "actors"),
            selected = "actors",
            inline = TRUE
          ),
          if (input$century == "1") {
            sliderInput("year", "Year released", 2000, 2020,
              value = c(2000, 2020),
              sep = ""
            )
          } else {
            sliderInput("year", "Year released", 1900, 1999,
              value = c(1900, 1999),
              sep = ""
            )
          },
          actionButton("do", "Generate Plot")
        )
      } else {
        list(
          textInput("name", "Actor name contains (e.g., Brad Pitt)"),
          radioButtons("connections",
            "What connections do you want to visualize:",
            choices = c("Writers" = "writer", "Directors" = "director"),
            selected = "writer",
            inline = TRUE
          ),
          if (input$century == "1") {
            sliderInput("year", "Year released", 2000, 2020,
              value = c(2000, 2020),
              sep = ""
            )
          } else {
            sliderInput("year", "Year released", 1900, 1999,
              value = c(1900, 1999),
              sep = ""
            )
          },
          actionButton("do", "Generate Plot")
        )
      }
    }
  })

  spatialGraph <- eventReactive(input$do, {
    df <- connections(input$century, input$start, input$name, input$connections)
  })


  output$NetworkPlot <- renderPlot(
    {
      bigram_graph <- spatialGraph() %>%
        graph_from_data_frame(directed = TRUE)

      ggraph(bigram_graph, layout = "kk") +
        geom_edge_link(
          alpha = .5, color = "grey80",
          aes(width = weight), show.legend = FALSE
        ) +
        geom_node_point(size = 5, aes(color = as.factor(name)), show.legend = FALSE) +
        geom_node_text(aes(label = name), repel = TRUE) +
        theme_graph() +
        scale_edge_width(range = c(1, 10)) +
        scale_color_viridis(discrete = TRUE) +
        labs(title = paste("Connection from", input$name, "to", input$connections), caption = "Size of Edge corresponds to frequency of the connnection")
    },
    height = 600,
    width = 800
  )

  output$analyzeConnection <- renderText(
    paste0("Wow, it looks like  ", input$name, "'s", " favorite ", input$connections, " is ", spatialGraph()[1, 2])
  )

  output$connectTable <- DT::renderDataTable(
    spatialGraph() %>%
      rename(Name = split) %>%
      rename("Number of Appearances" = weight) %>%
      select(-node)
  )
}



# Run application.

shinyApp(ui = ui, server = server)
