# Load packages -----------------------------------------------------
library(shiny)
library(tidyverse)

# Load data ---------------------------------------------------------

ratings <- read_csv("../data/IMDbratings.csv")

movies <- read_csv("../data/IMDb movies.csv") %>%
  filter(year >= 1950)

movies_ratings <- left_join(movies, ratings, by = "imdb_title_id")

title_principles <- read_csv("../data/IMDb title_principals.csv")


# Country Column

country <- movies_ratings %>%
   filter(!is.na(country)) %>%
   mutate(
      country_other = fct_lump_min(country,min = 100)
   ) %>%
   distinct(country_other) %>%
   arrange(country_other) %>%
   pull()


# Shiny UI
ui <- navbarPage(inverse = TRUE, "Analysis of Movies",
                 #Page 1.

                 tabPanel("Gengres"),
                 tabPanel(
                    title = "Ratings Tab",
                    sidebarLayout(
                       sidebarPanel(
                          checkboxGroupInput(
                             inputId = "Country",
                             label = "Select countries",
                             choices = country
                          )
                       ),
                       mainPanel(
                          plotOutput(outputId = "len_rating"),
                          plotOutput(outputId = "budget_rating")
                       )
                    )),
                 tabPanel("Davis Tab"),
                 tabPanel("Martha Tab")
)


# SHINY SERVER

 server <- function(input, output,session) {

   movie_dt <- reactive({
      movies_ratings %>%
         filter(country %in% input$Country)
   })

   # Things to change:
   #  Cast budget into numeric
   #  Keep currencies constant
   movie_budget <- reactive({
      movies_ratings %>%
         filter(country %in% input$Country) %>%
         filter(!is.na(budget)) %>%
         mutate(budget_cat1 = case_when(
            budget < 500000 ~ "< $500k",
            budget <= 500000 ~ "$500k-$5M",
            budget <= 2000000 ~ "$5M-$20M",
            budget <= 10000000 ~ "$5M-$100M",
            TRUE ~ ">$100M"
         ),
         budget_cat = cut(parse_number(budget),
            breaks=c(-Inf, 500000, 2000000, 10000000, Inf),
            labels=c("< $500k","$500k-$20M","$20M-$100M",">$100M")
         )
         ) %>%
         group_by(budget_cat) %>%
         summarise(mean_vote = mean(mean_vote))
   })

   output$budget_rating <- renderPlot(
      ggplot(data = movie_budget(),
             aes(x = budget_cat,
                 y = mean_vote,
                 color = mean_vote)) +
         geom_point()
   )

   output$len_rating <- renderPlot(
      ggplot(data = movie_dt(),
             aes(x = duration,
                 y = median_vote,
                 color = median_vote)) +
         geom_point()
   )

   output$datatb<- DT::renderDataTable({
     # Remind Shiny it is a reactive objective
     # DO NOT FORGET THIS
     movie_dt()
   })

 }



# Run application.

shinyApp(ui = ui, server = server)
