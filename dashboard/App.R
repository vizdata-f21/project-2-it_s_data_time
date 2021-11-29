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
      country_other = fct_lump_min(country,min = 300)
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
                          ),
                            sliderInput(
                              inputId = "ylim",
                              label = "Select Year Range",
                              min = 1950,
                              value = c(1950, 2020),
                              max = max(movies_ratings$year),
                              width = "100%",
                              step = 5
                            )
                          ),
                       mainPanel(
                          plotOutput(outputId = "len_rating"),
                          plotOutput(outputId = "budget_rating"),
                          plotOutput(outputId = "yr_plot",
                                     hover = hoverOpts(id ="plot_hover")),
                          verbatimTextOutput("hover_info")
                       )
                    )),
                 tabPanel("Davis Tab"),
                 tabPanel("Martha Tab")
)


# SHINY SERVER

 server <- function(input, output,session) {

    # To Do for later:
      # Pivot Longer Age Breakdowns and Gender Breadowns and make it into a
      # Selection menu

   movie_dt <- reactive({
      movies_ratings %>%
         filter(country %in% input$Country)
   })


   movie_yr_ratings <- reactive({
      movies_ratings %>%
         filter(country %in% input$Country) %>%
         group_by(year) %>%
         summarise(med = median(avg_vote), count = n())
   })


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
             aes(y = budget_cat,
                 x = mean_vote,
                 color = mean_vote)) +
         geom_point()
   )

   output$len_rating <- renderPlot(
      ggplot(data = movie_dt(),
             aes(y = duration,
                 x = median_vote,
                 color = median_vote)) +
         geom_point()
   )

   output$datatb<- DT::renderDataTable({
     # Remind Shiny it is a reactive objective
     # DO NOT FORGET THIS
     movie_dt()
   })

   output$yr_plot <- renderPlot(
      ggplot(data = movie_yr_ratings(),
             aes(y = med,
                 x = year)) +
         geom_point(aes(size = count)) +
         geom_line() +
         scale_x_continuous(
            limits = input$ylim
         )
   )

   output$hover_info <- renderPrint({
      if(!is.null(input$plot_hover)){
         hover=input$plot_hover
         dist=sqrt((hover$x-movie_yr_ratings()$year)^2+(hover$y-movie_yr_ratings()$med)^2)
         cat("Total movies")
         if(min(dist) < 3)
            movie_yr_ratings()$count[which.min(dist)]
      }

   })

 }

 #year slider
 observeEvent(input$year, {
   updateSliderInput(
     inputId = "ylim",
     min = min(movie_yr_ratings()$year),
     max = max(movie_yr_ratings()$year),
     value = c(
       min(movie_yr_ratings()$year),
       max(movie_yr_ratings()$year)
     )
   )
 })



# Run application.

shinyApp(ui = ui, server = server)
