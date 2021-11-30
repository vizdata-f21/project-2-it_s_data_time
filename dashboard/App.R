# Load packages -----------------------------------------------------
library(shiny)
library(tidyverse)
library(gt)
# Load data ---------------------------------------------------------

ratings <- read_csv("../data/IMDbratings.csv")

movies <- read_csv("../data/IMDb movies.csv") %>%
  filter(year >= 1950)

movies_ratings <- left_join(movies, ratings, by = "imdb_title_id")

title_principles <- read_csv("../data/IMDb title_principals.csv")


# Country Column

movies_ratings <- movies_ratings %>%
   separate(country, c("country1", "country2"), ", ") %>%
   pivot_longer(starts_with("country"), names_to = "temp",
                values_to = "country") %>%
   filter(!is.na(country))

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
                             step = 5,
                             sep = ""
                             )
                          ),
                       mainPanel(
                          tabsetPanel(
                             tabPanel("rename",
                                      splitLayout(
                                         plotOutput(
                                            outputId = "male_duration_rating"
                                            ),
                                         plotOutput(
                                            outputId = "female_duration_rating"
                                            )
                                         )
                                      ),
                             tabPanel("rename2",
                                   plotOutput(
                                      outputId = "budget_rating"
                                      ),
                                   plotOutput(outputId = "yr_plot",
                                              hover = hoverOpts(
                                                 id ="plot_hover")
                                              ),
                                   verbatimTextOutput("hover_info")
                                   ),
                             tabPanel("Directors & Rating",
                                      sidebarLayout(
                                         sidebarPanel(
                                            selectInput(
                                               inputId = "Top",
                                               label = "Top __ Directors",
                                               choices = c(10, 20, 50)
                                               ),
                                            selectInput(
                                               inputId = "Num_films",
                                               label = "Minimum number of films",
                                               choices = c(1, 2, 3, 4, 5, 6, 7,
                                                           8, 9, 10)
                                               )
                                            ),
                                         mainPanel(
                                            gt_output(outputId = "directors")
                                            )
                                         )
                                      )
                             )
                          )
                       )
                    ),
                 tabPanel("Davis Tab"),
                 tabPanel("Martha Tab")
                 )


# SHINY SERVER

 server <- function(input, output,session) {

    # To Do for later:
      # Pivot Longer Age Breakdowns and Gender Breadowns and make it into a
      # Selection menu

    movie_duration <- reactive({
       movies_ratings %>%
          filter(country %in% input$Country) %>%
          filter(year >= min(input$ylim) &&
                    year <= max(input$ylim)) %>%
          mutate(duration_cat = cut(
             duration,
             breaks = c(-Inf, 41, 151, Inf),
             labels = c(
                "Short Film: <40 mins",
                "Feature Film: 41-150 mins",
                "Long Film: >150 mins"
             )
          ))
   })


   movie_yr_ratings <- reactive({
      movies_ratings %>%
         filter(country %in% input$Country) %>%
         filter(year >= min(input$ylim) && year <= max(input$ylim)) %>%
         group_by(year) %>%
         summarise(med = median(avg_vote), count = n())
   })


   movie_budget <- reactive({
      movies_ratings %>%
         filter(country %in% input$Country) %>%
         filter(year >= min(input$ylim) && year <= max(input$ylim)) %>%
         filter(!is.na(budget)) %>%
         mutate(budget_cat = cut(
            parse_number(budget),
            breaks = c(-Inf, 500000, 2000000, 10000000, Inf),
            labels = c("< $500k", "$500k-$20M", "$20M-$100M", ">$100M")
         ),
         rating_cat = cut(
            median_vote,
            breaks = c(-Inf, 4, 7, Inf),
            labels = c("0-3", "4-7", "8-10")
         )
         )
   })


   director_rating <- reactive({
      movies_ratings %>%
         filter(country %in% input$Country) %>%
         filter(year >= min(input$ylim) &&
                   year <= max(input$ylim)) %>%
         select(c(director, mean_vote, country, duration)) %>%
         separate(director, c("director1", "director2"),
                  ", ") %>%
         pivot_longer(starts_with("director"),
                      names_to = "temp", values_to = "director") %>%
         na.omit() %>%
         group_by(director) %>%
         mutate(avg_ratings = mean(mean_vote),
                count = n()) %>%
         #avg_duration = mean (duration)) %>%
         select(-c(temp, mean_vote, duration)) %>%
         arrange(desc(avg_ratings), director) %>%
         distinct() %>%
         ungroup() %>%
         filter(count >= input$Num_films) %>%
         slice(1:input$Top)
   })


   output$budget_rating <- renderPlot(
      ggplot(data = movie_budget(),
             aes(x = rating_cat,
                 fill = budget_cat)) +
         geom_bar(position="dodge") +
         labs(
            fill = "Budget category",
            x = "Rating Category",
            y = "Count"
         )
   )


   output$male_duration_rating <- renderPlot(
      ggplot(data = movie_duration(),
             aes(y = males_allages_avg_vote,
                 x = duration_cat)) +
         geom_boxplot(fill = "blue") +
         labs(
            x = "Duration Category",
            y = "Average rating by Males"
         )
   )

   output$female_duration_rating <- renderPlot(
      ggplot(data = movie_duration(),
             aes(y = females_allages_avg_vote,
                 x = duration_cat)) +
         geom_boxplot(fill = "red") +
         labs(
            x = "Duration Category",
            y = "Average rating by Females"
         )
   )

   output$directors <- render_gt({
      director_rating() %>%
         gt() %>%
         cols_label(director = "Director",
                    country = "Country",
                    avg_ratings = "Average Rating",
                    count = "# Films") %>%
         tab_spanner(
            label = "Top 10 Most Highly Rated Directors",
            columns = everything()) %>%
         fmt_number(
            columns = where(is.numeric),
            decimals = 2) %>%
         cols_align(align = "right", columns = where(is.numeric))%>%
         cols_align(align = "left", columns = where(is.character))
   })

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
         ) +
         labs(
            y = "Median Rating",
            x = "Year"
         )
   )

   output$hover_info <- renderPrint({
      if(!is.null(input$plot_hover)){
         hover=input$plot_hover
         dist=sqrt((hover$x-movie_yr_ratings()$year)^2+(hover$y-movie_yr_ratings()$med)^2)
         cat("Total movies in ")
         cat(movie_yr_ratings()$year[which.min(dist)])
         cat(": ")
         cat(movie_yr_ratings()$count[which.min(dist)])
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
