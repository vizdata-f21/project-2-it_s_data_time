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

test <- read_csv("../data/test.csv")
# Country Column

movies_ratings <- movies_ratings %>%
   separate(country, c("country1", "country2"), ", ") %>%
   pivot_longer(starts_with("country"),
                names_to = "temp",
                values_to = "country")
# filter(!is.na(country)) %>%
# pivot_longer(
#    cols = c(
#       males_0age_avg_vote,
#       males_18age_avg_vote,
#       males_30age_avg_vote,
#       males_45age_avg_vote,
#       males_allages_avg_vote,
#       females_0age_avg_vote,
#       females_18age_avg_vote,
#       females_30age_avg_vote,
#       females_45age_avg_vote,
#       females_allages_avg_vote
#    ),
#    names_to = "age_Cat",
#    values_to = "rating_age"
# ) %>%
# mutate(
#    voter_age = case_when(
#       age_Cat == "males_0age_avg_vote" ~ "0-17",
#       age_Cat == "males_18age_avg_vote" ~ "18-29",
#       age_Cat == "males_30age_avg_vote" ~ "30-45",
#       age_Cat == "males_45age_avg_vote" ~ "over_45",
#       age_Cat == "males_allages_avg_vote" ~ "All",
#       age_Cat == "females_0age_avg_vote "~ "0-17",
#       age_Cat == "females_18age_avg_vote" ~ "18-29",
#       age_Cat == "females_30age_avg_vote" ~ "30-45",
#       age_Cat == "females_45age_avg_vote" ~ "over_45",
#       age_Cat == "females_allages_avg_vote" ~ "All"
#    ),
#    voter_gender = case_when(
#       age_Cat == "males_0age_avg_vote" ~ "M",
#       age_Cat == "males_18age_avg_vote" ~ "M",
#       age_Cat == "males_30age_avg_vote" ~ "M",
#       age_Cat == "males_45age_avg_vote" ~ "M",
#       age_Cat == "males_allages_avg_vote" ~ "M",
#       TRUE ~ "F"
#    )
# )

gender <- test %>%
   filter(!is.na(voter_gender)) %>%
   distinct(voter_gender) %>%
   pull()

country <- movies_ratings %>%
   filter(!is.na(country)) %>%
   mutate(country_other = fct_lump_min(country, min = 1000)) %>%
   distinct(country_other) %>%
   arrange(country_other) %>%
   pull()

# Shiny UI
ui <- navbarPage(
   inverse = TRUE,
   "Analysis of Movies",
   #Page 1.

   tabPanel("Gengres"),
   tabPanel(title = "Ratings Tab",
            sidebarLayout(
               sidebarPanel(
                  checkboxGroupInput(
                     inputId = "Country",
                     label = "Select countries",
                     choices = country,
                     selected = c("USA", "UK")
                  )
               ),
               mainPanel(tabsetPanel(
                  tabPanel(
                     "Ratings by Age and Gender",
                     plotOutput(outputId = "duration_rating"),
                     radioButtons(
                        inputId = "Gender",
                        label = "Choose Gender",
                        choices = gender,
                        width = "50%"
                     )
                  ),
                  tabPanel(
                     "Ratings by Year and Budget",
                     plotOutput(outputId = "budget_rating"),
                     splitLayout(radioButtons(
                        inputId = "Budget",
                        label = "Choose bar graph type",
                        choices = c("stack","dodge")
                     ),
                     radioButtons(
                        inputId = "Budget_Cat",
                        label = "Choose rating metric",
                        choices = c("Median Rating","Rating Category")
                     )),
                     plotOutput(outputId = "yr_plot",
                                hover = hoverOpts(id = "plot_hover")),
                     verbatimTextOutput("hover_info"),
                     sliderInput(
                        inputId = "ylim",
                        label = "Select Year Range",
                        min = 1950,
                        value = c(1950, 2020),
                        max = 2020,
                        width = "70%",
                        step = 5,
                        sep = ""
                     )
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
                              mainPanel(gt_output(outputId = "directors"))
                           ))
               ))
            )),
   tabPanel("Davis Tab"),
   tabPanel("Martha Tab")
)


# SHINY SERVER

server <- function(input, output, session) {
   ######################



   # Duration Plot Data Set
   movie_duration <- reactive({
      test %>%
         filter(country %in% input$Country) %>%
         filter(voter_gender == input$Gender) %>%
         mutate(duration_cat = cut(
            duration,
            breaks = c(-Inf, 41, 151, Inf),
            labels = c(
               "Short Film: <40 mins",
               "Feature Film: 41-150 mins",
               "Long Film: >150 mins"
            )
         )) %>%
         filter(!is.na(voter_gender))
   })

   # Year Rating Data Set
   movie_yr_ratings <- reactive({
      movies_ratings %>%
         filter(country %in% input$Country) %>%
         #filter(year >= min(input$ylim) && year <= max(input$ylim)) %>%
         group_by(year) %>%
         summarise(med = median(avg_vote), count = n())
   })


   # Budget Rating Dataset
   movie_budget <- reactive({
      movies_ratings %>%
         filter(country %in% input$Country) %>%
         filter(!is.na(budget)) %>%
         filter(!is.na(median_vote)) %>%
         mutate(
            budget_cat = cut(
               parse_number(budget),
               breaks = c(-2, 500000, 2000000, 10000000, Inf),
               labels = c("< $500k", "$500k-$20M", "$20M-$100M", ">$100M")
            ),
            rating_cat = cut(
               median_vote,
               breaks = c(0, 4, 7, 11),
               labels = c("0-3", "4-7", "8-10")
            ),
            ,
            median_vote_r = round(median_vote)
         )
   })

   # Director Rating Dataset
   director_rating <- reactive({
      movies_ratings %>%
         filter(country %in% input$Country) %>%
         filter(year >= min(input$ylim) &&
                   year <= max(input$ylim)) %>%
         select(c(director, mean_vote, country, duration)) %>%
         separate(director, c("director1", "director2"),
                  ", ") %>%
         pivot_longer(starts_with("director"),
                      names_to = "temp",
                      values_to = "director") %>%
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


################################################################################
# Plots
################################################################################

   # Budget rating plot
   output$budget_rating <- renderPlot({

      if(input$Budget_Cat == "Median Rating"){
         ggplot(data = movie_budget(),
                aes(x = median_vote_r,
                    fill = budget_cat)) +
            geom_bar(position = input$Budget) +
            labs(fill = "Budget category",
                 x = "Rating",
                 y = "Count",
                 title = "Median IMDb rating",
                 subtitle = "By Budget categories") +
            scale_x_continuous(breaks = c(1:10))
      }else{
         ggplot(data = movie_budget(),
                aes(x = rating_cat,
                    fill = budget_cat)) +
            geom_bar(position = input$Budget) +
            labs(fill = "Budget category",
                 x = "Rating Category",
                 y = "Count",
                 title = "Median IMDb rating",
                 subtitle = "By Budget categories")
      }


   })

   # Duration Plot
   output$duration_rating <- renderPlot({

      print(unique(movie_duration()$voter_gender))
      print(unique(input$Gender))

      movie_duration() %>%
         ggplot(aes(y = avg_vote,
                    x = duration_cat)) +
         geom_boxplot(fill = "red") +
         labs(x = "Duration Category",
              y = "Average rating by Males",
              title = "Average IMDb rating by movie duration",
              subtitle = "Faceted by age categories") +
         facet_wrap(. ~ voter_age)
   })

   # Directors table
   output$directors <- render_gt({
      director_rating() %>%
         gt() %>%
         cols_label(
            director = "Director",
            country = "Country",
            avg_ratings = "Average Rating",
            count = "# Films"
         ) %>%
         tab_spanner(label = "Top 10 Most Highly Rated Directors",
                     columns = everything()) %>%
         fmt_number(columns = where(is.numeric),
                    decimals = 2) %>%
         cols_align(align = "right", columns = where(is.numeric)) %>%
         cols_align(align = "left", columns = where(is.character))
   })


   # Year Plot
   output$yr_plot <- renderPlot(
      ggplot(data = movie_yr_ratings(),
             aes(y = med,
                 x = year)) +
         geom_point(aes(size = count)) +
         geom_line() +
         scale_x_continuous(limits = input$ylim) +
         labs(title = "Median IMDb Rating by Year",
              x = "Year",
              y = "Median IMDb rating",
              size = "Number of movies"
              )
   )

   # Tooltip information
   output$hover_info <- renderPrint({
      if (!is.null(input$plot_hover)) {
         hover = input$plot_hover
         dist = sqrt((hover$x - movie_yr_ratings()$year) ^ 2 + (hover$y -
                                                   movie_yr_ratings()$med) ^ 2)
         cat("Total movies in ")
         cat(movie_yr_ratings()$year[which.min(dist)])
         cat(": ")
         cat(movie_yr_ratings()$count[which.min(dist)])
      }

   })

}

# Year slider
observeEvent(input$year, {
   updateSliderInput(
      inputId = "ylim",
      min = min(movie_yr_ratings()$year),
      max = max(movie_yr_ratings()$year),
      value = c(min(movie_yr_ratings()$year),
                max(movie_yr_ratings()$year))
   )
})

# Motivation
# Purpose
   # Overcoming technical challenges (Not too much)
# Audience


# Run application.

shinyApp(ui = ui, server = server)
