# Load packages -----------------------------------------------------
library(shiny)
library(tidyverse)
library(gt)
library(scales)
library(DT)
library(igraph)
library(tidytext)
library(ggraph)
library(visNetwork)
library(maps)
library(shinydashboard)

# Load data ---------------------------------------------------------
ratings <- read_csv("../data/IMDbratings.csv")

movies <- read_csv("../data/IMDb movies.csv") %>%
   filter(year >= 1950)

movies_ratings <- left_join(movies, ratings, by = "imdb_title_id")

title_principles <- read_csv("../data/IMDb title_principals.csv")
movies <- movies %>%
   mutate(century = case_when(year >= 2000 ~ "1",
                              year >= 900 & year < 2000 ~ "0"))


test <- read_csv("../data/test.csv")
# Country Column

movies_ratings <- movies_ratings %>%
   separate(country, c("country1", "country2"), ", ") %>%
   pivot_longer(starts_with("country"),
                names_to = "temp",
                values_to = "country")

            #Warning appears for observations with more than 2 countries.
            #Did not count countries listed in 3rd position and beyond.


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



## Function for People Tab Data Filtering.

connections <-
   function(centurysel,
            initialnode,
            name = "Spike Lee",
            connection) {
      if (initialnode == "directors") {
         movies %>%
            filter(century == centurysel) %>%
            filter(str_detect(director, name)) %>%
            select(connection) -> A
         top10 <-
            unnest_tokens(A, split, names(A), token = str_split, pattern = ",") %>%
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
            top10 <-
               unnest_tokens(A,
                             split,
                             names(A),
                             token = str_split,
                             pattern = ",") %>%
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
            top10 <-
               unnest_tokens(A,
                             split,
                             names(A),
                             token = str_split,
                             pattern = ",") %>%
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

### Genre
movie <- read_csv(here::here("data/IMDb movies.csv"))
data_genre <- movie %>%
   select(imdb_title_id:votes)

# clean variable 'genre' into the basic film categories: https://www.premiumbeat.com/blog/guide-to-basic-film-genres/
genre_all <- data_genre %>%
   mutate(genre = ifelse(str_detect(genre, "Comedy") == TRUE, "Comedy", genre)) %>%
   mutate(genre = ifelse(str_detect(genre, "Romance") == TRUE, "Romance", genre)) %>%
   mutate(genre = ifelse(str_detect(genre, "Action") == TRUE, "Action", genre)) %>%
   mutate(genre = ifelse(str_detect(genre, "Fantasy") == TRUE, "Fantasy", genre)) %>%
   mutate(genre = ifelse(str_detect(genre, "Crime") == TRUE, "Crime", genre)) %>%
   mutate(genre = ifelse(str_detect(genre, "Drama") == TRUE, "Drama", genre)) %>%
   mutate(genre = ifelse(str_detect(genre, "Horror") == TRUE, "Horror", genre)) %>%
   mutate(genre = ifelse(str_detect(genre, "Mystery") == TRUE, "Mystery", genre)) %>%
   mutate(genre = ifelse(str_detect(genre, "Thriller") == TRUE, "Thriller", genre)) %>%
   mutate(genre = ifelse(str_detect(genre, "Western") == TRUE, "Western", genre)) %>%
   mutate(genre = ifelse(str_detect(genre, "Animation") == TRUE, "Animation", genre)) %>%
   mutate(genre = ifelse(str_detect(genre, "Music") == TRUE, "Musical", genre)) %>%
   mutate(genre = ifelse(str_detect(genre, "Sci-Fi") == TRUE, "Sci-Fi", genre)) %>%
   mutate(genre = ifelse(str_detect(genre, "Biography") == TRUE, "Biography", genre)) %>%
   mutate(genre = ifelse(str_detect(genre, "History") == TRUE, "History", genre)) %>%
   mutate(genre = ifelse(str_detect(genre, "Adventure") == TRUE, "Adventure", genre))

genre_count <- genre_all %>%
   group_by(genre) %>%
   summarize(Count = n()) %>%
   arrange(desc(Count)) %>%
   subset(Count > 200)

genre_data <- subset(genre_all, (genre %in% genre_count$genre))


# clean variable 'country' using the first country listed for each entry
genre_data$country <- paste(genre_data$country,",", sep="")

genre_data <- genre_data %>%
   separate(country, c("main_country", "other_country"), extra = "merge", fill = "left")



# Shiny UI
ui <- navbarPage(
   inverse = TRUE,
   "Let's Make A M💚VIE",
   # Page 1.
   tabPanel(
      "Guess the Genre",
      includeCSS("css/styles.css"),
      tabItem(
         tabName = "genre",
         h2("Explore movie genres"),
         selectInput(inputId = "genre",
                     label = "Choose Movie Genre",
                     list("Action", "Adventure","Animation","Biography","Comedy",
                          "Crime", "Drama", "Fantasy", "History", "Horror", "Mystery",
                          "Musical", "Romance", "Sci-Fi", "Thriller", "Western")),
         fluidRow(plotOutput("bar_plot"), plotOutput("map"))
      )
   ),
   tabPanel(
      "Everyone's A Critic",
      sidebarLayout(sidebarPanel(
        style = "background: black",
        wellPanel(
          style = "background: #2D708E; color: white",
         checkboxGroupInput(
            inputId = "Country",
            label = "Select countries",
            choices = country,
            selected = c("USA", "UK")
         )
        )
      ),
      mainPanel(
         h3(strong(
            em("\"I'm told we movie critics praise movies that are long"),
            em("\ and boring.\""),
             " - Roger Ebert"
         )),
         tabsetPanel(
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
               splitLayout(
                  radioButtons(
                     inputId = "Budget",
                     label = "Choose bar graph type",
                     choices = c("stack", "dodge")
                  ),
                  radioButtons(
                     inputId = "Budget_Cat",
                     label = "Choose rating metric",
                     choices = c("Median Rating", "Rating Category")
                  )
               ),
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
                                          8, 9)
                           )
                        ),
                        mainPanel(gt_output(outputId = "directors"))
                     ))
         )
      ))
   ),
   tabPanel("A Net of Stars ",
            fluidPage(
               sidebarLayout(
                  position = "right",
                  sidebarPanel(
                     style = "background: black",
                     wellPanel(
                        style = "background: #2D708E; color: white",
                        selectInput(
                           "century",
                           "Select A Time Period: ",
                           choices = c("The 2000s" = "1", "The 1900s" = "0"),
                           selected = "1"
                        )
                     ),
                     wellPanel(
                        style = "background: #2D708E; color: white",
                        selectInput(
                           "start",
                           "Select initial node of connection: ",
                           choices = c(
                              "Directors" = "directors",
                              "Writers" = "writer",
                              "Actors" = "actors"
                           ),
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
                     h3(strong(
                        em("\"Filmmaking is a chance to live many lifetimes.\""),
                        "- Robert Altman"
                     )),
                     h3(textOutput("timeperiod")),
                     br(),
                     plotOutput("NetworkPlot"),
                    # visNetworkOutput("vizNetWork", width = 800, height = 600),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     h3(textOutput("analyzeConnection")),
                     br(),
                     br(),
                     br(),
                     dataTableOutput("connectTable")
                  )
               )
            ))
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
         filter(!is.na(voter_gender))%>%
        filter(!is.na(voter_age))
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
            median_vote_r = round(median_vote)
         )
   })

   # Director Rating Dataset
   director_rating <- reactive({
      movies_ratings %>%
         filter(country %in% input$Country) %>%
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
      if (input$Budget_Cat == "Median Rating") {
         ggplot(data = movie_budget(),
                aes(x = median_vote_r,
                    fill = budget_cat)) +
            geom_bar(position = input$Budget) +
            labs(
               fill = "Budget category",
               x = "Rating",
               y = "Count",
               title = "Median IMDb rating",
               subtitle = "By Budget categories"
            ) +
            scale_x_continuous(breaks = c(1:10)) +
            scale_fill_viridis_d() +
            theme_minimal()
      } else{
         ggplot(data = movie_budget(),
                aes(x = rating_cat,
                    fill = budget_cat)) +
            geom_bar(position = input$Budget) +
            labs(
               fill = "Budget category",
               x = "Rating Category",
               y = "Count",
               title = "Median IMDb rating",
               subtitle = "By Budget categories"
            ) +
            scale_fill_viridis_d()  +
            theme_minimal()
      }


   })

   # Duration Plot
   output$duration_rating <- renderPlot({
      print(unique(movie_duration()$voter_gender))
      print(unique(input$Gender))

      movie_duration() %>%
         ggplot(aes(y = avg_vote,
                    x = duration_cat)) +
         geom_violin(fill = "#29AF7F") +
         labs(
            x = "Duration Category",
            y = "Average rating by Males",
            title = "Average IMDb rating by movie duration",
            subtitle = "Faceted by age categories"
         ) +
         facet_wrap(. ~ voter_age)  +
         theme_minimal()
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
         tab_spanner(label = "Top Rated Directors",
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
         labs(
            title = "Median IMDb Rating by Year",
            x = "Year",
            y = "Median IMDb rating",
            size = "Number of movies"
         )  +
         theme_minimal()
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


   # Year slider
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
            radioButtons(
               "connections",
               "What connections do you want to visualize:",
               choices = c("Writers" = "writer", "Actors" = "actors"),
               selected = "actors",
               inline = TRUE
            ),
            if (input$century == "1") {
               sliderInput(
                  "year",
                  "Year released",
                  2000,
                  2020,
                  value = c(2000, 2020),
                  sep = ""
               )
            } else {
               sliderInput(
                  "year",
                  "Year released",
                  1900,
                  1999,
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
               radioButtons(
                  "connections",
                  "What connections do you want to visualize:",
                  choices = c("Directors" = "director", "Actors" = "actors"),
                  selected = "actors",
                  inline = TRUE
               ),
               if (input$century == "1") {
                  sliderInput(
                     "year",
                     "Year released",
                     2000,
                     2020,
                     value = c(2000, 2020),
                     sep = ""
                  )
               } else {
                  sliderInput(
                     "year",
                     "Year released",
                     1900,
                     1999,
                     value = c(1900, 1999),
                     sep = ""
                  )
               },
               actionButton("do", "Generate Plot")
            )
         } else {
            list(
               textInput("name", "Actor name contains (e.g., Brad Pitt)"),
               radioButtons(
                  "connections",
                  "What connections do you want to visualize:",
                  choices = c("Writers" = "writer", "Directors" = "director"),
                  selected = "writer",
                  inline = TRUE
               ),
               if (input$century == "1") {
                  sliderInput(
                     "year",
                     "Year released",
                     2000,
                     2020,
                     value = c(2000, 2020),
                     sep = ""
                  )
               } else {
                  sliderInput(
                     "year",
                     "Year released",
                     1900,
                     1999,
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
      df <-
         connections(input$century,
                     input$start,
                     input$name,
                     input$connections)
   })


   output$NetworkPlot <- renderPlot({
      bigram_graph <- spatialGraph() %>%
         graph_from_data_frame(directed = TRUE)

      ggraph(bigram_graph, layout = "kk") +
         geom_edge_link(
            color = "grey80",
            aes(width = weight*5),
            show.legend = FALSE
         ) +
         geom_node_point(size = c(0.5, V(bigram_graph)$weight) * 20,
                         aes(color = as.factor(name)),
                         show.legend = FALSE) +
         geom_node_text(aes(label = name), repel = TRUE) +
         theme_graph() +
         scale_edge_width(range = c(1, 10)) +
         scale_color_viridis(discrete = TRUE) +
         labs(
            title = paste("Connection from", input$name, "to", input$connections),
            caption = "Size of Edge corresponds to frequency of the connnection"
         )
   },
   height = 600,
   width = 800)

  # output$vizNetWork <- renderVisNetwork (
  #   {
 #      bigram_graph <- spatialGraph() %>%
#         graph_from_data_frame(directed = TRUE)
 #    V(bigram_graph)$color <- (viridis_pal()(11))
 #    V(bigram_graph)$size  <- spatialGraph()$weight * 5
#     visIgraph(bigram_graph, type= "full") %>%
 #        visInteraction(hover = TRUE, tooltipDelay = 0)

#     })

   output$analyzeConnection <- renderText(
      paste0(
         "Wow, it looks like  ",
         input$name,
         "'s",
         " favorite ",
         input$connections,
         " is ",
         spatialGraph()[1, 2]
      )
   )

   output$connectTable <- DT::renderDataTable(
      spatialGraph() %>%
         rename(Name = split) %>%
         rename("Number of Appearances" = weight) %>%
         select(-node)
   )


## GENRE TAB ------------------------------------------------------------------

# Load data  ---------------------------------------------------------
   # explore how the average ratings and the total number of movies made within each genre change over the years
data <- reactive({
   req(input$genre)
   df <-
      genre_data %>% filter(genre %in% input$genre) %>% group_by(year) %>% summarise(avg_rating = mean(avg_vote), count = n())
})

output$bar_plot <- renderPlot({
   ggplot(data(), aes(y = count, x = year, fill = avg_rating)) +
      geom_bar(stat = "identity",
               width = 1,
               size = 1) +
      scale_fill_continuous(
         name = "Rating average",
         trans = "log10",
         breaks = seq(from = 0, to = 10, by = 1),
         high = "red4",
         low = "darkgoldenrod1"
      ) +
      scale_y_continuous(name = "Count") +
      scale_x_continuous(
         name = "Year",
         limits = c(1900, 2020),
         breaks = seq(from = 1900, to = 2020, by = 10)
      ) +
      scale_fill_viridis() +
      theme(
         legend.position = "bottom",
         panel.grid = element_blank(),
         axis.ticks = element_blank(),
         panel.background = element_blank(),
         axis.text = element_text(size = 9, face = "bold"),
         axis.title = element_text(size = 13, face = "bold")
      ) +
      labs(title = "Production and ratings of major movie genres from 1900 to 2020",
           caption = "Source: IMDb movies extensive dataset; Kaggle")

})

# explore how the number of movies made varies across countries within each genre
world <- map_data("world")
mapdata <- reactive({
   req(input$genre)
   df <-
      genre_data %>% filter(genre %in% input$genre) %>% group_by(main_country) %>% summarise(count = n())
   df <- inner_join(df, world, by = c("main_country" = "region"))
   return(df)
})
output$map <- renderPlot({
   ggplot(data = mapdata(),
          mapping = aes(x = long, y = lat, group = group)) +
      coord_fixed(1.3) +
      geom_polygon(aes(fill = count)) +
      scale_fill_continuous(trans = "log10",
                            high = "red4",
                            low = "darkgoldenrod1") +
      scale_fill_viridis() +
      theme(
         plot.title = element_text(hjust = 0.1),
         plot.caption = element_text(hjust = 1),
         axis.text = element_blank(),
         axis.line = element_blank(),
         axis.ticks = element_blank(),
         panel.border = element_blank(),
         panel.grid = element_blank(),
         axis.title = element_blank(),
         panel.background = element_rect(fill = "white")
      ) +
      labs(title = "Production of major movie genres around the world",
           caption = "Source: IMDb movies extensive dataset; Kaggle")
})

}









# Motivation
# Purpose
# Overcoming technical challenges (Not too much)
# Audience


# Run application.

shinyApp(ui = ui, server = server)
