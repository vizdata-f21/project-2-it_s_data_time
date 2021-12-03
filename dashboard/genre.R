# Load packages -----------------------------------------------------
library(tidyverse)
library(shiny)
library(shinydashboard)
library(styler)
library(maps)

# Load data  ---------------------------------------------------------
movie <- read_csv(here::here("data/IMDb movies.csv"))
data <- movie %>%
  select(imdb_title_id:votes)

# clean variable 'genre' into the basic film categories: https://www.premiumbeat.com/blog/guide-to-basic-film-genres/
genre_all <- data %>%
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


# create genre tab

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "IMDb Movies"),
  dashboardSidebar(
    sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
    sidebarMenu(
      menuItem("Genre", tabName = "genre", icon = icon("grip-horizontal"))
    ),
    textInput("caption", "Text input", ""),
    verbatimTextOutput("value"),
    sliderInput("year", "Date range",
                min = 1894, max = 2021, value = 1
    )
  ),
  dashboardBody(
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
  )
)


server <- function (input, output) {
  # explore how the average ratings and the total number of movies made within each genre change over the years
  data <- reactive({
    req(input$genre)
    df <- genre_data %>% filter(genre %in% input$genre) %>% group_by(year) %>% summarise(avg_rating = mean(avg_vote), count = n())
  })
  output$bar_plot <- renderPlot({
    ggplot(data(), aes(y = count, x = year, fill = avg_rating)) +
      geom_bar(stat = "identity", width = 1, size = 1) +
      scale_fill_continuous(name = "Rating average", trans = "log10",
                            breaks = seq(from = 0, to = 10, by = 1),
                            high = "red4", low = "darkgoldenrod1") +
      scale_y_continuous(name = "Count") +
      scale_x_continuous(name = "Year", limits = c(1900, 2020), breaks = seq(from = 1900, to = 2020, by = 10)) +
      theme(
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 9, face = "bold"),
        axis.title = element_text(size = 13, face = "bold")) +
      labs(
        title = "Production and ratings of major movie genres from 1900 to 2020",
        caption = "Source: IMDb movies extensive dataset; Kaggle")

  })

  # explore how the number of movies made varies across countries within each genre
  world <- map_data("world")
  mapdata <- reactive({
    req(input$genre)
    df <- genre_data %>% filter(genre %in% input$genre) %>% group_by(main_country) %>% summarise(count = n())
    df <- inner_join(df, world, by = c("main_country" = "region"))
    return(df)
  })
  output$map <- renderPlot({
    ggplot(data = mapdata(), mapping = aes(x = long, y = lat, group = group)) +
      coord_fixed(1.3) +
      geom_polygon(aes(fill = count)) +
      scale_fill_continuous(trans = "log10", high = "red4", low = "darkgoldenrod1") +
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
      labs(
        title = "Production of major movie genres around the world",
        subtitle = "from 1900 to 2020",
        caption = "Source: IMDb movies extensive dataset; Kaggle")
  })
}

shinyApp(ui, server)