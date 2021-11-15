library(shiny)

# Shiny UI


ui <- navbarPage(inverse = TRUE, "Analysis of Movies",
                 #Page 1.

                 tabPanel("Gengres"),
                 tabPanel("Raffay Tab"),
                 tabPanel("Davis Tab"),
                 tabPanel("Martha Tab")


)


# SHINY SERVER

 server <- function(input, output) {}



# Run application.

shinyApp(ui = ui, server = server)
