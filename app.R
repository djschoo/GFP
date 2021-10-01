# https://stackoverflow.com/questions/68550960/r-shiny-how-to-make-the-initial-value-for-numericinput-dynamic-based-on-user-i

library(shiny)
library(tidyverse)
library(readxl)
options(scipen = 999)

defaults = read_xlsx("data/country_defaults.xlsx")

# Define UI
ui <- fluidPage(
    
    # App title
    titlePanel("Egg Calculator"),
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
        
        # Sidebar to demonstrate various slider options
        sidebarPanel(
            h2("Basic stats about your farm"),
            selectInput("country", "What country are you from?", choices = unique(defaults$country), selected = "China"),
            numericInput("flock_size", "What is the initial flock size?", value = NULL),
            h2("Costs")
        ),
        
        # Main panel for displaying outputs
        mainPanel(
            textOutput("flock")
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    output$flock = renderText(input$flock_size)
    
    default_flock = 
    observeEvent(
        input$country, {
            updateNumericInput(session, inputId = "flock_size", value = filter(defaults, country==input$country, variable=="flock_size") %>% pull(value))
            #updateNumericInput(session, inputId = "flock_size", value = case_when(input$country == "China" ~ 10, T ~ 500))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
