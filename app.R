# https://stackoverflow.com/questions/68550960/r-shiny-how-to-make-the-initial-value-for-numericinput-dynamic-based-on-user-i

library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(readxl)
library(reactable)
options(scipen = 999)

defaults_all = read_xlsx("data/country_defaults.xlsx")

# Define UI
ui <- fluidPage(
    
    # App title
    titlePanel("Egg Calculator"),
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
        
        # Sidebar to demonstrate various slider options
        sidebarPanel(
            h3("Country"),
            selectInput("country", "What country are you from?", choices = unique(defaults_all$country), selected = NULL),
            p("Click below to generate some default numbers based on your country"),
            actionButton("go", "Generate defaults!"),
            br(),
            br(),
            
            h3("Basic stats"),
            numericInput("num_years", "How many years would you like to forecast?", value = 10, min=1, max=50, step=1),
            numericInput("flock_size", "What is the initial flock size?", value = NULL, min=0, step=1000),
            numericInputIcon("mortality", "What is the mortality rate of your flock?", value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))),
            numericInputIcon("growth", "What is the expected growth rate of your flock?", value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))),
            numericInputIcon("perc_laying", "What percentage of your flock lays eggs?", value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))),
            
            
            h3("Costs")
        ),
        
        # Main panel for displaying outputs
        mainPanel(
            plotlyOutput("g"),
            reactableOutput("fc")
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    defaults = reactive(defaults_all %>% filter(country == input$country))
    fc = reactive({
        tibble(
            year = 1:input$num_years,
            flock_size = as.integer(input$flock_size * (1 + input$growth/100 - input$mortality/100) ^ year)
    )})
    
    observeEvent(input$country, {for (var in c("flock_size", "mortality", "growth", "perc_laying")) updateNumericInput(session, inputId = var, value = NA)})

    observeEvent(input$go, {for (var in c("flock_size", "mortality", "growth", "perc_laying")) updateNumericInput(session, inputId = var, value = filter(defaults(), variable==var) %>% pull(value))})
            
    output$flock_size = renderText(input$flock_size)
    output$fc = renderReactable(reactable(fc()))
    output$g = renderPlotly(
        ggplot(fc()) + 
            aes(x=year, y=flock_size, group=1) + 
            geom_line() + geom_point() +
            scale_x_continuous(breaks=fc()$year) + 
            scale_y_continuous(labels = scales::comma)
    )
}

# Run the application
shinyApp(ui = ui, server = server)