library(shiny)
options(scipen = 999)

# Define UI
ui <- fluidPage(
    
    # App title
    titlePanel("Egg Calculator"),
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
        
        # Sidebar to demonstrate various slider options
        sidebarPanel(
            h2("Basic stats about your farm"),
            selectInput("country", "What country are you from?", choices = c("X", "Y"), selected = ""),
            numericInput("flock_size", "What is the initial flock size?", value = 10000),
            h2("Costs")
        ),
        
        # Main panel for displaying outputs
        mainPanel(
            
            textOutput("flock")
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    output$flock = renderText(input$flock_size)
}

# Run the application
shinyApp(ui = ui, server = server)
