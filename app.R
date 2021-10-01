library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Simple Calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId = "x", label = "Type in a value", value = 0),
            numericInput(inputId = "y", label = "Type in a value", value = 0),
            numericInput(inputId = "z", label = "Type in a value", value = 0)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h3(textOutput("sum"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$sum <- renderText({
        s = input$x + input$y + input$z
        paste("The sum of the three numbers is ", s)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
