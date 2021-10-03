# https://stackoverflow.com/questions/68550960/r-shiny-how-to-make-the-initial-value-for-numericinput-dynamic-based-on-user-i

library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(readxl)
library(reactable)
options(scipen = 999)

defaults_all = read_xlsx("data/country_defaults.xlsx")
flags = read_xlsx("data/flags.xlsx")
flags$flag = paste0("https://cdn.jsdelivr.net/gh/lipis/flag-icon-css@master/flags/4x3/", flags$flag, ".svg")

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

            h3("Basic stats"),
            sliderInput("num_years", "How many years in the future would you like to forecast?", value = 10, min=1, max=50, step=1),
            numericInput("flock_size", "What is the initial flock size?", value = NULL, min=0, step=1000),
            numericInputIcon("mortality", "What is the mortality rate of your flock?", value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))),
            numericInputIcon("growth", "What is the expected growth rate of your flock?", value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))),
            numericInputIcon("perc_laying", "What percentage of your flock lays eggs?", value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))),
            numericInput("eggs_laid", "What number of eggs does each hen lay?", value = NULL, min=0, step=1000),
            numericInputIcon("breakage", "What percentage of eggs break?", value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))),

            h3("Revenue per egg"),
            numericInputIcon("price_egg", "What is the selling price per egg?", value = NULL, min=0, max=10, step=.5, icon = list(icon("dollar", verify_fa=F))),
            numericInputIcon("price_spent", "What is your revenue per spent hen?", value = NULL, min=0, max=10, step=.5, icon=list(icon("dollar", verify_fa=F))),
            numericInputIcon("price_manure", "What is your revenue for manure from your spent hens?", value = NULL, min=0, max=10, step=.5, icon=list(icon("dollar", verify_fa=F))),

            h3("Cost per bird"),
            numericInputIcon("cost_feed", "What is the feed cost per bird per year?", value = NULL, min=0, max=5000, step=.5, icon=list(icon("dollar", verify_fa=F))),
            numericInputIcon("cost_labor", "What is the cost per labor per bird per year?", value = NULL, min=0, max=5000, step=.5, icon=list(icon("dollar", verify_fa=F))),
            numericInputIcon("cost_pullet", "What is the cost per pullet per bird per year?", value = NULL, min=0, max=5000, step=.5, icon=list(icon("dollar", verify_fa=F))),
            numericInputIcon("cost_equip", "What is the cost per equipment & maintenance per year?", value = NULL, min=0, max=5000, step=.5, icon=list(icon("dollar", verify_fa=F))),
            numericInputIcon("cost_litter", "What is the cost per litter per bird per year?", value = NULL, min=0, max=5000, step=.5, icon=list(icon("dollar", verify_fa=F))),
            numericInputIcon("cost_vet", "What is the cost per vaccination/veterinary per bird per year?", value = NULL, min=0, max=5000, step=.5, icon=list(icon("dollar", verify_fa=F))),
            
            h3("Fixed costs per year"),
            numericInputIcon("cost_land", "What is the cost of your land per year?", value = NULL, min=0, max=50000, step=.5, icon=list(icon("dollar", verify_fa=F))),
            numericInputIcon("cost_office", "What is the cost of your office rent per year?", value = NULL, min=0, max=50000, step=.5, icon=list(icon("dollar", verify_fa=F)))
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
    event_vars = c("flock_size", "mortality", "growth", "perc_laying", "eggs_laid", "breakage", "price_egg", "price_spent", "price_manure", "cost_feed", "cost_labor", "cost_pullet", "cost_equip", "cost_litter", "cost_vet", "cost_land", "cost_office")
    defaults = reactive(defaults_all %>% filter(country == input$country))
    
    observeEvent(input$country, {for (var in event_vars) updateNumericInput(session, inputId = var, value = NA)})

    observeEvent(input$go, {for (var in event_vars) updateNumericInput(session, inputId = var, value = filter(defaults(), variable==var) %>% pull(value))})
    
    fc = reactive({
        tibble(
            year = 1:input$num_years,
            flock_size = as.integer(input$flock_size * (1 + input$growth/100 - input$mortality/100) ^ year),
            viable_hens = flock_size * input$perc_laying/100,
            spent_hens = flock_size - viable_hens,
            num_eggs = as.integer(flock_size * input$perc_laying/100 * input$eggs_laid),
            broken_eggs = as.integer(num_eggs * input$breakage/100),
            viable_eggs = num_eggs - broken_eggs,
            revenue_eggs = viable_eggs * input$price_egg,
            revenue_spent = viable_eggs * input$price_spent,
            revenue_manure = viable_eggs * input$price_manure,
            revenue_total = revenue_eggs + revenue_spent + revenue_manure,
            cost_feed = flock_size * input$cost_feed,
            cost_labor = flock_size * input$cost_labor,
            cost_pullet = flock_size * input$cost_pullet,
            cost_equip = flock_size * input$cost_equip,
            cost_litter = flock_size * input$cost_litter,
            cost_vet = flock_size * input$cost_vet,
            cost_variable_total = cost_feed + cost_labor + cost_pullet + cost_equip + cost_litter + cost_vet,
            cost_land = input$cost_land,
            cost_office = input$cost_office,
            cost_fixed_total = cost_land + cost_office,
            cost_total = cost_variable_total + cost_fixed_total,
            profit = revenue_total - cost_total
        )})
            
    output$fc = renderReactable(reactable(fc()))
    output$g = renderPlotly(
        fc() %>%
            pivot_longer(cols = 2:ncol(fc())) %>%
            mutate(facet = case_when(
                name %in% c("flock_size", "viable_hens", "spent_hens") ~ "flock", 
                name %in% c("num_eggs", "broken_eggs", "viable_eggs") ~ "eggs",  
                name %in% c("revenue_eggs", "revenue_spent", "revenue_manure") ~ "revenue",
                name %in% c("cost_feed", "cost_feed", "cost_labor", "cost_pullet", "cost_equip", "cost_litter", "cost_vet", "cost_variable_total") ~ "variable costs",
                name %in% c("cost_land", "cost_office", "cost_fixed_total") ~ "fixed costs",
                name %in% c("cost_total", "revenue_total", "profit") ~ "total",
                T ~ "error!")) %>%
            ggplot() + 
            aes(x=year, y=value, color=name) + 
            facet_wrap(~facet, ncol=1, scales='free_y') +
            geom_line() + geom_point() +
            scale_x_continuous(breaks=fc()$year) + 
            scale_y_continuous(labels = scales::comma) +
            labs(y=NULL)
    )
}

# Run the application
shinyApp(ui = ui, server = server)