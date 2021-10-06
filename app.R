library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(readxl)
library(reactable)
options(scipen = 999)

countries = c("", "China", "Vietnam", "Cambodia")
flags = c("", "cn", "vn", "kh")
flags = sapply(flags, function(x) paste0("https://cdn.jsdelivr.net/gh/lipis/flag-icon-css@master/flags/4x3/", x, ".svg"))
flags[1] = ""

defaults_all = read_xlsx("data/country_defaults.xlsx")
event_vars = defaults_all$variable %>% unique()

pl = function(df, l=scales::comma) {
    df %>%
    pivot_longer(cols = 2:ncol(df)) %>%
    ggplot() + 
    theme_light() +
    aes(x=`Year`, y=value, color=name) + 
    geom_line() + geom_point() +
    
    scale_x_continuous(breaks=df$`Year`) + 
    scale_y_continuous(labels = l) +
    labs(y=NULL, color=NULL)
}

# Define UI
ui <- fluidPage(
    
    # App title
    titlePanel("Egg Calculator"),
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
        
        # Sidebar to demonstrate various slider options
        sidebarPanel(
            sliderInput("num_years", "How many years in the future would you like to forecast?", value = 10, min=1, max=50, step=1),
            pickerInput("country", "What country are you from?", multiple = F, choices = countries, choicesOpt = list(content = mapply(countries, flags, FUN = function(country, flagUrl) {HTML(paste(tags$img(src=flagUrl, width=20, height=15), country))}, SIMPLIFY = FALSE, USE.NAMES = FALSE))),
            p("When you pick a country in the box above, we will estimate some of your costs/revenues and fill them in below"),
            
            h3("Basic stats"),
            fluidRow(
                column(6, numericInput("flock_size", "What is the initial flock size?", value = NULL, min=0, step=1000)),
                column(6, numericInputIcon("mortality", "What is the mortality rate of your flock?", value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))))),
            fluidRow(
                column(6, numericInputIcon("growth", "What is the expected growth rate of your flock?", value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent")))),
                column(6, numericInputIcon("perc_laying", "What percentage of your flock lays eggs?", value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))))),
            fluidRow(
                column(6, numericInput("eggs_laid", "How many eggs does each hen lay?", value = NULL, min=0, step=1000)),
                column(6, numericInputIcon("breakage", "What percentage of eggs break?", value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))))),

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
            conditionalPanel(
                condition = "input.country != ''",
                tabsetPanel(
                    tabPanel("Flock",
                    #h4("Flock"),
                    plotlyOutput("g_flock"),
                    reactableOutput("t_flock")),
                    
                    tabPanel("Eggs",
                    plotlyOutput("g_eggs"),
                    reactableOutput("t_eggs")),
                    
                    tabPanel("Revenues",
                    plotlyOutput("g_revenues"),
                    reactableOutput("t_revenues")),
                    
                    tabPanel("Totals",
                    plotlyOutput("g_totals"),
                    reactableOutput("t_totals"))
                )
            )
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    defaults = reactive(defaults_all %>% filter(country == input$country))
    observeEvent(input$country, {for (var in event_vars) updateNumericInput(session, inputId = var, value = filter(defaults(), variable==var) %>% pull(value))})

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
    
    flock = reactive(fc() %>% select(year, flock_size, viable_hens, spent_hens) %>% setNames(c("Year", "Flock Size", "Viable Hens", "Spent Hens")))
    output$t_flock = renderReactable(reactable(flock() %>% mutate(across(2:4, scales::comma))))
    output$g_flock = renderPlotly(pl(flock()))
    
    eggs = reactive(fc() %>% select(year, num_eggs, viable_eggs, broken_eggs) %>% setNames(c("Year", "Number of Eggs", "Viable Eggs", "Broken Eggs")))
    output$t_eggs = renderReactable(reactable(eggs() %>% mutate(across(2:4, scales::comma))))
    output$g_eggs = renderPlotly(pl(eggs()))
    
    revenues = reactive(fc() %>% select(year, starts_with("revenue")) %>% setNames(c("Year", "Eggs", "Spent Hens", "Manure", "Total")))
    output$t_revenues = renderReactable(reactable(revenues() %>% mutate(across(2:5, scales::dollar))))
    output$g_revenues = renderPlotly(pl(revenues(), l = scales::dollar))
    
    totals = reactive(fc() %>% select(year, cost_total, revenue_total, profit) %>% setNames(c("Year", "Total Cost", "Total Revenue", "Total Profit")))
    output$t_totals = renderReactable(reactable(totals() %>% mutate(across(2:4, scales::dollar))))
    output$g_totals = renderPlotly(
        totals() %>%
            pivot_longer(cols = 2:4) %>%
            ggplot() + 
            theme_light() +
            aes(x=`Year`, y=value, color=name) + 
            geom_line() + geom_point() +
            geom_ribbon(aes(ymax=value*1.05, ymin=value*.95, alpha=.1, fill=name)) +
            scale_x_continuous(breaks=totals()$`Year`) + 
            scale_y_continuous(labels = scales::dollar) +
            labs(y=NULL, color=NULL)
    )
}

# Run the application
shinyApp(ui = ui, server = server)