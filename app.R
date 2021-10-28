library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(readxl)
library(reactable)
options(scipen = 999)

`%,%` = function(a,b) paste0(a,b)

blurbs = list(
    num_years = "blah blah blah",
    country = "blah blah blah",
    flock_size = "blah blah blah",
    mortality = "blah blah blah"
)

countries_all = read_excel("www/countries.xlsx", sheet="countries")
countries = c("", countries_all$country)
flags = c("", countries_all$symbol)
currencies_df = countries_all %>% select(currency_icon, currency_text)
currencies = list(icon("dollar", verify_fa=F))
for (i in 1:(nrow(currencies_df))) {
    if (!is.na(currencies_df[i, 'currency_icon'])) {
        ic = currencies_df[[i, 'currency_icon']]
        currencies[length(currencies) + 1] = list(eval(parse(text=paste0("icon('", ic, "', verify_fa=F)"))))
    } else {
        ic = currencies_df[[i, 'currency_text']]
        currencies[length(currencies) + 1] = ic
    }
}
flags = sapply(flags, function(x) "https://cdn.jsdelivr.net/gh/lipis/flag-icon-css@master/flags/4x3/" %,% x %,% ".svg")
flags[1] = ""

defaults_all = read_excel("www/countries.xlsx", sheet="defaults")
event_vars = defaults_all$variable %>% unique()
currency_vars = c("price_egg", "price_spent", "revenue_manure", "cost_feed", "cost_labor", "cost_pullet", "cost_equip", "cost_litter", "cost_vet", "cost_land", "cost_office")

pl = function(df, l=scales::comma, ylabel=NULL) {
    df %>%
        pivot_longer(cols = 2:ncol(df)) %>%
        ggplot() +
        theme_light() +
        aes(x=`Year`, y=value, color=name) +
        geom_line() + geom_point() +
        
        scale_x_continuous(breaks=df$`Year`) +
        scale_y_continuous(labels = l) +
        labs(y=ylabel, color=NULL)
}

info_icon = function(text, message='blah blah') tags$span(text, tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = message))

# Define UI
ui <- fluidPage(
    
    # App title
    titlePanel("Egg Calculator"),
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
        
        # Sidebar to demonstrate various slider options
        sidebarPanel(
            sliderInput("num_years", label=info_icon("Number of Years to Forecast", blurbs$num_years), value = 10, min=1, max=50, step=1),
            pickerInput("country", selected="China", info_icon("Your Country", blurbs$country), multiple = F, choices = countries, choicesOpt = list(content = mapply(countries, flags, FUN = function(country, flagUrl) {HTML(paste(tags$img(src=flagUrl, width=20, height=15), country))}, SIMPLIFY = FALSE, USE.NAMES = FALSE))),
            
            h3("Basic Statistics"),
            fluidRow(
                column(6, numericInput("flock_size", info_icon("Initial Size of the Flock", blurbs$flock_size), value = NULL, min=0, step=1000)),
                column(6, numericInputIcon("mortality", info_icon("Mortality Rate", blurbs$mortality), value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))))),
            fluidRow(
                column(6, numericInputIcon("period_length", info_icon("Period Length for Hens to Lay (Months)"), value = 14, min=0, max=20, step=1)),
                column(6, numericInputIcon("transition_length", info_icon("Delay Between Selling and Buying New Hens  (Months)"), value = 2, min=0, max=20, step=1))),
            #column(6, numericInputIcon("perc_laying", info_icon("Percentage of Flock that Lays Eggs"), value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))))),
            fluidRow(
                #column(6, numericInput("eggs_laid", info_icon("Average Number of Eggs Laid per Bird"), value = NULL, min=0, step=1000)),
                column(6, numericInputIcon("new_hens", info_icon("Number of Hens Purchased Each Period"), value = 5000, min=0, step=1000)),
                column(6, numericInputIcon("breakage", info_icon("Percentage of Eggs that Break"), value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))))),
            
            h3("Revenues"),
            fluidRow(
                column(6, numericInputIcon("price_egg", info_icon("Selling Price per Egg"), value = NULL, min=0, max=10, step=.5, icon = icon("dollar", verify_fa=F))),
                column(6, numericInputIcon("price_spent", info_icon("Price per Spent Hen"), value = NULL, min=0, max=10, step=.5, icon = icon("dollar", verify_fa=F)))
            ),
            fluidRow(
                column(6, numericInputIcon("revenue_manure", info_icon("Revenue from Manure per Period"), value = NULL, min=0, max=10, step=.5, icon = icon("dollar", verify_fa=F)))
            ),
            
            h3("Variable Yearly Costs"),
            fluidRow(
                column(6, numericInputIcon("cost_feed", info_icon("Feed"), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F))),
                column(6, numericInputIcon("cost_labor", info_icon("Labour"), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F)))
            ),
            fluidRow(
                column(6, numericInputIcon("cost_pullet", info_icon("Pullets"), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F))),
                column(6, numericInputIcon("cost_equip", info_icon("Equipment & Maintenance"), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F)))
            ),
            fluidRow(
                column(6, numericInputIcon("cost_litter", info_icon("Litter"), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F))),
                column(6, numericInputIcon("cost_vet", info_icon("Vaccinations/Veterinary Care"), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F)))
            ),
            
            h3("Fixed Yearly Costs"),
            fluidRow(
                column(6, numericInputIcon("cost_land", info_icon("Land"), value = NULL, min=0, max=50000, step=.5, icon = icon("dollar", verify_fa=F))),
                column(6, numericInputIcon("cost_office", info_icon("Office Rental"), value = NULL, min=0, max=50000, step=.5, icon = icon("dollar", verify_fa=F)))
            )
        ),
        
        # Main panel for displaying outputs
        mainPanel(
            conditionalPanel(
                condition = "input.country != ''",
                tabsetPanel(
                    tabPanel("Monthly Table",
                             p("Note: we will delete this table in the final product"),
                             reactableOutput("t_monthly")),
                    
                    tabPanel("Flock",
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
    
    # can we do this in one line?
    observeEvent(input$country, {for (var in event_vars) updateNumericInput(session, inputId = var, value = filter(defaults(), variable==var) %>% pull(value))})
    observeEvent(input$country, {for (var in currency_vars) updateNumericInputIcon(session, inputId = var, icon = currencies[match(input$country, countries)])
    })
    
    survival = reactive({(1 - input$mortality / 100) ^ (1/(input$period_length - 1))})
    
    monthly = reactive({
        tibble(month = 1:(12 * (input$num_years+1))) %>%
            mutate(
                year = ceiling(month / 12),
                period = ceiling(month / input$period_length),
                period_rank = (month - 1) %% (input$period_length + input$transition_length) + 1,
                is_transition = period_rank > input$period_length,
                num_hens = as.double(input$flock_size),
                num_hens = case_when(
                    month == 1 ~ num_hens,
                    period_rank == 1 ~ as.double(input$new_hens),
                    is_transition ~ 0.0,
                    T ~ survival() * lag(num_hens))
            )})
    
    output$t_monthly = renderReactable(reactable(monthly(), defaultPageSize = 30))
    
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
            revenue_manure = viable_eggs * input$revenue_manure,
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
    output$g_flock = renderPlotly(pl(flock(), ylabel="Number of Birds"))
    
    eggs = reactive(fc() %>% select(year, num_eggs, viable_eggs, broken_eggs) %>% setNames(c("Year", "Number of Eggs", "Viable Eggs", "Broken Eggs")))
    output$t_eggs = renderReactable(reactable(eggs() %>% mutate(across(2:4, scales::comma))))
    output$g_eggs = renderPlotly(pl(eggs(), ylabel="Number of Eggs"))
    
    revenues = reactive(fc() %>% select(year, starts_with("revenue")) %>% setNames(c("Year", "Eggs", "Spent Hens", "Manure", "Total")))
    output$t_revenues = renderReactable(reactable(revenues() %>% mutate(across(2:5, scales::comma))))
    output$g_revenues = renderPlotly(pl(revenues(), ylabel="Revenues"))
    
    totals = reactive(fc() %>% select(year, cost_total, revenue_total, profit) %>% setNames(c("Year", "Total Cost", "Total Revenue", "Total Profit")))
    output$t_totals = renderReactable(reactable(totals() %>% mutate(across(2:4, scales::comma))))
    output$g_totals = renderPlotly(pl(totals(), ylabel="Total Cost/Revenue/Profit"))
    # output$g_totals = renderPlotly(
    #     totals() %>%
    #         pivot_longer(cols = 2:4) %>%
    #         ggplot() +
    #         theme_light() +
    #         aes(x=`Year`, y=value, color=name) +
    #         geom_line() + geom_point() +
    #         geom_ribbon(aes(ymax=value*1.05, ymin=value*.95, alpha=.1, fill=name)) +
    #         scale_x_continuous(breaks=totals()$`Year`) +
    #         scale_y_continuous(labels = scales::comma) +
    #         labs(y=NULL, color=NULL)
    # )
}

# Run the application
shinyApp(ui = ui, server = server)