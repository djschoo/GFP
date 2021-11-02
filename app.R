library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(readxl)
library(reactable)
options(scipen = 999)
options(warn=-1)

`%,%` = function(a,b) paste0(a,b)

calc_hens = function(v, p, i) {
  if (is.na(p)) return(0)
  t = v[1]
  for (i in 1:length(v)) {
    if (is.na(v[i])) t[i] = t[i-1] * p else t[i] = v[i]
  }
  return(t)
}

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
            pickerInput("country", selected="", info_icon("Your Country", blurbs$country), multiple = F, choices = countries, choicesOpt = list(content = mapply(countries, flags, FUN = function(country, flagUrl) {HTML(paste(tags$img(src=flagUrl, width=20, height=15), country))}, SIMPLIFY = FALSE, USE.NAMES = FALSE))),
            
            h3("Basic Statistics"),
            numericInput("flock_size", info_icon("Initial Size of the Flock", blurbs$flock_size), value = NULL, min=0, step=1000),
            numericInputIcon("mortality", info_icon("Mortality Rate", blurbs$mortality), value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))),
            numericInputIcon("period_length", info_icon("Period Length for Hens to Lay (Months)"), value = NULL, min=0, max=20, step=1),
            numericInputIcon("transition_length", info_icon("Down Time Between Flocks (Months)"), value = NULL, min=0, max=20, step=1),
            numericInputIcon("new_hens", info_icon("Number of Hens Purchased Each Period"), value = NULL, min=0, step=1000),
            numericInputIcon("breakage", info_icon("Percentage of Eggs that Break"), value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))),
            
            h3("Revenues"),
            numericInputIcon("price_egg", info_icon("Selling Price per Egg"), value = NULL, min=0, max=10, step=.5, icon = icon("dollar", verify_fa=F)),
            numericInputIcon("price_spent", info_icon("Price per Spent Hen"), value = NULL, min=0, max=10, step=.5, icon = icon("dollar", verify_fa=F)),
            numericInputIcon("revenue_manure", info_icon("Revenue from Manure per Period"), value = NULL, min=0L, step=1, icon = icon("dollar", verify_fa=F)),
            
            h3("Variable Yearly Costs"),
                numericInputIcon("cost_feed", info_icon("Feed"), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F)),
                numericInputIcon("cost_labor", info_icon("Labour"), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F)),
                #numericInputIcon("cost_pullet", info_icon("Pullets"), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F)),
                numericInputIcon("cost_equip", info_icon("Equipment & Maintenance"), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F)),
                numericInputIcon("cost_litter", info_icon("Litter"), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F)),
                numericInputIcon("cost_vet", info_icon("Vaccinations/Veterinary Care"), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F)),
            
            h3("Fixed Yearly Costs"),
            numericInputIcon("cost_land", info_icon("Land"), value = NULL, min=0, max=50000, step=.5, icon = icon("dollar", verify_fa=F)),
            numericInputIcon("cost_office", info_icon("Office Rental"), value = NULL, min=0, max=50000, step=.5, icon = icon("dollar", verify_fa=F))),
        
        # Main panel for displaying outputs
        mainPanel(
            conditionalPanel(
                condition = "input.country != ''",
                tabsetPanel(
                    tabPanel("Monthly Table",
                             p("Note: we will delete this table in the final product"),
                             downloadButton("d_monthly", "Download Data"),
                             reactableOutput("t_monthly")),
                    
                    tabPanel("Revenues",
                             plotlyOutput("g_revenue"),
                             downloadButton("d_revenue", "Download Data"),
                             reactableOutput("t_revenue")),
                    
                    tabPanel("Costs",
                             plotlyOutput("g_cost"),
                             downloadButton("d_cost", "Download Data"),
                             reactableOutput("t_cost")),
                    
                    tabPanel("Profits",
                             plotlyOutput("g_profit"),
                             downloadButton("d_profit", "Download Data"),
                             reactableOutput("t_profit")))))
    )
)

# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$country, {
    defaults = defaults_all %>% filter(country == input$country)
    for (var in event_vars) updateNumericInput(session, inputId = var, value = filter(defaults, variable==var) %>% pull(value))
    for (var in currency_vars) updateNumericInputIcon(session, inputId = var, icon = currencies[match(input$country, countries)])
  })
    
  observe(if(input$country != "") {

  survival = reactive({(1 - input$mortality / 100) ^ (1/(input$period_length - 1))})
  
  monthly = reactive({
    tibble(month = 1:(12 * (input$num_years))) %>%
                mutate(
                    year = ceiling(month / 12),
                    period = ceiling(month / input$period_length),
                    period_rank = (month - 1) %% (input$period_length + input$transition_length) + 1,
                    is_transition = period_rank > input$period_length,
                    num_hens = as.double(input$flock_size),
                    num_hens = case_when(
                        month == 1 ~ num_hens,
                        period_rank == 1 ~ as.double(input$new_hens),
                        is_transition ~ 0.0),
                    num_hens = calc_hens(num_hens, survival()),
                    num_eggs = num_hens * 30.5 * (1 - input$breakage / 100),
                    revenue_eggs = num_eggs * input$price_egg,
                    revenue_spent = case_when(period_rank == input$period_length + 1 ~ lag(num_hens) * input$price_spent, T ~ 0),
                    revenue_manure = case_when(period_rank == input$period_length + 1 ~ as.double(input$revenue_manure), T ~ 0.0),
                    cost_feed = num_hens * input$cost_feed,
                    cost_labor = num_hens * input$cost_labor,
                    cost_equip = num_hens * input$cost_equip,
                    cost_litter = num_hens * input$cost_litter,
                    cost_vet = num_hens * input$cost_vet
                ) %>%
                ungroup()})
        output$t_monthly = renderReactable(reactable(monthly(), defaultPageSize = 30, highlight = T))
        output$d_monthly = downloadHandler(filename = "monthly_data.csv", content = function(file) write.csv(monthly(), file, row.names = FALSE))
        
        yearly = reactive({
            monthly() %>%
                group_by(year) %>%
                summarise_at(c("revenue_eggs", "revenue_spent", "revenue_manure", "cost_feed", "cost_labor", "cost_equip", "cost_litter", "cost_vet"), sum) %>%
                ungroup() %>%
                mutate(
                    cost_land = input$cost_land,
                    cost_office = input$cost_office,
                    revenue_total = revenue_eggs + revenue_spent + revenue_manure,
                    variable_cost_total = cost_feed + cost_labor + cost_equip + cost_litter + cost_vet,
                    fixed_cost_total = cost_land + cost_office,
                    cost_total = variable_cost_total + fixed_cost_total,
                    profit = revenue_total - cost_total
                )})
            
        revenue = reactive(yearly() %>% select(year, revenue_eggs, revenue_spent, revenue_manure, revenue_total) %>% setNames(c("Year", "Eggs", "Spent Hens", "Manure", "Total Revenue")))
        output$t_revenue = renderReactable(reactable(revenue() %>% mutate(across(2:5, scales::comma)), highlight=T))
        output$g_revenue = renderPlotly(pl(revenue() %>% select(-`Total Revenue`), ylabel="Revenue"))
        output$d_revenue = downloadHandler(filename = "revenue_data.csv", content = function(file) write.csv(revenue(), file, row.names = FALSE))
        
        cost = reactive(yearly() %>% select(year, starts_with("cost_")) %>% setNames(c("Year", "Feed", "Labor", "Equipment", "Litter", "Veterinarian/Vaccine", "Land", "Office", "Total Cost")))
        output$t_cost = renderReactable(reactable(cost() %>% mutate(across(2:8, scales::comma)), highlight=T))
        output$g_cost = renderPlotly(pl(cost() %>% select(-`Total Cost`), ylabel="Cost"))
        output$d_cost = downloadHandler(filename = "cost_data.csv", content = function(file) write.csv(cost(), file, row.names = FALSE))
        
        profit = reactive(yearly() %>% select(year, cost_total, revenue_total, profit) %>% setNames(c("Year", "Total Cost", "Total Revenue", "Total Profit")))
        output$t_profit = renderReactable(reactable(profit() %>% mutate(across(2:4, scales::comma)), highlight=T))
        output$g_profit = renderPlotly(pl(profit(), ylabel="Total Cost/Revenue/Profit"))
        output$d_profit = downloadHandler(filename = "profit_data.csv", content = function(file) write.csv(profit(), file, row.names = FALSE))
      
  })

}

# Run the application
shinyApp(ui = ui, server = server)