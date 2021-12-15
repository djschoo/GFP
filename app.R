blurbs = list(
  num_years = "blah blah blah",
  country = "blah blah blah",
  flock_size = "blah blah blah",
  mortality = "blah blah blah",
  lay_percent = "blah blah blah"
)

library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(readxl)
library(reactable)
library(stringi)
options(scipen = 999)
options(warn=-1)

# FUNCTIONS

`%,%` = function(a,b) paste0(a,b)

calc_hens = function(v, p, i) {
  if (is.na(p)) return(0)
  t = v[1]
  for (i in 1:length(v)) {
    if (is.na(v[i])) t[i] = t[i-1] * p else t[i] = v[i]
  }
  return(t)
}

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

# IMPORT COUNTRY INFO

countries = read_excel("www/countries.xlsx", sheet="Sheet1") %>% arrange(country)
countries_all = countries$country %>% unique()
currencies = select(countries, country, starts_with("currency"))
flags = c("", sapply(countries$flag_symbol, function(x) "https://cdn.jsdelivr.net/gh/lipis/flag-icon-css@master/flags/4x3/" %,% x %,% ".svg"))
countries = select(countries, -starts_with(c("flag", "currency"))) %>%
  pivot_longer(cols=-1, names_to = "variable") %>%
  arrange(country, variable)
event_vars = countries$variable %>% unique()
currency_vars = event_vars[grep("^(cost)|(revenue)|(price)", event_vars)]

# DEFINE UI

ui <- fluidPage(
    
    # App title
    titlePanel("Egg Calculator"),
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
        
        # Sidebar to demonstrate various slider options
        sidebarPanel(
            pickerInput("country", selected="", info_icon("Your Country", blurbs$country), multiple = F, choices = c("", countries_all), choicesOpt = list(content = mapply(c("", countries_all), flags, FUN = function(country, flagUrl) {HTML(paste(tags$img(src=flagUrl, width=20, height=15), country))}, SIMPLIFY = FALSE, USE.NAMES = FALSE))),
            sliderInput("num_years", label=info_icon("Number of Years to Forecast", blurbs$num_years), value = 10, min=1, max=50, step=1),
            
            h3("Basic Statistics"),
            numericInputIcon("flock_size", info_icon("Initial Size of the Flock", blurbs$flock_size), value = NULL, min=0, step=1000),
            numericInputIcon("mortality", info_icon("Mortality Rate", blurbs$mortality), value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))),
            numericInputIcon("period_length", info_icon("Period Length for Hens to Lay (Months)"), value = NULL, min=0, max=20, step=1),
            numericInputIcon("transition_length", info_icon("Down Time Between Flocks (Months)"), value = NULL, min=0, max=20, step=1),
            numericInputIcon("new_hens", info_icon("Number of Hens Purchased Each Period"), value = NULL, min=0, step=1000),
            numericInputIcon("breakage", info_icon("Percentage of Eggs that Break"), value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))),
            numericInputIcon("lay_percent", info_icon("Percentage of Hens that Lay Eggs"), value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))),
            
            h3("Revenues"),
            numericInputIcon("price_egg", info_icon("Selling Price per Egg"), value = NULL, min=0, max=10, step=.5, icon = icon("dollar", verify_fa=F)),
            numericInputIcon("price_spent", info_icon("Price per Spent Hen"), value = NULL, min=0, max=10, step=.5, icon = icon("dollar", verify_fa=F)),
            numericInputIcon("revenue_manure", info_icon("Revenue from Manure per Period"), value = NULL, min=0L, step=1, icon = icon("dollar", verify_fa=F)),
            
            h3("Variable Yearly Costs"),
                numericInputIcon("cost_feed", info_icon("Feed"), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F)),
                numericInputIcon("cost_labor", info_icon("Labour"), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F)),
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
    defaults = countries %>% filter(country == input$country)
    for (var in event_vars) updateNumericInput(session, inputId = var, value = filter(defaults, variable==var) %>% pull(value))
    for (var in currency_vars) {
      unicode = filter(currencies, country == input$country) %>% pull(currency_unicode)
      updateNumericInputIcon(session, inputId = var, icon = list(stri_unescape_unicode(gsub("\\U","\\u", unicode, fixed=TRUE))))
    }
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
        num_eggs = num_hens * 30.5 * (1 - input$breakage / 100) * input$lay_percent,
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

    revenue = reactive(
      yearly() %>% 
        select(year, revenue_eggs, revenue_spent, revenue_manure, revenue_total) %>% 
        setNames(c("Year", "Eggs", "Spent Hens", "Manure", "Total Revenue")))
    
    output$t_revenue = renderReactable(reactable(revenue(), highlight=T, columns = list(Year = colDef(format = colFormat())), defaultColDef = colDef(format = colFormat(currency = filter(currencies, country == input$country) %>% pull(currency_text), separators = TRUE, locale=filter(currencies, country == input$country) %>% pull(currency_locale)))))
    output$g_revenue = renderPlotly(pl(revenue() %>% select(-`Total Revenue`), ylabel=filter(currencies, country == input$country) %>% pull(currency_text)))
    output$d_revenue = downloadHandler(filename = "revenue_data.csv", content = function(file) write.csv(revenue(), file, row.names = FALSE))
    
    cost = reactive(yearly() %>% select(year, starts_with("cost_")) %>% setNames(c("Year", "Feed", "Labor", "Equipment", "Litter", "Veterinarian/Vaccine", "Land", "Office", "Total Cost")))
    output$t_cost = renderReactable(reactable(cost(), highlight=T, columns = list(Year = colDef(format = colFormat())), defaultColDef = colDef(format = colFormat(currency = filter(currencies, country == input$country) %>% pull(currency_text), separators = TRUE, locale=filter(currencies, country == input$country) %>% pull(currency_locale)))))
    output$g_cost = renderPlotly(pl(cost() %>% select(-`Total Cost`), ylabel=filter(currencies, country == input$country) %>% pull(currency_text)))
    output$d_cost = downloadHandler(filename = "cost_data.csv", content = function(file) write.csv(cost(), file, row.names = FALSE))

    profit = reactive(yearly() %>% select(year, cost_total, revenue_total, profit) %>% setNames(c("Year", "Total Cost", "Total Revenue", "Total Profit")))
    output$t_profit = renderReactable(reactable(profit(), highlight=T, columns = list(Year = colDef(format = colFormat())), defaultColDef = colDef(format = colFormat(currency = filter(currencies, country == input$country) %>% pull(currency_text), separators = TRUE, locale=filter(currencies, country == input$country) %>% pull(currency_locale)))))
    output$g_profit = renderPlotly(pl(profit(), ylabel=filter(currencies, country == input$country) %>% pull(currency_text)))
    output$d_profit = downloadHandler(filename = "profit_data.csv", content = function(file) write.csv(profit(), file, row.names = FALSE))

  })

}

# Run the application
shinyApp(ui = ui, server = server)