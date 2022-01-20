library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(readxl)
library(reactable)
options(scipen = 999)
options(warn=-1)

# FUNCTIONS

`%,%` = function(a,b) paste0(a,b)

info_icon = function(text, message="") tags$span(text, tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = message))

# IMPORT COUNTRY INFO

countries = read_excel("www/countries.xlsx")
blurbs = countries %>% select(country, blurb) %>% deframe()
countries = countries %>% select(-blurb) 
countries = bind_cols(country = names(countries), t(countries)) %>% as_tibble()
names(countries) = unlist(countries[1,])
countries = countries[2:nrow(countries), ] %>% arrange(country)
countries_all = countries$country %>% unique()
currencies = select(countries, country, starts_with("currency"))
flags = countries %>% select(country, flag_symbol)
countries = select(countries, -starts_with(c("flag", "currency"))) %>%
  pivot_longer(cols=-1, names_to = "variable") %>%
  arrange(country, variable) %>%
  mutate(value = as.double(value))
event_vars = countries$variable %>% unique()
currency_vars = event_vars[grep("^(cost)|(revenue)|(price)|(fixed)", event_vars)]

# DEFINE UI

ui <- fluidPage(
  
  # App title
  titlePanel("Egg Calculator"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options
    sidebarPanel(
      pickerInput(
        inputId="country", 
        label="Your Country", 
        multiple = F, 
        choices = flags$country, 
        options = list(title = "Pick a country!"),
        choicesOpt = list(content = purrr::map2(flags$flag_symbol, flags$country, function(flag, text) shiny::HTML(paste(tags$img(src=flag %,% ".svg", width=30, height=22), text))))),
      sliderInput("num_years", label="Number of Years to Forecast",  value = 10, min=1, max=50, step=1),
      
      h3("Basic Statistics"),
      numericInputIcon("flock_size", info_icon("Flock size (number of birds)", blurbs["flock_size"]), value = NULL, min=0, step=1000),
      numericInputIcon("mortality", info_icon("Mortality rate (%)", blurbs['mortality']), value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))),
      sliderInput("period_length", info_icon("Laying period (months)", blurbs['period_length']), value = 12, min=1, max=20, step=1),
      numericInputIcon("lay_percent", info_icon("Average rate of lay (%)", blurbs['lay_percent']), value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))),
      sliderInput("transition_length", info_icon("Down Time Between Flocks (Months)", blurbs['transition_length']), value = 2, min=1, max=10, step=1),
      numericInputIcon("breakage", info_icon("Eggs lost (%)", blurbs['breakage']), value = NULL, min=0, max=100, step=.5, icon=list(NULL, icon("percent"))),
      
      h3("Revenues"),
      numericInputIcon("price_egg", info_icon("Selling Price per Egg", blurbs['price_egg']), value = NULL, min=0, step=.5, icon = icon("dollar", verify_fa=F)),
      numericInputIcon("price_spent", info_icon("Selling price per hen", blurbs['price_spent']), value = NULL, min=0, step=.5, icon = icon("dollar", verify_fa=F)),
      numericInputIcon("revenue_manure", info_icon("Sale of manure (per flock)", blurbs['revenue_manure']), value = NULL, min=0L, step=1, icon = icon("dollar", verify_fa=F)),
      
      h3("Variable Costs"),
      numericInputIcon("cost_feed", info_icon("Feed costs (per month)", blurbs['cost_feed']), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F)),
      numericInputIcon("cost_labor", info_icon("Labour costs (per month)", blurbs['cost_labor']), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F)),
      numericInputIcon("cost_equip", info_icon("Equipment & Maintanance (per year)", blurbs['cost_equip']), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F)),
      numericInputIcon("cost_pullet", info_icon("Replacement pullets (per pullet)", blurbs['cost_pullet']), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F)),
      numericInputIcon("cost_litter", info_icon("Litter costs (per flock)", blurbs['cost_litter']), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F)),
      numericInputIcon("cost_vet", info_icon("Veterinary care and medications (per flock)", blurbs['cost_vet']), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F)),
      numericInputIcon("cost_utilities", info_icon("Utilities costs (per year)
", blurbs['cost_utilities']), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F)),
      numericInputIcon("cost_other", info_icon("Other costs (per year)", blurbs['cost_other']), value = NULL, min=0, max=5000, step=.5, icon = icon("dollar", verify_fa=F)),
      
      h3("Fixed Costs"),
      numericInputIcon("fixed_land", info_icon("Land (yearly)", blurbs['fixed_land']), value = NULL, min=0, max=50000, step=.5, icon = icon("dollar", verify_fa=F)),
      numericInputIcon("fixed_other", info_icon("Other fixed costs (yearly)", blurbs['fixed_other']), value = NULL, min=0, max=50000, step=.5, icon = icon("dollar", verify_fa=F))),
    
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

# DEFINE SERVER LOGIC
server <- function(input, output, session) {
  
  currency_text = eventReactive(input$country, filter(currencies, country==input$country) %>% pull(currency_text))
  currency_locale = eventReactive(input$country, filter(currencies, country==input$country) %>% pull(currency_locale))
  currency_symbol = eventReactive(input$country, filter(currencies, country==input$country) %>% pull(currency_symbol))
  
  observeEvent(input$country, {
    for (var in event_vars) updateNumericInput(session, inputId = var, value = filter(countries, country == input$country, variable==var) %>% pull(value))
    for (var in currency_vars) updateNumericInputIcon(session, inputId = var, icon = list(currency_symbol()))
  })
  
  observe({
    
    survival = reactive({(1 - input$mortality / 100) ^ (1/(input$period_length - 1))})
    monthly = reactive({
      tibble(month = 1:(12 * (input$num_years))) %>%
        mutate(
          year = ceiling(month / 12),
          period = ceiling(month / input$period_length),
          period_rank = (month - 1) %% (input$period_length + input$transition_length) + 1,
          is_transition = period_rank > input$period_length,
          num_hens = case_when(
            period_rank == 1 ~ as.double(input$flock_size),
            is_transition ~ 0.0,
            T ~ input$flock_size * (survival() ^ (period_rank - 1))),
          num_eggs = num_hens * 30.5 * (1 - input$breakage / 100) * input$lay_percent,
          revenue_eggs = num_eggs * input$price_egg,
          revenue_spent = case_when(period_rank == input$period_length + 1 ~ lag(num_hens) * input$price_spent, T ~ 0),
          revenue_manure = case_when(period_rank == input$period_length + 1 ~ as.double(input$revenue_manure), T ~ 0.0),
          cost_feed = case_when(!is_transition ~ as.double(input$cost_feed), T ~ 0.0),
          cost_labor = as.double(input$cost_labor),
          cost_equip = input$cost_equip / 12,
          cost_pullet = case_when(is_transition & !lead(is_transition) ~ as.double(input$cost_pullet * input$flock_size), T ~ 0.0),
          cost_litter = case_when(period_rank == 1 ~ as.double(input$cost_litter), T ~ 0.0),
          cost_vet = case_when(!is_transition ~ input$cost_vet / input$period_length, T ~ 0.0),
          cost_utilities = input$cost_utilities / 12,
          cost_other = input$cost_other / 12
        ) %>%
        ungroup()})
    
    output$t_monthly = renderReactable(reactable(monthly(), defaultPageSize = 30, highlight = T))
    output$d_monthly = downloadHandler(filename = "monthly_data.csv", content = function(file) write.csv(monthly(), file, row.names = FALSE))
    
    yearly = reactive({
      monthly() %>%
        group_by(year) %>%
        summarise_if(is.numeric, sum) %>%
        ungroup() %>%
        select(year, num_eggs, starts_with(c("revenue", "cost"))) %>%
        mutate(
          fixed_land = input$fixed_land,
          fixed_other = input$fixed_other,
          revenue_total = revenue_eggs + revenue_spent + revenue_manure,
          cost_variable_total = cost_feed + cost_labor + cost_equip + cost_pullet + cost_litter + cost_vet + cost_utilities + cost_other, 
          fixed_cost_total = fixed_land + fixed_other,
          cost_total = cost_variable_total + fixed_cost_total,
          profit = revenue_total - cost_total
        )})
    
    print(yearly())
    
    revenue = reactive(yearly() %>% 
      select(year, num_eggs, revenue_eggs, revenue_spent, revenue_manure, revenue_total) %>% 
      setNames(c("Year", "Number of Eggs", "Revenue from Eggs", "Revenue from Spent Hens", "Revenue from Manure", "Total Revenue")))
    
    output$t_revenue = renderReactable(
      reactable(
        revenue(), 
        highlight=T, 
        columns = list(
          Year = colDef(format = colFormat()),
          `Number of Eggs` = colDef(format = colFormat(locales = currency_locale()))), 
        defaultColDef = colDef(format = colFormat(currency = currency_text(), separators = T, locales=currency_locale()))))
    output$g_revenue = renderPlotly(revenue() %>%
      select(-`Total Revenue`) %>%
      pivot_longer(cols = 2:5) %>%
      mutate(facet = case_when(name == "Number of Eggs" ~ "Eggs", T ~ "Revenues")) %>%
      ggplot() +
      theme_light() +
      aes(x=`Year`, y=value, color=name) +
      geom_line() + geom_point() +
      scale_x_continuous(breaks=revenue()$`Year`) +
      scale_y_continuous(labels = scales::comma) +
      facet_wrap(~facet, scales="free_y", ncol=1) + 
      labs(y=currency_text(), color=NULL))
    
    output$d_revenue = downloadHandler(filename = "revenue_data.csv", content = function(file) write.csv(revenue(), file, row.names = FALSE))
    
    cost = reactive(yearly() %>% 
        select(year, starts_with(c("cost", "fixed"))) %>%
        select(-cost_variable_total, -fixed_cost_total) %>%
        setNames(c("Year", "Feed", "Labor", "Equipment", "Pullet", "Litter", "Veterinarian/Vaccine", "Utilities", "Other", "Land", "Other Fixed", "Total Cost")))
    output$t_cost = renderReactable(
      reactable(
        cost(), 
        highlight=T, 
        columns = list(Year = colDef(format = colFormat())), 
        defaultColDef = colDef(format = colFormat(currency = currency_text(), separators = TRUE, locales = currency_locale()))))
    output$g_cost = renderPlotly(cost() %>%
        select(-`Total Cost`) %>%
        pivot_longer(cols = 2:11) %>%
        ggplot() +
        theme_light() +
        aes(x=`Year`, y=value, color=name) +
        geom_line() + geom_point() +
        scale_x_continuous(breaks=cost()$`Year`) +
        scale_y_continuous(labels = scales::comma) +
        labs(y=currency_text(), color=NULL))
    
    output$d_cost = downloadHandler(filename = "cost_data.csv", content = function(file) write.csv(cost(), file, row.names = FALSE))
    
    profit = reactive(yearly() %>% 
      select(year, cost_total, revenue_total, profit) %>% 
      setNames(c("Year", "Total Cost", "Total Revenue", "Total Profit")))
    output$t_profit = renderReactable(
      reactable(
        profit(), 
        highlight=T, 
        columns = list(Year = colDef(format = colFormat())), 
        defaultColDef = colDef(format = colFormat(currency = currency_text(), separators = TRUE, locales=currency_locale()))))
    output$g_profit = renderPlotly(profit() %>%
         pivot_longer(cols = 2:4) %>%
         ggplot() +
         theme_light() +
         aes(x=`Year`, y=value, color=name) +
         geom_line() + geom_point() +
         scale_x_continuous(breaks=profit()$`Year`) +
         scale_y_continuous(labels = scales::comma) +
         labs(y=currency_text(), color=NULL))
      
    output$d_profit = downloadHandler(filename = "profit_data.csv", content = function(file) write.csv(profit(), file, row.names = FALSE))
  })
}

# RUN THE APPLICATION
shinyApp(ui = ui, server = server)