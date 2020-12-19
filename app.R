#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("helpers.R")

list.of.packages <- c("shiny", "xlsx", "extrafont", "assertthat", "gridExtra", "officer", "DT", "qdap")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(magrittr)
library(xlsx)
library(ggplot2)
library(extrafont)
library(data.table)
library(lubridate)
library(extrafont)
library(assertthat)
library(stringr)
library(gridExtra)
library(officer)
library(DT)


options(repos = structure(c("https://mran.microsoft.com/snapshot/2017-09-01", "http://www.stats.ox.ac.uk/pub/RWin",
                            "file:///users/shared/ra/cag-cran/"), .Names = c("CRAN", "CRANextra",
                                                                             "CAG")))
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
      }
    "))
  ),
  
  # Application title
  titlePanel("After Tax Returns"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 3, 
                 shiny::downloadButton(outputId = "xlsx",
                                       label = "Download Portfolio Template",
                                       icon = shiny::icon("download")),
                 fileInput(inputId = "port_file", label = "Input Portfolios:"),
                 shiny::checkboxGroupInput(inputId = "time_periods", 
                                           label = "Select Periods", 
                                           choices = c("1", "3", "5", "7", "10"), 
                                           selected = c("1", "3", "5", "7", "10")),
                 shiny::downloadButton(outputId = "allData",
                                       label = "Download All Data",
                                       icon = shiny::icon("download"))
                 
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(selected = "Info",
                  tabPanel("Info", 
                           h3(p("Input a portfolio file to begin.  Download the template for an example", 
                                br(), br(),
                                "Assumes F-2 shares for all portfolios",
                                br())),
                           verbatimTextOutput("test_file"),
                           p("Note: portfolio after-tax returns are weighted averages of the underlying funds. ", 
                             br(), br(),
                             "Backfilled after-tax returns approximated with the median of the rolling x-year tax efficiency (ratio of pre/post to total return) of each fund.
                       If the median is not available, then a comparable fund's tax-efficiency is used."),
                           dataTableOutput("backfill_details", width = "1024")),
                  tabPanel("Portfolio Table",  dataTableOutput("portfolio_table", width = "1024", height = "900")),
                  tabPanel("Absolute Rolling", plotOutput("absolutePlot", width = "1024", height = "900")), 
                  tabPanel("Relative Rolling", plotOutput("relativePlot", width = "1024", height = "900")), 
                  tabPanel("Tax Efficiency", plotOutput("tax_efficiency", width = "1024", height = "900")), 
                  plotOutput("distPlot")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  downloadable_data <- NULL
  
  values <- reactiveVal(NULL)
  
  get_portfolios <- reactive({
    portfolio_file <- input$port_file
    print(portfolio_file)
    if(!is.null(portfolio_file)) {
      
      fread(input$port_file$datapath)
    } else {
      return(NULL)
    }
  })
  
  get_select_periods <- reactive({
    print(input$time_periods)
    input$time_periods
  })
  
  get_port_data <- reactive({
    print(input$port_file)
    if(is.null(input$port_file)) {
      return(NULL)
    }
    port_returns <- get_portfolios() %>% 
      # .[ , paste0("base") := get(names(.)[2])] %>% 
      melt(., id.var = c("abbreviation"),
           variable.name = "portfolio", 
           value.name = "weight", 
           variable.factor = F) %>% 
      merge(., returns[ , .(abbreviation, date, time_period, return_type, return)], by = "abbreviation", allow.cartesian = T) %>% 
      .[time_period != "1m_c"] %>% 
      .[weight != 0] %>% 
      .[ , weighted_return := weight * return] %>% 
      # .[ , base_weighted := base * return] %>% 
      .[ , total_weight := sum(weight), by = c("portfolio", "date", "time_period", "return_type")] %>% 
      .[total_weight == 1] %>%
      .[ , .(return = sum(weighted_return)), by = c("portfolio", "date", "time_period", "return_type")] %>% 
      .[ , time_period := gsub("[^0-9]+", "", time_period)] %>% 
      .[ , time_period := factor(time_period, levels = c("1", "3", "5","7", "10"))] %>% 
      .[ , return_type := factor(return_type, levels = c("TR", "Pre-Liq", "Post_Liq"))] %>% 
      # .[ , relative_return := return - base_return] %>% 
      # .[ , min_date_by_portfolio := min(date), by = c("portfolio", "time_period")] %>% 
      split(by = "time_period") %>% 
      # lapply(., function(x) x[date >= max(min_date_by_portfolio)]) %>% 
      rbindlist()
    
    port_returns[]
  })
  
  output$test_file <- shiny::renderText({
    
    if(is.null(get_portfolios())) {
      return("Upload portfolio file.  Calculations take time.")
    }
    
    test <- get_portfolios() %>% 
      melt(., id.var = "abbreviation") %>% 
      .[ , .(summed_weights = sum(value)), by = "variable"] %>% 
      .[ , summed_weights] %>% 
      unique()
    
    if(test == 1 & length(test) == 1) {
      return("Ready for download or reupload new portfolios.")
    } else {
      "Constituents need to add to 1 for each portfolio"
    }
    
    
  })
  
  
  observeEvent(input$port_file, {
    
    withProgress({
      incProgress(amount = 0.1, message = paste("Calculating data..."))  
      
      funds <- get_portfolios() %>% 
        melt(., id.var = "abbreviation", value.factor = F) %>% 
        .[value != 0] %>% 
        .[ , abbreviation] %>% 
        unique()
      
      calc_returns <- get_portfolios() %>% 
        merge(., monthly_returns[abbreviation %in% funds][ , .(abbreviation, return, date)], by = c("abbreviation"), all.x = T) %>% 
        melt(., id.var = c("abbreviation", "date", "return"), variable.factor = F, variable.name = "portfolio", value.name = "weight") %>% 
        # .[weight != 0] %>%
        .[ , total_weight := sum(weight), by = c("portfolio", "date")] %>% 
        .[total_weight == 1] %>% 
        setkey(portfolio, abbreviation, date) %>% 
        .[ , weighted_return := return * weight] %>% 
        .[ , portfolio_return := sum(weighted_return), by = c("portfolio", "date")]
      
      fund_rets <- calc_returns[ , .(abbreviation, date, return)] %>% 
        unique() %>% 
        setkey(abbreviation, date)
      
      fund_at <- returns[abbreviation %in% funds] %>% 
        .[time_period != "1m_c"] %>% 
        .[ , .(abbreviation, date, metric = return_type, value = return, time_period)] %>% 
        .[ , time_period := stringr::str_extract(time_period,"[0-9]+")] %>% 
        .[ , time_period := factor(time_period, levels = c("1", "3", "5","7", "10"))] 
      
      port_rets <- calc_returns[ , .(abbreviation = portfolio, return = portfolio_return, date)] %>% 
        unique() %>% 
        setkey(abbreviation, date)
      
      ports <- get_port_data()
      
      downloadable_data <<- lapply(list(fund_rets, port_rets), tr_stats) %>% 
        rbindlist(., use.names = T) %>% 
        rbind(., ports[ , .(abbreviation = portfolio, date, time_period, metric = return_type, value = return)], fund_at, use.names = T, fill = T) %>% 
        unique()
      
    })
    
    values(NULL)
  })
  
  get_tax_efficiency <- function(DT) {
    tax <- DT %>% 
      dcast(portfolio + date + time_period ~ return_type, value.var = "return") %>% 
      .[ , tax_eff := Post_Liq/TR]
  }
  
  output$portfolio_table <- renderDataTable({
    
    portfolios <- get_portfolios()
    
    if(is.null(portfolios)) {
      verbatimTextOutput("Input a file")
    } else {
      DT::datatable(portfolios)
    }
    
    
  })
  
  output$backfill_details <- renderDataTable({
    
    DT::datatable(back_deets, options = list(dom = "t"), rownames = F)
  })
  
  output$absolutePlot <- renderPlot({
    dat <- get_port_data()
    
    if(is.null(dat)) {
      verbatimTextOutput("Input a file")
    } else {
      ggplot(dat[time_period %in% get_select_periods()], aes(x = date, y = return, color = portfolio)) +
        geom_line() +
        facet_wrap(~ time_period + return_type, ncol = 3, scales = "free_x") +
        scale_y_continuous(labels = scales::percent_format())
    }
    
    
  })
  
  output$relativePlot <- renderPlot({
    dat <- get_port_data()
    
    if(is.null(dat)) {
      verbatimTextOutput("Input a file")
    } else {
      ggplot(dat[time_period %in% get_select_periods()], aes(x = date, y = relative_return, color = portfolio)) +
        geom_line() +
        geom_hline(yintercept = 0, color = "red") +
        facet_wrap(~ time_period + return_type, ncol = 3, scales = "free_x") +
        scale_y_continuous(labels = scales::percent_format())
    }
    
    
  })
  
  output$tax_efficiency <- renderPlot({
    dat <- get_port_data() 
    
    if(is.null(dat)) {
      verbatimTextOutput("Input a file")
    } else {
      tax_data <- get_port_data() %>% 
        get_tax_efficiency() %>% 
        .[tax_eff <= 1 & tax_eff > 0]
      
      ggplot(tax_data[time_period %in% get_select_periods()], aes(x = date, y = tax_eff, color = portfolio)) +
        geom_line() +
        geom_hline(yintercept = 0, color = "red") +
        facet_wrap(~ time_period, ncol = 1, scales = "free_x") +
        scale_y_continuous(labels = scales::percent_format())
    }
    
    
  })
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$xlsx <- downloadHandler(
    filename = function() paste0("portfolio_template.csv"),
    content = function(file){
      fwrite(template, file)
    })
  
  output$allData <- downloadHandler(
    filename = function() paste0("after_tax_data.csv"),
    content = function(file){
      fwrite(downloadable_data, file, col.names = T)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)