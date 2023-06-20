
library(shiny)
library(shinyBS)
library(magrittr)



if(Sys.info()[[1]] == "Windows"){ #For testing on Windows
  
  sapply(as.character(fs::dir_ls(
    stringr::str_c(here::here(), "/app/scripts")
  )), source)
} else { # For Linux Production
  sapply(as.character(fs::dir_ls(stringr::str_c(
    here::here(), "/scripts"
  ))), source)
}


options(spinner.color = main_color,
        spinner.type = 8)


shinyUI(
  
  shinydashboardPlus::dashboardPage(     
    shinydashboardPlus::dashboardHeader(
      title = HTML(glue::glue(
        '<span class="logo-mini"><em>p</em><strong>e</strong></span>
           <span class="logo-lg"><em>price </em><strong>estimation</strong></span>'
      )), 
      leftUi = tagList(
        shiny::dateInput(inputId = "val_date",
                         label = "Select Valuation Date",
                         value = lubridate::as_date("2023-06-19"),
                         format = "dd.mm.yyyy",
                         daysofweekdisabled = c(0,6))
    
      )
    ),
    
    # sidebar ----------------------------------------------------------------
    
    shinydashboardPlus::dashboardSidebar(
      collapsed = TRUE,
      # workaround for reactive sidebar collapsing        
      tags$script("$(document).on('click', '.sidebar-toggle', function () {
                      Shiny.onInputChange('SideBar_col_react', Math.random())});"),
      
      shinydashboard::sidebarMenu(
        id = "mysidebar",
        
        br(),
        shiny::imageOutput("picture", height  = "auto"),
        br(),
        
        # 1st Menu:
        shinydashboard::menuItem(strong("Ticker Selection"),
                                 tabName = "prc_est",
                                 icon = icon("bars-staggered"))
        
      )
    ),
    
    
    # Body --------------------------------------------------------------------
    shinydashboard::dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", 
                  type = "text/css", 
                  href = "custom.css"),
        tags$style("@import url(https://use.fontawesome.com/releases/v6.1.1/css/all.css);")
      ),
      
      # Website metadata for eg link preview
      metathis::meta() %>%
        metathis::meta_social(
          title = "Financial Market Dashboard",
          description = "a simple price estimation using forecasted PE-eps ratios",
          url = "https://aikia.org/simple_price_estimation/",
          image = "https://aikia.org/images/logo_aikia.png",
          image_alt = "aikia logo",
          twitter_card_type = "summary",
          twitter_site = "@aikia_lab",
          og_site_name = "https://aikia.org"
        ),
      
      
      # PE EPS Overview ----------------------------------------------
      
      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "prc_est",
                                h2("Using forecasted earnings-per-shares with avaialble PE ratios"),

                                shiny::fluidRow(
                                  shiny::h3("bubble size gives current price vs calculated price difference"),
                                  
                                  shiny::column(width = 4,
                                                shiny::selectInput("choose_pe", 
                                                                   label = "choose an available PE ratio to scale current EPS estimations", 
                                                                   choices = c('Trailing PE' = 'trailing_pe',
                                                                               'Forward PE' = 'forward_pe',
                                                                               'PE long term' = 'pe_long_term',
                                                                               'PE last 5 Yr' = 'pe_last_5_yr'),
                                                                   selected = 'Forward PE')
                                  ),
                                  shiny::column(width = 4,
                                                shiny::sliderInput('scale_pe', "scale bubble size to see all tickers", 1, 20, 10)
                                  )
                                
                                ),
                                  br(), #br(),br(), br(),
                                shiny::fluidRow(
                                              plotly::plotlyOutput("px_est", height = "600px") %>%
                                                shinycssloaders::withSpinner()
                                )
        )
      )
    )
  )
)
        
        
        
        
        
        
        
        
        
        