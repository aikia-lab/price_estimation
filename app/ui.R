
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
        shinyWidgets::pickerInput(inputId = 'date_selection',
                    label = 'available Dates',
                    choices = "2023-06-19",
                    selected = "2023-06-19",
                    options = list(style = "btn-warning"))
    
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
                                h2("Track changing Market Expectations by using forecasted Earnings-per-Shares with avaialble PE ratios"),

                                shiny::fluidRow(
                                  shiny::h3("the bubble size reflects the price gap of the current market price against the estimated price"),
                                  shiny::h4("  a ",span("green ring",style = "color:green; font-size:16pt")," indicates a potential positive change, a ",
                                            span("red ring",style = "color:red; font-size:16pt")," indicates a negative outlook"),
                                  br(),
                                  
                                  
                                  tags$span("App Information", 
                                                    shinyBS::bsButton(inputId = "app_info", 
                                                                      label = "", 
                                                                      icon = icon("info"), 
                                                                      style = "info", 
                                                                      size = "extra-small")),
                                  shinyBS::bsPopover(
                                    id = "app_info",
                                    title = "App information",
                                    content = 
                                      "share prices depend on company profits <b>(eps)</b> and the willingness of shareholders to pay a premium for expected future profits <b>(PE ratio)</b>.<br><br> This app assumes various premiums and combines them with currently expected earnings.<br>If analysts set earnings expectations up or down, the theoretical market price changes and the app compares it to the current market price."
                                    ,
                                    placement = "left"#,
                                  #  trigger = "hover"#,
                                   # options = list(container = "body")
                                  ),
                                  
                                  
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
                                            #    shiny::sliderInput('scale_pe', "scale bubble size to see all tickers", 1, 20, 10)
                                                shinyThings::radioSwitchButtons(
                                                  inputId = "scale_bubble", 
                                                  label = NULL, 
                                                  choices = tibble::tibble(
                                                    'one size for all' = 1,
                                                    'size to potential gain/loss' = 2),
                                                  selected = 2,
                                                  not_selected_background = 'grey',
                                                  selected_background = main_color_light),
                                          #  shinyBS::bsPopover(
                                          #    id = "scale_bubble", title = "bubble size",placement = "bottom", content = "by scaling to potential gain/loss some bubbles will be too small to be plotted"
                                          #  )
                                            shinyBS::bsTooltip(id = "scale_bubble", title = "by scaling to potential gain/loss some bubbles will be too small to be plotted", 
                                                      placement = "bottom", trigger = "hover")
                                  
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
        
        
        
        
        
        
        
        
        
        