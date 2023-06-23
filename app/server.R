

library(shiny)
library(shinyBS)
library(magrittr)
library(DBI)



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
  
  # Temprorary Initial load of data Frames
  mydb <- connect_to_DB()
  
  tic_estimates <- DBI::dbGetQuery(conn = mydb,
                                   "SELECT *
                                   FROM fin_ticker_market_estimation
                                   WHERE `Estimation eps (act yr)` > 0") %>% 
    dplyr::as_tibble()
  
  tic_names <- DBI::dbGetQuery(mydb,"SELECT *
                                    FROM fin_ticker_meta_data") %>% 
    dplyr::as_tibble()
  
  DBI::dbDisconnect(mydb)
  
  # select dates for selection input
  all_dates <- tic_estimates %>% dplyr::pull(retrieval_date) %>% unique() %>% as.character()
  
  
  df_prep <- tic_estimates %>%
    dplyr::left_join(tic_names,#[,c("ticker_yh","name","country","bics_level_1_sector_name")],
                     by = "ticker_yh") %>% 
    janitor::clean_names()
  
  
  
  
  
  # Update to last available date
  shinyWidgets::updatePickerInput(session = session,
                    inputId = "date_selection",
                    choices = all_dates,
                    selected = all_dates[1]
  )
  
# Resizing aikia logo -----------------------------------------------------
  
  # reactive value 4 sidebar collapsing
  vals<-reactiveValues()
  vals$collapsed=FALSE
  observeEvent(input$SideBar_col_react,{
    vals$collapsed=!vals$collapsed
  })
  
  # reactive logo   
  size <- reactive({
    if(vals$collapsed){
      return("120px") 
    } else {
      return("50px")
    }
  }) 
  
  
  
  output$picture <- shiny::renderImage({
    
    return(list(src = "www/aikia_logo.svg", 
                contentType = "image/svg+xml", 
                width = size(),
                height = size(),
                alt = "Analytics")) #style = 'position: absolute; left: 50%; transform: translateX(-50%);'
    
  },
  deleteFile = FALSE) 
  
# Plotly Bubble Plot ----------------------------------------------------------
  
  get_tic_forecasts <- shiny::reactiveVal(NULL)
  
  # Reactive Value estimation df
  shiny::observe({
    px_est_plotly <- px_est_fun(new_est = df_prep,
                                input$date_selection,
                                input$choose_pe,
                                input$scale_bubble
                                #input$scale_pe
                              )
    plotly::event_register(px_est_plotly, "plotly_click")
    
    if(exists("click_data")){rm(click_data)}
    
    get_tic_forecasts(px_est_plotly)
  })
  
  
  
  
  # Output estimation bubble plot
  output$px_est <- plotly::renderPlotly({
    get_tic_forecasts()
  })
  
})
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  