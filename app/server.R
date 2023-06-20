

library(shiny)
library(shinyBS)
library(magrittr)
library(DBI)



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
  
  # Temprorary Initial load of data Frames
  
#  mydb <- aikia::connect_to_db(user = "ceilert",password = "ceilert")
  mydb <- connect_to_DB()
  
  tic_estimates <- DBI::dbReadTable(conn = mydb,
                                   "fin_ticker_market_estimation") %>% 
    dplyr::as_tibble()
  
  tic_names <- DBI::dbGetQuery(mydb,"SELECT *
                                    FROM fin_ticker_meta_data") %>% 
    dplyr::as_tibble()
  
  DBI::dbDisconnect(mydb)
  
  
  
  
  df_prep <- tic_estimates %>%
    dplyr::left_join(tic_names,#[,c("ticker_yh","name","country","bics_level_1_sector_name")],
                     by = "ticker_yh") %>% 
    janitor::clean_names()
  
  
  # Update to last available date
  shiny::updateDateInput(
    session, 
    "val_date",
    value = unique(tic_estimates$retrieval_date)
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
                                input$choose_pe,
                                input$scale_pe
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  