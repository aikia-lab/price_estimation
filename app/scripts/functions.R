

connect_to_DB <- function(mydb, group = "fin_data"){
  
  Checkmydb <- tryCatch(DBI::dbIsValid(mydb),
                        error=function(e) e)
  if(inherits(Checkmydb, "simpleError")){
    
    if(Sys.info()[[1]] == "Windows"){ #For testing on Windows
      
      ip <- unlist(pingr::nsl("aikia.org")$answer$data)
      mydb <- DBI::dbConnect(RMariaDB::MariaDB(), 
                             group = group, 
                             host = ip,
                             default.file = stringr::str_c(here::here(),
                                                           "/my.cnf"))
      
    } else { #Production on Linux (No Macs here ;))
      
      ip <- unlist(pingr::nsl("aikia.org")$answer$data)
      mydb <- DBI::dbConnect(RMariaDB::MariaDB(),host = ip, group = group)
      
    }
  }
}


getbuttons <- function(name){
  list(method = "restyle",
       args = list("transforms[0].value", name),
       label = name,
       visible = TRUE)
}


px_est_fun <- function(new_est,date, pe_val = 'forward_pe', scale = 1){#scale = 10


  plot_prep <- new_est %>% 
    dplyr::filter(retrieval_date == lubridate::as_date(date)) %>% 
    dplyr::arrange(bics_level_1_sector_name) %>% 
    tidyr::drop_na(bics_level_1_sector_name)  
    
  
  fct_levels <- plot_prep %>% 
    dplyr::arrange(bics_level_1_sector_name) %>% 
    dplyr::pull(name)

  # Calculate estimated prices
  plot_prep <- plot_prep %>% 
      dplyr::mutate(indic_px_act_yr = estimation_eps_act_yr * get(pe_val),
                    indic_px_nxt_yr = estimation_eps_nxt_yr * get(pe_val),
                    price_difference_act_yr = (indic_px_act_yr / last_price) -1,
                    price_difference_nxt_yr = (indic_px_nxt_yr / last_price) -1,
                    ring_color = ifelse(price_difference_act_yr > 0, aikia::aikia_palette_eight()[1], aikia::aikia_palette_eight()[4])) %>% 
    dplyr::mutate(diameter = round(abs(price_difference_act_yr),digits = 0),
                  name = factor(name, levels = fct_levels)) %>% 
    tidyr::drop_na(diameter) 
  
#  plot_prep <- head(plot_prep,n=6)
testdf<<-plot_prep %>% dplyr::select(ticker_yh,rlang::sym(pe_val),estimation_eps_act_yr,indic_px_act_yr,last_price,price_difference_act_yr,diameter)


  # helper functions
  hline <- function(y = 0, color = "black"){
    list(
      type = 'line',
      x0 = 0,
      x1 = 1,
      xref = "paper",
      y0 = y,
      y1 = y,
      line = list(color = color,
                  width = 0.5,
                  dash = "dot")
    )
  }
  
  
  hline_coords <- plot_prep %>% 
    dplyr::count(bics_level_1_sector_name) %>% 
    dplyr::mutate(n = cumsum(n))

  filter_buttons <- purrr::map(unique(sort(as.character(plot_prep$name))), getbuttons)

  
  if(scale == 1){
  est_plotly <- plot_prep %>%
    dplyr::filter(diameter < 5) %>% 
    plotly::plot_ly(type = 'scatter',
                    mode = 'markers',
                    x = ~get(pe_val),
                    y = ~name,
                    marker = list(size = 10,
                                  opacity = 0.7,
                                  line = list(
                                    color = ~ring_color,
                                    width = 2
                                  )),
                    color = ~bics_level_1_sector_name,
                    hovertemplate = ~paste0("<b>%{y}</b><br>",
                                            "Ticker: ",ticker_yh,
                                           "<br>used PE ratio: %{x:.0f}",
                                           "<br>current estimated EPS: ", estimation_eps_act_yr,
                                           "<br>Last Price: ", last_price,
                                           "<br>Price Indication: ", scales::number(indic_px_act_yr,accuracy = 0.01),
                                           "<b><br>Price difference: ",scales::percent(price_difference_act_yr,accuracy = 0.01),
                                           "</b><extra></extra>"),
                    showlegend = F)  
  } else {
    est_plotly <- plot_prep %>%
      dplyr::filter(diameter < 5) %>% 
      plotly::plot_ly(type = 'scatter',
                      mode = 'markers',
                      x = ~get(pe_val),
                      y = ~name,
                      marker = list(size = ~diameter * 10,
                                    opacity = 0.7,
                                    line = list(
                                      color = ~ring_color,
                                      width = 2
                                    )),
                      color = ~bics_level_1_sector_name,
                      hovertemplate = ~paste0("<b>%{y}</b><br>",
                                              "Ticker: ",ticker_yh,
                                              "<br>used PE ratio: %{x:.0f}",
                                              "<br>current estimated EPS: ", estimation_eps_act_yr,
                                              "<br>Last Price: ", last_price,
                                              "<br>Price Indication: ", scales::number(indic_px_act_yr,accuracy = 0.01),
                                              "<b><br>Price difference: ",scales::percent(price_difference_act_yr,accuracy = 0.01),
                                              "</b><extra></extra>"),
                      showlegend = F) 
    
  }
  
  est_plotly <- est_plotly %>% 
    plotly::add_trace(
      type = 'scatter',
      mode = "text+markers",
      x = ~get(pe_val),
      y = ~name,
      text = ~name,
      textposition = 'top center',
      textfont = list(size = 12, color = 'black'),
      transforms = list(
        list(type = 'filter',
             target = ~name,
             operation = 'in')
      ),
      showlegend = F
    ) %>% 
    plotly::add_annotations(
      x = 0.0,
      y = ~hline_coords$n-1,
      text = hline_coords$bics_level_1_sector_name,
      xref = "x",
      yref = "y",
      xanchor = "right",
      showarrow = F
    ) %>% 
    plotly::layout(
      yaxis = list(visible = F, categoryorder = "trace"),
      xaxis = list(title = paste(as.character(rlang::sym(pe_val))," Ratio")#,
                  # range = c(-50,100)
      ),
      shapes = list(
        hline(hline_coords[[1,2]]),
        hline(hline_coords[[2,2]]),
        hline(hline_coords[[3,2]]),
        hline(hline_coords[[4,2]]),
        hline(hline_coords[[5,2]]),
        hline(hline_coords[[6,2]]),
        hline(hline_coords[[7,2]]),
        hline(hline_coords[[8,2]]),
        hline(hline_coords[[9,2]]),
        hline(hline_coords[[10,2]]),
        hline(hline_coords[[11,2]])
      ),
      updatemenus = list(
        list(x = 0.2,
             y=1.1,
             bgcolor = aikia::aikia_main_light(),
             buttons = filter_buttons)
      ),
      font = list(weight = 'bold')
    )
  
  return(est_plotly)
  
  
}