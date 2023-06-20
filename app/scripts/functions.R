

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





px_est_fun <- function(new_est,pe_val = 'forward_pe', scale = 10){


  plot_prep <- new_est %>% 
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
                    price_difference_nxt_yr = (indic_px_nxt_yr / last_price) -1) %>% 
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
  


  est_plotly <- plot_prep %>%
    dplyr::filter(diameter < 5) %>% 
    plotly::plot_ly(type = 'scatter',
                    mode = 'markers',
                    x = ~get(pe_val),
                    y = ~name,
                    marker = list(size = ~diameter * as.numeric(scale),
                                  opacity = 0.7),
                    color = ~bics_level_1_sector_name,
                    hovertemplate = ~paste0("<b>%{y}</b><br>",
                                            "Ticker: ",ticker_yh,
                                           "<br>used PE ratio: %{x:.0f}",
                                           "<br>current estimated EPS: ", estimation_eps_act_yr,
                                           "<br>Last Price: ", last_price,
                                           "<br>Price Indication: ", indic_px_act_yr,
                                           "<br>Price difference: ",scales::percent(price_difference_act_yr,accuracy = 0.01),
                                           "<extra></extra>"),
                    showlegend = F) %>% 
    plotly::layout(
      yaxis = list(visible = F, categoryorder = "trace"),
      xaxis = list(title = as.character(rlang::sym(pe_val))#,
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
      )
    )
  
  return(est_plotly)
  
  
}