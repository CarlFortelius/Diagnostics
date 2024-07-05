fn_plot_data_at_mast <- function(
    fc_object           = NA, #a harp forecast object (list of data frames)
    plotvar             = NA, #string, name of parameter to be plotted
    SID                 = NA, # station identifier (1=SODA, 2=CABA, 3=LIND)
    site                = NA, # name of station
    plot_type_i         = NULL, # not used
    plot_type           = NULL, # possible choices: valid_dttm, 
    #valid_hour, lead_time, histo, cumhisto, scatter
    rolling_mean_window = 3*24) #integer, assumed hourly
{
      fn_fcst_to_df <- function(fc_object, x_axis, plotvar, SID){
    # This function takes a harp forecast object and returns a  
    # single data frame with the observation included as a forecast
    # identified by forecast_model = "OBS"
    
    
        #fc_object <- object
        #plotvar <- "MOMF"
        #SID <- 1
        #plot_type <- "hexbin"
        #x_axis <- "valid_dttm"
    
    do.call(rbind, c(list(
      fc_object[[1]] %>% 
        select({{x_axis}}, {{plotvar}}, SID) %>% 
        rename("fcst" = {{plotvar}}) %>% 
        filter(SID=={{SID}}) |> #, minute(valid_dttm) == 0) |>
        mutate(fcst_model = "OBS")
    ),
    lapply(fc_object, function(model){
      xy <-  model %>% 
        select( {{x_axis}}, fcst, fcst_model, SID) |>
        filter(SID=={{SID}})
    })
    ))
  }  
  
  #plot_type <- plot_types[[plot_type_index]]
  #print (paste("you asked for a ", plot_type) )
  start_date <- as_date( min(fc_object[[1]]$valid_dttm))
  end_date   <- as_date( max(fc_object[[1]]$valid_dttm))
  days <- length(unique(as_date(fc_object[[1]]$valid_dttm)))
  this_unit <- fc_object[[1]]$units
  
  
  if (plot_type == "valid_dttm"){
    
    print(
      fn_fcst_to_df(fc_object, plot_type, plotvar, SID) |> 
        ggplot( aes(x=valid_dttm,y=rollmean(fcst,k=rolling_mean_window,fill=NA),
                    color=fcst_model)) + 
        geom_line()        +  
        labs(
          x = "date",
          y = this_unit,
          title =  paste(
            plotvar," ", rolling_mean_window/24., " days running mean at ",
            site,"\n",
            days, " days present in ", start_date," - ",end_date,sep="")
        )
    )
    
  } else if(plot_type == "valid_hour"){
    if (!("valid_hour" %in% colnames(fc_object[[1]])) ) {
      fc_object <- expand_date(fc_object, valid_dttm)
    }
    print( 
      fn_fcst_to_df(fc_object, plot_type, plotvar, SID) |>
        group_by( !!sym(plot_type), fcst_model, SID) %>%
        summarise(mean_fcst = mean(fcst, na.rm = TRUE)) |>
        ggplot( aes(x = !!sym(plot_type),y=mean_fcst,
                    color=fcst_model)) + geom_line() +  
        #scale_color_manual(values = custom_colors) +
        labs(
          x = "hour UTC",
          y = this_unit, 
          title =  paste(
            plotvar," diurnal mean at ",site,"\n",
            days, " days present in ", start_date," - ",end_date,sep="")
        )
    ) 
    
  } else if (plot_type == "cumhisto"){
    print(fn_fcst_to_df(fc_object, "valid_dttm", plotvar, SID) |>
            group_by(fcst_model) |>
            arrange(fcst) |> 
            mutate( cum_freq = seq_along(fcst) / length(fcst), 
                    total = length(fcst) ) |> 
            ggplot(aes(x = fcst, y = cum_freq, color = fcst_model)) +
            geom_step() +
            labs(x = this_unit, 
                 y = "Cumulative Frequency", 
                 title =  paste(
                   plotvar," at ",site,"\n",
                   days, " days present in ", start_date," - ",end_date,sep="")
            )
    )
  } else if (plot_type == "histogram_solo"){
    print( 
      fn_fcst_to_df(fc_object, "valid_dttm", plotvar, SID) |>
        ggplot(aes(x = fcst, y = after_stat(density), fill = fcst_model)) +
        geom_histogram(position = "identity", alpha = 0.3, bins = 50) +
        facet_wrap(~fcst_model, ncol = 2) +
        labs(x = plotvar, y = "Relative frequency", title = " Distribution Functions")
    )
  } else if (plot_type == "histogram"){
      dfr <- dplyr::bind_rows(lapply(filter(fc_object,SID=={{SID}}), function(model){
        data.frame(
          x      =  c(model$fcst,model[[{{plotvar}}]]), 
          source = c(model$fcst_model, substr(paste("OBS",model$parameter),1,3) ),
          pair   = c(paste(model$fcst_model, "OBS"), paste(model$fcst_model, "OBS")))
      }
      )
      )
      
      #It can happen that there are no common cases, so we need check
      #print(paste(plotvar, length(dfr[["x"]])) )
      if (length(dfr[["x"]]) > 0){
        print(
          dfr |> 
        ggplot(aes(x = x , y = after_stat(density), fill = source)) +
        geom_histogram(position = "identity", alpha = 0.3, bins = 25) +
        facet_wrap(~pair, ncol = 2) +
        labs(x = this_unit, 
             y = "Relative frequency", 
             title =  paste(
               plotvar," at ",site,"\n",
               days, " days present in ", start_date," - ",end_date,sep="")
        ))
        }
  } else if (plot_type == "hexbin"){
    dfr <- do.call(rbind, 
            lapply(fc_object, function(model){
              model |>
                select( valid_dttm, fcst, all_of({{plotvar}}), fcst_model, SID)
            })) |>
      filter(SID=={{SID}})
    
    #It can happen that there are no common cases, so we need check
    if (length(dfr[[plotvar]]) > 0){
      print(
      dfr |>
        ggplot(
          aes(x  = !!sym(plotvar), y = fcst)) +
        geom_hex(position = "identity")       +
        scale_fill_viridis_c(
          option = "magma", 
          name   = "Frequency", 
          trans  = "log",
          breaks = seq_double(2, 1024),
          labels = function(x) format(x, scientific = FALSE,
                                      trim = TRUE),
        ) +
        geom_abline(intercept = 0, slope = 1, color = "black", linetype = "solid") +
        facet_wrap(~fcst_model, ncol = 2) +
        labs(x = this_unit, 
             y = this_unit, 
             title =  paste(
               plotvar," at ",site,"\n",
               days, " days present in ", start_date," - ",end_date,sep="")
        )
 
    )
  }
    }else {
    print (paste(plot_type, " is a no go") )
  }
}
