#This function plots data according to the plot_type given and returns 
# plot object

fn_return_plot_of_data_at_mast <- function(
    plot_type           = NULL, # possible choices: valid_dttm, 
    fc_object           = NA, #a harp forecast object (list of data frames)
    secondary_object    = NA, #used for certain plot_types ("compare_params", A_vs_B)
    #plotvar             = NA, #string, name of parameter to be plotted
    #secondary_var       = NA, #string, used for certain plot_types
    SID                 = NA, # station identifier (1=SODA, 2=CABA, 3=LIND)
    site                = NA, # name of station
    #valid_hour, lead_time, histo, cumhisto, scatter
    rolling_mean_window = 3*24,  #integer, assumed hourly
    show_zero           = "Neither") # possible values: "Neither", "Both", "X", "Y"
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
      fc_object[[1]] |> 
        select({{x_axis}}, {{plotvar}}, SID) |> 
        rename("fcst" = {{plotvar}}) |> 
        filter(SID=={{SID}}) |> #, minute(valid_dttm) == 0) |>
        mutate(fcst_model = "OBS")
    ),
    lapply(fc_object, function(model){
      xy <-  model |> 
        select( {{x_axis}}, fcst, fcst_model, SID) |>
        filter(SID=={{SID}})
    })
    ))
  }  
  
  #plot_type <- plot_types[[plot_type_index]]
  #print (paste("you asked for a ", plot_type) )
  start_date     <- as_date( min(fc_object[[1]]$valid_dttm))
  end_date       <- as_date( max(fc_object[[1]]$valid_dttm))
  days           <- length(unique(as_date(fc_object[[1]]$valid_dttm)))
  plotvar        <- unique(fc_object[[1]]$parameter) 
  secondary_var  <- unique(secondary_object[[1]]$parameter)
  this_unit      <- fc_object[[1]]$units
  secondary_unit <- secondary_object[[1]]$units
  
  
  if (       show_zero == "Both"){xlimit <- 0        ; ylimit <- 0
  } else if (show_zero == "Neither") {xlimit <- NULL ; ylimit <- NULL
  } else if (show_zero == "X") {xlimit <- 0          ; ylimit <- NULL
  } else if (show_zero == "Y") {xlimit <- NULL       ; ylimit <- 0}
  
  if (plot_type == "valid_dttm"){
    
    this_plot <- 
      fn_fcst_to_df(fc_object, plot_type, plotvar, SID) |> 
      group_by(fcst_model) |>
      summarise(
        x    = valid_dttm,
        y = rollmean(fcst,k=rolling_mean_window, fill = NA)
      ) |>
        ggplot( aes(x=x,y=y,
                    color=fcst_model)) + 
        geom_line()        +  
        expand_limits(x = NULL, y = ylimit) + # include zero
        theme(axis.text=element_text(size=15)) + #change font size of axis text
        scale_y_continuous(breaks = pretty_breaks(n=5)) +
        labs(
          x = "date",
          y = this_unit,
          title =  paste(
            plotvar, " at ",
            site,";", rolling_mean_window/24., " days running mean \n",
            days, " days present in ", start_date," - ",end_date,sep="")
        )
    
    
  } else if(plot_type == "valid_hour"){
    if (!("valid_hour" %in% colnames(fc_object[[1]])) ) {
      fc_object <- expand_date(fc_object, valid_dttm)
    }
    this_plot <- 
      fn_fcst_to_df(fc_object, plot_type, plotvar, SID) |>
        group_by( !!sym(plot_type), fcst_model, SID) |>
        summarise(mean_fcst = mean(fcst, na.rm = TRUE)) |>
        ggplot( aes(x = !!sym(plot_type),y=mean_fcst,
                    color=fcst_model)) + geom_line() +
        scale_x_continuous(breaks = c(0,3,6,9,12,15,18,21,24)) +
        expand_limits(x = NULL, y = ylimit) + # include zero
        theme(axis.text=element_text(size=15)) + #change font size of axis text
        scale_y_continuous(breaks = pretty_breaks(n=5)) +
        #scale_color_manual(values = custom_colors) +
        labs(
          x = "hour UTC",
          y = this_unit, 
          title =  paste(
            plotvar," at ",site,"; diurnal mean \n",
            days, " days present in ", start_date," - ",end_date,sep="")
        )
    
  } else if (plot_type == "cumhisto"){
    this_plot <- fn_fcst_to_df(fc_object, "valid_dttm", plotvar, SID) |>
            group_by(fcst_model) |>
            arrange(fcst) |> 
            mutate( cum_freq = seq_along(fcst) / length(fcst), 
                    total = length(fcst) ) |> 
            ggplot(aes(x = fcst, y = cum_freq, color = fcst_model)) +
            geom_step() +
            theme(axis.text=element_text(size=15)) + #change font size of axis text
            labs(x = this_unit, 
                 y = "Cumulative Frequency", 
                 title =  paste(
                   plotvar," at ",site,"\n",
                   days, " days present in ", start_date," - ",end_date,sep="")
            )
  } else if (plot_type == "histogram_solo"){
    this_plot <- 
      fn_fcst_to_df(fc_object, "valid_dttm", plotvar, SID) |>
        ggplot(aes(x = fcst, y = after_stat(density), fill = fcst_model)) +
        geom_histogram(position = "identity", alpha = 0.3, bins = 50) +
        facet_wrap(~fcst_model, ncol = 2) +
        theme(axis.text=element_text(size=15)) + #change font size of axis text
        labs(x = plotvar, y = "Relative frequency", title = " Distribution Functions")

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
        this_plot <- 
          dfr |> 
        ggplot(aes(x = x , y = after_stat(density), fill = source)) +
        geom_histogram(position = "identity", alpha = 0.3, bins = 25) +
        facet_wrap(~pair, ncol = 2) +
        theme(axis.text.x=element_text(size=12)) + #change font size of axis text
        labs(x = this_unit, 
             y = "Relative frequency", 
             title =  paste(
               plotvar," at ",site,"\n",
               days, " days present in ", start_date," - ",end_date,sep="")
        )
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
      this_plot <-
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
        theme(axis.text=element_text(size=12)) + #change font size of axis text
        labs(x = paste0("OBS, ",this_unit), 
             y = paste0("MOD, ",this_unit), 
             title =  paste(
               plotvar," at ",site,"\n",
               days, " days present in ", start_date," - ",end_date,sep="")
        )
    }
  } else if (plot_type == "A_vs_B"){
    dfr <- inner_join(fn_fcst_to_df(fc_object,        "valid_dttm", plotvar,       SID),
                      fn_fcst_to_df(secondary_object, "valid_dttm", secondary_var, SID),
                      by=c("valid_dttm", "fcst_model", "SID"), suffix = c(".y", ".x"))

    
    #It can happen that there are no common cases, so we need check
    #if (length(dfr[[fcst.x]]) > 0 & length(dfr[[fcst.y]]) > 0){
      this_plot <-
        dfr |>
        ggplot(
          aes(x  = fcst.x, y = fcst.y)) +
        geom_hex(position = "identity")       +
        scale_fill_viridis_c(
          option = "magma", 
          name   = "Frequency", 
          trans  = "log",
          breaks = seq_double(2, 1024),
          labels = function(x) format(x, scientific = FALSE,
                                      trim = TRUE),
        ) +
        #coord_equal() + 
        facet_wrap(~fcst_model, ncol = 2) +
        theme(axis.text=element_text(size=12)) + #change font size of axis text
        labs(y = paste(plotvar,       unique(       fc_object[[1]]$units), sep=", "), 
             x = paste(secondary_var, unique(secondary_object[[1]]$units), sep=", "), 
             title =  paste(
               plotvar," vs ",secondary_var," at ",site,"\n",
               days, " days present in ", start_date," - ",end_date,sep="")
        )
    #}
  } else if(plot_type == "compare_params"){
    df <- inner_join(fn_fcst_to_df(fc_object, "valid_dttm", plotvar, SID),
                      fn_fcst_to_df(secondary_object, "valid_dttm", secondary_var, SID),
                      by=c("valid_dttm", "fcst_model", "SID"), suffix = c(".y", ".x"))
    mean_obs_x <- mean( filter(df, fcst_model=="OBS")$fcst.x )
    mean_obs_y <- mean( filter(df, fcst_model=="OBS")$fcst.y )
    
    this_plot <- 
        df |> 
        group_by(fcst_model) |>
        summarise(
          x = mean(fcst.x),
          y = mean(fcst.y)
      ) |> 
      mutate( ratio = with(df, ifelse(x != 0, paste0("y/x: ", round(y / x, 2)), "NA"))) |>
        ggplot( aes(x=x, y=y, color=fcst_model)) + 
        geom_point(size=6)        +  
      geom_abline(intercept = 0, 
                 slope     =  mean(filter(df,fcst_model=="OBS")$fcst.y)/mean(filter(df,fcst_model=="OBS")$fcst.x), 
                 color     = "black", linetype = "dashed") +
      geom_text(aes(label = ratio), vjust = -1, hjust = 1, size = 3, color = "black") +  # Add the text annotations
      expand_limits(x = xlimit, y = ylimit) + # include zero?
      scale_x_continuous(breaks = pretty_breaks(n=5)) + 
      scale_y_continuous(breaks = pretty_breaks(n=5)) +
      theme(axis.text=element_text(size=15)) + #change font size of axis text
      labs(
          x = paste0(secondary_var,", ",secondary_unit),
          y = paste0(plotvar,", ",this_unit),
          title =  paste("Mean ",plotvar," vs mean ", secondary_var," at ",
            site,"\n",
            days, " days present in ", start_date," - ",end_date,sep="")
        )
  }else {
    print (paste(plot_type, " is a no go") )
  }
return(this_plot)}
