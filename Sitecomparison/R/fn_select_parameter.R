#Observations <- all_obs
#Forecasts <- all_mods
#parm <- "MOMF"
#SID <- 1

fn_select_parameter <- function(Observations, Forecasts, parm, units,  SID){
  
  OB <- Observations |> 
    filter(SID=={{ SID }}) |>
    select(valid_dttm,SID,lon, lat, elev, all_of({{parm}})) |>
    mutate(units = units) |>
    filter(!is.na(!!sym(parm)))  #
  
  if (length(OB[[parm]]) > 0){
    
    FC <- 
      lapply( names(Forecasts), function(thismodel){ 
        modcolname <- "fcst" 
        tryCatch(
          as_harp_df( Forecasts[[ thismodel ]] |>
                        filter(SID == {{SID}}) |>
                        select("fcst_model", "fcst_dttm", "lead_time", 
                               "valid_dttm", "fcst_cycle", "SID",
                               "model_elevation", all_of(parm)) |> 
                        mutate( parameter={{parm}}, 
                                z=2,
                                units =  units ) |> 
                        rename(!!modcolname:=parm)) |>
            filter(!is.na(fcst)),   # discard rows with NA
          error = function(e) NULL
        )
      } ) |> 
      purrr::discard(is.null)  # gets rid of model who did not have parm
    
    #  Give names to the data frames, identify data frames (models) with yero rows
    FC <- set_names( FC, 
                     as.vector( unlist(
                       sapply(FC, function(x){
                         ifelse ( length( unique(x$fcst_model)) > 0, 
                                  unique(x$fcst_model), "nano")
                       }
                       )
                     ))
    ) 
    
    FC <- FC[names(FC) != "nano"]  #discard models with zero rows
    
    
    if (length(FC)>0){ 
      join_to_fcst(common_cases(as_harp_list(FC)),as_harp_df(OB)) 
    }
    
  } else if(length(OB[[parm]]) == 0 || length(FC)==0) {
    NULL
  }
  
}

