library(yaml)

conf_get_config <- function(){
  CONFIG <- yaml.load_file(here("config/config_atos.yml") )
  CONFIG$params_details = conf_get_params_details()
  CONFIG
}

conf_get_params_details <- function(){
    TT_thr <- c(seq(-30, 30, 2.5))
    DT_thr <- c(seq(-4,4, 1))
    RH_thr <- c(25, 50, 75, 95)
    FF_thr <- c(2,3,7,10,15)
    SW_thr <- c(7.5, 30, 75, 300, 750)
    LW_thr <- c(seq(-300,400, 10))
    SSH_thr <- c(seq(-50,600, 100))
    EVAP_thr <- c(seq(-0.5,10, 1))
    MOMF_thr <- c(seq(0,2, 0.1))
    
    params_all <- list(
    		TT = list(
        thresholds = TT_thr,
        units = "degC"
        ),       
    		TL = list(
        thresholds = TT_thr,
        units = "degC"       
    			),
    		DT = list(
    		  thresholds = DT_thr,
    		  units = "degC"       
    		),
    		DTLTS = list(
    		  thresholds = DT_thr,
    		  units = "degC",
    		  definition = "TL - TSRAD"
    		),
    		RH = list(
        thresholds = RH_thr, units="%"
    			),
    		FF = list(
        thresholds = FF_thr, units="m/s"
    			),
    		GLOB = list(
    		  thresholds = SW_thr, units="Wm^-2"
    		),
    		SWUP = list(
    		  thresholds = SW_thr, units="Wm^-2"
    		),
    		SWDIF = list(
    		  thresholds = SW_thr,units="Wm^-2"
    		),
    		DNI = list(
    		  thresholds = SW_thr,units="Wm^-2"
    		),
    		LWUP = list(
        thresholds = LW_thr,units="Wm^-2"
    			),
    		LWDN = list(
        thresholds = LW_thr,units="Wm^-2"
    			),
    		SSH = list(
        thresholds = SSH_thr,units="Wm^-2"
    			),
    		SLH = list(
    		  thresholds = SSH_thr,units="Wm^-2"
    		),
    		STH = list(
    		  thresholds = SSH_thr,units="Wm^-2",
    		  definiton = "SSH + SLH"
    		),
    		RNET = list(
    		  thresholds = LW_thr,units="Wm^-2",
    		  definiton = "GLOB + LWDN -SWUP - LWUP"
    		),
    		RESID = list(
    		  thresholds = SSH_thr, units="Wm^-2",
    		  definition = "GLOB + LWDN -SWUP - LWUP - SSH - SLH"
    		),
    		SFCFORC = list(
    		  thresholds = LW_thr,units="Wm^-2",
    		  definiton = "GLOB + LWDN -SWUP"
    		),
    		TSRAD = list(
    		  thresholds = TT_thr,units="degC",
    		  definiton = "(LWUP/5.67e-8)^0.25 - 273.15"
    		),
    		UDT = list(
    		  thresholds = FF_thr,units="ms^-1K",
    		  definiton = "( TL-(LWUP/5.67e-8)^0.25 ) * FF"
    		),
    		EVAP = list(
        thresholds = EVAP_thr,units="?"
    			),
    		MOMF = list(
        thresholds = MOMF_thr,units="kgm^-1s^-2"
    			))
    		  
         params_all
  
}

