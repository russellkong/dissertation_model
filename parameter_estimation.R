library(lubridate)

PE.START_YEAR<<-0000
PE.END_YEAR<<-2017
PE.SITE_OPT_OUT<<-c()
PE.SITE_OPT_IN<<-c()
#' Model parameter estimation on one dataset
#'
#'
#' @param str_weather_files 
#' @param str_param_file 
#' @param str_measured_files 
#' @param str_outfile 
#' @param conf_id 
#' @param sown_date 
#' @param weather_actual_end 
#' @param end_stage 
#' 
#' @example  cwm.calibrate(c("./Weather/W_100EA002_2016.xlsx","./Weather/W_nafferton_2017.xlsx"),"./Parameters/CWm_Parameters.xlsx",c("./Phenology/P_WindsorWest_2016.xlsx"))
cwm.calibrate <- function(str_weather_files=NULL,list_site_weather=NULL,
                           str_param_file,parameters_def=NULL,
                           str_measured_files=NULL,list_site_phenology=NULL,
                           start_year=NULL,end_year=NULL,optOutSites=NULL,optInSites=NULL,
                           str_outfile = NULL,
                           conf_id = 1,
                           weather_actual_end = NULL, trail_start=1,trail_end=NULL,store_rs=FALSE){
  
  if(!exists("PE.resultObj"))PE.resultObj<<-list()
  
  PE.START_YEAR<<-ifelse(!is.null(start_year),start_year,0)
  PE.END_YEAR<<-ifelse(!is.null(end_year),end_year,9999)
  if(!is.null(optOutSites)){
    PE.SITE_OPT_OUT<<-optOutSites
  } else {PE.SITE_OPT_OUT<<-c()}
  if(!is.null(optInSites)){
    PE.SITE_OPT_IN<<-optInSites
  } else {PE.SITE_OPT_IN<<-c()}
  
  #Preload all weather files
  if(is.null(list_site_weather))
    list_site_weather<-load.weather.data(str_weather_files,weather_actual_end = weather_actual_end)
  PE.weatherDT<<-lapply(FUN=as.data.table,list_site_weather) #performance handling
  PE.weatherDT<<-lapply(FUN=setindex,PE.weatherDT,"date")
  #Preload all measured data
  if(is.null(list_site_phenology))
    list_site_phenology<-load.phenology.data(str_measured_files)
  PE.phenologyDT<<-lapply(FUN=as.data.table,list_site_phenology) #performance handling
  
  #load parameters
  if(is.null(parameters_def)){
    parameters_def <- new('CWmParameterSet')
    parameters_def <- cwm.set_param(str_param_file, parameters_def, conf_id)
  }
  if(!exists("PE.pheoWeatherDT"))
    load.phenoWeather.data(stdPhenology = PE.phenologyDT,stdWeather = PE.weatherDT)
  
  ##vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  #Parameter to estimate (work around of optim unable to capture par name)
  
  #'temp_base	
  #'p9
  #'phyllochron	
  #'p1v	p1vd	p1d	p1dt	
  #'plastochron	
  #'gs_flp ph39	t_sum_internode	
  #'	
  #'p5		
  #'s1_therm_day	s4_therm_day	s6_therm_day	
  #'leave_prim_init	leave_emerg_init

  param_opti<-list()
  param_init<-list()
  param_lower<-list()
  param_upper<-list()
  parscale<-list()
  end_stage<-list()
  
  i=1
  param_opti[[1]]<-c( "p9")
  param_init[[1]]<-c(139)
  param_lower[[1]]<-c(50)
  param_upper[[1]]<-c(180)
  parscale[[1]]<-c(10000)
  end_stage[1]<-12
  
  i=i+1
  param_opti[[i]]<-c("phyllochron")
  param_init[[i]]<-c(80)
  param_lower[[i]]<-c(10)
  param_upper[[i]]<-c(110)
  parscale[[i]]<-c(1000)
  end_stage[i]<-92
  
  i=i+1
  param_opti[[i]]<-c("p1d",	"p1v")
  param_init[[i]]<-c(1.5,	4)
  param_lower[[i]]<-c(0,0)
  param_upper[[i]]<-c(3,8)
  parscale[[i]]<-c(100,100)
  end_stage[i]<-55
  
  i=i+1
  param_opti[[i]]<-c("phyllochron")
  param_init[[i]]<-c(80)
  param_lower[[i]]<-c(10)
  param_upper[[i]]<-c(110)
  parscale[[i]]<-c(1000)
  end_stage[i]<-55
  
  i=i+1
  param_opti[[i]]<-c("plastochron", "gs_flp")
  param_init[[i]]<-c(49.14,1.57)
  param_lower[[i]]<-c(0,1)
  param_upper[[i]]<-c(58,2)
  parscale[[i]]<-c(1000,100)
  end_stage[i]<-55

  i=i+1
  param_opti[[i]]<-c("t_sum_internode","ph39")
  param_init[[i]]<-c(97,101)
  param_lower[[i]]<-c(0,0)
  param_upper[[i]]<-c(150,200)
  parscale[[i]]<-c(100,1000)
  end_stage[i]<-55
  
  # i=i+1
  # param_opti[[i]]<-c("plastochron", "gs_flp","t_sum_internode","ph39")
  # param_init[[i]]<-c(49.14,1.57,97,101)
  # param_lower[[i]]<-c(0,1,0,0)
  # param_upper[[i]]<-c(58,2,150,200)
  # parscale[[i]]<-c(1000,100,100,1000)
  # end_stage[i]<-55
  
  i=i+1
  param_opti[[i]]<-c("p5")
  param_init[[i]]<-c(5)
  param_lower[[i]]<-c(0)
  param_upper[[i]]<-c(8)
  parscale[[i]]<-c(10000)
  end_stage[i]<-92
  
  ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  parameters_def_hist<-list()
  parameters_def_hist<-as.data.frame(parameters_def)
  for(i in trail_start: length(param_opti)){#i=trail #trail
    if(!is.null(trail_end) && i>trail_end) break
    OLS<-calibrate.optim(par=param_init[[i]], 
                        fn=calibrate.cwm.rmse,
                        lower = param_lower[[i]], upper = param_upper[[i]],parscale=parscale[[i]],
                        param_custom_opti=param_opti[[i]],param_def=parameters_def,
                        end_stage=as.numeric(end_stage[i]),store_rs)
    #put optim par into parameter
    parameters_def<-cwm.param.replace(parameters_def,param_opti[[i]],OLS$par)
    parameters_def_hist<-rbind(parameters_def_hist,as.data.frame(parameters_def))
    saveRDS(parameters_def_hist,"./Parameters/CWmParameterInProgress.rds")
  }
  
  #remove(PE.weatherDT,pos = ".GlobalEnv" )
  #remove(PE.phenologyDT,pos = ".GlobalEnv" )
  
  return(parameters_def_hist)
}

#' Model parameter estimation on one dataset
#'
#'
#' @param str_weather_files 
#' @param str_param_file 
#' @param str_measured_files 
#' @param str_outfile 
#' @param conf_id 
#' @param sown_date 
#' @param weather_actual_end 
#' @param end_stage 
#' 
#' @example  wang.calibrate(c("./Weather/W_100EA002_2016.xlsx","./Weather/W_nafferton_2017.xlsx"), "./Parameters/Wang_Parameters.xlsx",c("./Phenology/P_WindsorWest_2016.xlsx"))
wang.calibrate <- function(str_weather_files=NULL,list_site_weather=NULL,
                           str_param_file,
                           str_measured_files,list_site_phenology=NULL,
                           start_year=NULL,end_year=NULL,optOutSites=NULL,optInSites=NULL,
                           str_outfile = NULL,
                           conf_id = 1,
                           weather_actual_end = NULL, trail_start=1,trail_end=NULL,store_rs=FALSE){
  
  if(!exists("PE.resultObj"))PE.resultObj<<-list()
  
  PE.START_YEAR<<-ifelse(!is.null(start_year),start_year,0)
  PE.END_YEAR<<-ifelse(!is.null(end_year),end_year,9999)
  if(!is.null(optOutSites)){
    PE.SITE_OPT_OUT<<-optOutSites
  } else {PE.SITE_OPT_OUT<<-c("")}
  if(!is.null(optInSites)){
    PE.SITE_OPT_IN<<-optInSites
  } else {PE.SITE_OPT_IN<<-c()}
  
  #Preload all weather files
  if(is.null(list_site_weather))
    list_site_weather<-load.weather.data(str_weather_files,weather_actual_end = weather_actual_end)
  PE.weatherDT<<-lapply(FUN=as.data.table,list_site_weather) #performance handling
  
  #Preload all measured data
  if(is.null(list_site_phenology))
    list_site_phenology<-load.phenology.data(str_measured_files)
  PE.phenologyDT<<-lapply(FUN=as.data.table,list_site_phenology) #performance handling
  
  #load parameters
  parameters_def <- new('WangParameterSet')
  parameters_def <- wang.set_param(str_param_file, parameters_def, conf_id)
  
  if(!exists("PE.pheoWeatherDT"))
    load.phenoWeather.data(stdPhenology = PE.phenologyDT,stdWeather = PE.weatherDT)
  
  ##vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  #Parameter to estimate (work around of optim unable to capture par name)
  # 0-10
  # "temp_emerg_sum",  77.98
  # 11-22
  # "temp_vn_min",  "temp_vn_opt",  "temp_vn_max", 2 15 
  # "vern_full", 46
  # 11-60
  # "temp_v_min",  "temp_v_opt",  "temp_v_max",
  # "dev_v_min_day", "photo_crit",  "photo_opp",
  # "photo_sen",  
  # 14-40
  # "temp_if_opt",
  # "leave_prim_mx", "leave_app_mx",
  # "node_mx",
  # 60-92
  # "temp_r_min",  "temp_r_opt",  "temp_r_max",
  # "dev_r_min_day"
  param_opti<-list()
  param_init<-list()
  param_lower<-list()
  param_upper<-list()
  parscale<-list()
  end_stage<-list()
  
  i=1
  param_opti[[1]]<-c("temp_emerg_sum")
  param_init[[1]]<-c(77)
  param_lower[[1]]<-c(0)
  param_upper[[1]]<-c(100)
  parscale[[1]]<-c(10000)
  end_stage[1]<-11
  
  i=i+1
  param_opti[[i]]<-c( "vern_full")
  param_init[[i]]<-c(46)
  param_lower[[i]]<-c(1)
  param_upper[[i]]<-c(100)
  parscale[[i]]<-c(1000)
  end_stage[i]<-40
  
  # i=i+1
  # param_opti[[i]]<-c("temp_if_opt")
  # param_init[[i]]<-c(11.64)
  # param_lower[[i]]<-c(1)
  # param_upper[[i]]<-c(34)
  # parscale[[i]]<-c(1000)
  # end_stage[i]<-40
  
  i=i+1
  param_opti[[i]]<-c("leave_prim_mx", "leave_app_mx","node_mx")
  param_init[[i]]<-c(1, 0.3, 0.28)
  param_lower[[i]]<-c(0.2, 0.1, 0.1)
  param_upper[[i]]<-c(2,1.9,1)
  parscale[[i]]<-c(100,100,100)
  end_stage[i]<-40
  
  i=i+1
  param_opti[[i]]<-c("dev_v_min_day", "photo_crit")
  param_init[[i]]<-c(38,5)
  param_lower[[i]]<-c(10,0)
  param_upper[[i]]<-c(150,10)
  parscale[[i]]<-c(1000,1000)
  end_stage[i]<-60
  
  i=i+1
  param_opti[[i]]<-c("dev_r_min_day")
  param_init[[i]]<-c(25)
  param_lower[[i]]<-c(10)
  param_upper[[i]]<-c(50)
  parscale[[i]]<-c(1000)
  end_stage[i]<-97
  ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  parameters_def_hist<-list()
  parameters_def_hist<-as.data.frame(parameters_def)
  for(i in trail_start: length(param_opti)){#i=trail #trail
    if(!is.null(trail_end) && i>trail_end) break
    
    OLS<-calibrate.optim(par=param_init[[i]], 
                         fn=calibrate.wang.rmse,
                         lower = param_lower[[i]], upper = param_upper[[i]],parscale=parscale[[i]],
                         param_custom_opti=param_opti[[i]],param_def=parameters_def,
                         end_stage=as.numeric(end_stage[i]),store_rs)
    #put optim par into parameter
    parameters_def<-wang.param.replace(parameters_def,param_opti[[i]],OLS$par)
    parameters_def_hist<-rbind(parameters_def_hist,as.data.frame(parameters_def))
    saveRDS(parameters_def_hist,"./Parameters/WangParameterInProgress.rds")
  }
  
  #remove(PE.weatherDT,pos = ".GlobalEnv" )
  #remove(PE.phenologyDT,pos = ".GlobalEnv" )
  
  return(parameters_def_hist)
  
}


calibrate.optim <- function(par, fn, lower, upper,parscale,param_custom_opti,param_def,end_stage,store_rs){
  print_progress(paste("start optim: ",paste(paste(param_custom_opti,collapse = "|"))))
  
  curpar<-lower+(upper-lower)*0.2
  
  print_progress(paste("start low: ",paste(curpar,collapse = "|")))
  PE.iteration<<-0
  PE.resultObj.working<<-list()
  # if(length(par)>1){
      OLS_lo <- optim(par=curpar, 
              fn=fn, method="Nelder-Mead", control = list(parscale=parscale, maxit=100),
              param_custom_opti=param_custom_opti,param_def=param_def,
              end_stage=end_stage,low = lower, up = upper,
              store_rs=store_rs)
  # }else{
  #   OLS_lo <- optim(par=curpar, 
  #             fn=fn, method="L-BFGS-B", control = list(parscale=parscale, maxit=100, trace=2),
  #             param_custom_opti=param_custom_opti,param_def=param_def,
  #             end_stage=end_stage,low = lower, up = upper,lower = lower, upper = upper,
  #             store_rs=store_rs)
  # }
  
  if(store_rs)PE.resultObj.working.lo<-PE.resultObj.working
  print_progress(paste("End low with ",PE.iteration," iterations"))
  
  curpar<-par
  print_progress(paste("start medium: ",paste(par,collapse = "|")))
  PE.iteration<<-0
  PE.resultObj.working<<-list()
    # if(length(par)>1){
      OLS_mi <- optim(par=curpar, 
                          fn=fn, method="Nelder-Mead", control = list(parscale=parscale, maxit=100),
                          param_custom_opti=param_custom_opti,param_def=param_def,
                          end_stage=end_stage,low = lower, up = upper,
                          store_rs=store_rs)
    # }else{
    #   OLS_mi <- optim(par=curpar, 
    #                       fn=fn, method="L-BFGS-B", control = list(parscale=parscale, maxit=100),
    #                       param_custom_opti=param_custom_opti,param_def=param_def,
    #                       end_stage=end_stage,low = lower, up = upper,lower = lower, upper = upper,
    #                       store_rs=store_rs)
    # }
  if(store_rs)PE.resultObj.working.mi<-PE.resultObj.working
  print_progress(paste("End medium with ",PE.iteration," iterations"))
  
  curpar<-upper-(upper-lower)*0.2
  print_progress(paste("start high: ",paste(curpar,collapse = "|")))
  PE.iteration<<-0
  PE.resultObj.working<<-list()
  # if(length(par)>1){
    OLS_hi <- optim(par=curpar, 
                        fn=fn, method="Nelder-Mead", control = list(parscale=parscale, maxit=100),
                        param_custom_opti=param_custom_opti,param_def=param_def,
                        end_stage=end_stage,low = lower, up = upper,
                        store_rs=store_rs)
  # }else{
  #   OLS_hi <- optim(par=curpar, 
  #                       fn=fn, method="L-BFGS-B", control = list(parscale=parscale, maxit=100),
  #                       param_custom_opti=param_custom_opti,param_def=param_def,
  #                       end_stage=end_stage,low = lower, up = upper,lower = lower, upper = upper,
  #                       store_rs=store_rs)
  # }
  if(store_rs)PE.resultObj.working.hi<-PE.resultObj.working
  print_progress(paste("End high with ",PE.iteration," iterations"))
  
  print_progress(paste("RMSE of each guess: ",OLS_lo$value,"|",OLS_mi$value,"|",OLS_hi$value))
  print_detail(paste("par (Low):",paste(OLS_lo$par,collapse = "|")))
  print_detail(paste("par (Mid):",paste(OLS_mi$par,collapse = "|")))
  print_detail(paste("par (High):",paste(OLS_hi$par,collapse = "|")))
  
  lowest<-min(OLS_lo$value,OLS_mi$value,OLS_hi$value)
  if(OLS_lo$value==lowest){
    print_progress("Use par from low guess")
    #PE.resultObj.working<-PE.resultObj.working.lo
    OLS<-OLS_lo
    if(store_rs) PE.resultObj[[length(PE.resultObj)+1]]<<-PE.resultObj.working.lo
  }else
    if(OLS_mi$value==lowest){
      print_progress("Use par from mid guess")
      #PE.resultObj.working<-PE.resultObj.working.mi
      OLS<-OLS_mi
      if(store_rs) PE.resultObj[[length(PE.resultObj)+1]]<<-PE.resultObj.working.mi
    }else
      if(OLS_hi$value==lowest){
        print_progress("Use par from high guess")
        #PE.resultObj.working<-PE.resultObj.working.hi
        OLS<-OLS_hi
        if(store_rs) PE.resultObj[[length(PE.resultObj)+1]]<<-PE.resultObj.working.hi
      }
  
  ##only show plot of first site, checking
  #print(plot.Sim_Obs(PE.resultObj.working$resultDF[[1]][[1]],list_measured_data[[1]]))
  #for(i in 1:length(PE.resultObj.working$resultDF)){
  ##only first year first site printed for reference
  #print(plot.Sim_Obs(simDFs=list(PE.resultObj.working.mi$resultDF[["start"]][[1]][[1]],PE.resultObj.working$resultDF[["end"]][[1]][[1]]),simDFs_leg = list("start","end"),obsDF=PE.phenologyDT[[1]][[1]],title=paste(param_custom_opti,collapse = ",")))
  #}
  
  return(OLS)
}

cwm.param.replace <-function(param_def,param_custom_opti,param_custom){
  #replace default values in param file
  names(param_custom)<-param_custom_opti
  if("temp_base" %in% param_custom_opti) param_def@temp_base<-param_custom["temp_base"]
  if("p1d" %in% param_custom_opti) param_def@p1d<-param_custom["p1d"]
  if("p1dt" %in% param_custom_opti) param_def@p1dt<-param_custom["p1dt"]
  if("p1v" %in% param_custom_opti) param_def@p1v<-param_custom["p1v"]
  if("p1vd" %in% param_custom_opti) param_def@p1vd<-param_custom["p1vd"]
  if("phyllochron" %in% param_custom_opti) param_def@phyllochron<-param_custom["phyllochron"]
  if("plastochron" %in% param_custom_opti) param_def@plastochron<-param_custom["plastochron"]
  if("gs_flp" %in% param_custom_opti) param_def@gs_flp<-param_custom["gs_flp"]
  if("t_sum_internode" %in% param_custom_opti) param_def@t_sum_internode<-param_custom["t_sum_internode"]
  if("ph39" %in% param_custom_opti) param_def@ph39<-param_custom["ph39"]
  if("p5" %in% param_custom_opti) param_def@p5<-param_custom["p5"]
  if("p9" %in% param_custom_opti) param_def@p9<-param_custom["p9"]
  if("s1_therm_day" %in% param_custom_opti) param_def@s1_therm_day<-param_custom["s1_therm_day"]
  if("s4_therm_day" %in% param_custom_opti) param_def@s4_therm_day<-param_custom["s4_therm_day"]
  if("s6_therm_day" %in% param_custom_opti) param_def@s6_therm_day<-param_custom["s6_therm_day"]
  if("leave_prim_init" %in% param_custom_opti) param_def@leave_prim_init<-param_custom["leave_prim_init"]
  if("leave_emerg_init" %in% param_custom_opti) param_def@leave_emerg_init<-param_custom["leave_emerg_init"]
  
  return(param_def)
}



wang.param.replace <-function(param_def,param_custom_opti,param_custom){
  #replace default values in param file
  names(param_custom)<-param_custom_opti
  if("temp_v_min" %in% param_custom_opti) param_def@temp_cardinal["V","MIN"]<-param_custom["temp_v_min"]
  if("temp_v_opt" %in% param_custom_opti) param_def@temp_cardinal["V","OPT"]<-param_custom["temp_v_opt"]
  if("temp_v_max" %in% param_custom_opti) param_def@temp_cardinal["V","MAX"]<-param_custom["temp_v_max"]
  if("temp_r_min" %in% param_custom_opti) param_def@temp_cardinal["R","MIN"]<-param_custom["temp_r_min"]
  if("temp_r_opt" %in% param_custom_opti) param_def@temp_cardinal["R","OPT"]<-param_custom["temp_r_opt"]
  if("temp_r_max" %in% param_custom_opti) param_def@temp_cardinal["R","MAX"]<-param_custom["temp_r_max"]
  if("temp_vn_min" %in% param_custom_opti) param_def@temp_cardinal["VN","MIN"]<-param_custom["temp_vn_min"]
  if("temp_vn_opt" %in% param_custom_opti) param_def@temp_cardinal["VN","OPT"]<-param_custom["temp_vn_opt"]
  if("temp_vn_max" %in% param_custom_opti) param_def@temp_cardinal["VN","MAX"]<-param_custom["temp_vn_max"]
  if("temp_if_min" %in% param_custom_opti) param_def@temp_cardinal["IF","MIN"]<-param_custom["temp_v_min"]
  if("temp_if_opt" %in% param_custom_opti) param_def@temp_cardinal["IF","OPT"]<-param_custom["temp_if_opt"]
  if("temp_if_max" %in% param_custom_opti) param_def@temp_cardinal["IF","MAX"]<-param_custom["temp_v_max"]
  
  if("temp_emerg_sum" %in% param_custom_opti) param_def@temp_emerg_sum<-param_custom["temp_emerg_sum"]
  if("temp_base" %in% param_custom_opti) param_def@temp_base<-param_custom["temp_base"]
  if("photo_crit" %in% param_custom_opti) param_def@photo_crit<-param_custom["photo_crit"]
  if("photo_sig" %in% param_custom_opti) param_def@photo_sig<-param_custom["photo_sig"]
  if("photo_sen" %in% param_custom_opti) param_def@photo_sen<-param_custom["photo_sen"]
  if("photo_opp" %in% param_custom_opti) param_def@photo_opp<-param_custom["photo_opp"]
  if("vern_full" %in% param_custom_opti) { param_def@vern_full<-param_custom["vern_full"]; param_def@vern_base<-param_def@vern_full*0.2 }
  if("leave_prim_mx" %in% param_custom_opti) param_def@leave_prim_mx<-param_custom["leave_prim_mx"]
  if("leave_app_mx" %in% param_custom_opti) param_def@leave_app_mx<-param_custom["leave_app_mx"]
  if("node_mx" %in% param_custom_opti) param_def@node_mx<-param_custom["node_mx"]
  if("dev_v_min_day" %in% param_custom_opti) param_def@dev_v_min_day<-param_custom["dev_v_min_day"]
  if("dev_r_min_day" %in% param_custom_opti) param_def@dev_r_min_day<-param_custom["dev_r_min_day"]
  
  return(param_def)
}

#'=================================================
#' Wrapper function for models 
#' 1. fix parameters on each iteration
#' 2. call model on each measured context
#' 3. extract the simulated values with measured values
#' ================================================
#'
#' @param param_custom 
#' @param param_custom_opti 
#' @param list_weather_data 
#' @param list_measured_data 
#' @param end_stage 
#' @param param_def 
calibrate.cwm.rmse <- function(...){
  return(calibrate.rmse(modelFUN=cwm.main, paramReplaceFUN=cwm.param.replace, ...))
}

calibrate.wang.rmse <- function(...){
  return(calibrate.rmse(modelFUN=wang.main, paramReplaceFUN=wang.param.replace, ...))
}

collective.rmse <- function(str_weather_files=NULL,weather_actual_end = NULL,list_site_weather=NULL,
                            str_phenology_files=NULL,list_site_phenology=NULL,
                            str_param_file,parameters_def=NULL,conf_id = 1,
                            start_year=NULL,end_year=NULL,optOutSites=NULL,
                            store_rs=FALSE){
  
  PE.START_YEAR<<-ifelse(!is.null(start_year),start_year,0)
  PE.END_YEAR<<-ifelse(!is.null(end_year),end_year,9999)
  if(!is.null(optOutSites)){
    PE.SITE_OPT_OUT<<-optOutSites
  } else {PE.SITE_OPT_OUT<<-c("")}
  
  #Preload all weather files
  if(is.null(list_site_weather))
    list_site_weather<-load.weather.data(str_weather_files,weather_actual_end = weather_actual_end)
  PE.weatherDT<<-lapply(FUN=as.data.table,list_site_weather) #performance handling
  PE.weatherDT<<-lapply(FUN=setindex,PE.weatherDT,"date")
  #Preload all measured data
  if(is.null(list_site_phenology))
    list_site_phenology<-load.phenology.data(str_phenology_files)
  PE.phenologyDT<<-lapply(FUN=as.data.table,list_site_phenology) #performance handling
  
  #load parameters
  if(is.null(parameters_def)){
    parameters_def <- new('CWmParameterSet')
    parameters_def <- cwm.set_param(str_param_file, parameters_def, conf_id)
  }

  if(!exists("PE.pheoWeatherDT"))
    load.phenoWeather.data(stdPhenology = PE.phenologyDT,stdWeather = PE.weatherDT)
  
  PE.iteration<<-0
  PE.resultObj.working<<-list()
  calibrate.rmse(modelFUN=cwm.main, param_def = parameters_def, return="DETAIL")
}
#'
#'Calculate error distance between obs and sim data
#'factors: minimum days of appearence + stage deviation on observaed day (weight = 1:1)
#'
#' @param modelFUN 
#' @param param_custom 
#' @param param_custom_opti 
#' @param param_def 
#' @param end_stage 
#' @param retain_rs 
#' @param low 
#' @param paramReplaceFUN 
#' @param sitePhenologyDT 
#' @param siteWeatherDT 
#' @param up 
#' @param PE.iteration
#' 
#' @param PE.SITE_OPT_OUT
#' @param PE.START_YEAR
#' 
#' @import PE.phenologyDT, PE.weatherDT, PE.pheoWeatherDT
#' @export PE.skipRun; 
calibrate.rmse <-function(modelFUN,
                          paramReplaceFUN=NULL,param_custom=NULL, param_custom_opti=NULL,low, up, 
                          param_def, 
                          end_stage=92, store_rs=TRUE,
                          sitePhenologyDT=NULL, siteWeatherDT=NULL, return="RMSE"){
  if(!is.null(param_custom)){
    if(any(param_custom<low) %in% TRUE) return(Inf)
    if(any(param_custom>up) %in% TRUE) return(Inf)
  }
  
  if(!is.null(paramReplaceFUN))
    param_def<-do.call(paramReplaceFUN,list(param_def,param_custom_opti,param_custom))
  
  if(!is.null(sitePhenologyDT)){
    PE.phenologyDT<<-sitePhenologyDT
  }
  if(!is.null(siteWeatherDT)){
    PE.weatherDT<<-siteWeatherDT
  }
  
  PE.iteration<<-ifelse(!exists("PE.iteration"),1,PE.iteration+1)
  if(!exists("PE.skipRun"))PE.skipRun<<-list()
  
  if(!is.null(param_custom)){
    print_progress(paste("[",PE.iteration,"] Start ",paste(param_custom_opti,collapse = "|"),"=",paste(param_custom,collapse = "|")))
  }
  # execute model for all available measured sites
  resultDF<-list()
  list_day_se<-list()
  list_gs_se<-list()
  list_sites<-names(PE.phenologyDT)
  
  for(i in 1:length(list_sites)){
      site<-as.character(list_sites[i])
      
      #logic to skip run
      if(!site %in% names(PE.skipRun))PE.skipRun$site<<-list()
      if(site %in% PE.SITE_OPT_OUT) next
      if(length(PE.SITE_OPT_IN)>0 && !site %in% PE.SITE_OPT_IN) next
      
      resultDF[[site]]<-list()
      
      siteObsDT<-PE.phenologyDT[[site]]
      list_years<-unique(sapply(FUN=year,siteObsDT[,'date']))
      
      for(j in 1:length(list_years)){
        yr<-as.character(list_years[j])
        
        ##logic to skip run
        if(list_years[j]<PE.START_YEAR || list_years[j]>PE.END_YEAR) next
        
        if(!yr %in% names(PE.skipRun[[site]])){
          PE.skipRun[[site]][yr]<<-0
        } else if(PE.skipRun[[site]][yr]>0) next
        
        print_progress(paste("Start process site ",site," for year ",yr))
        
        #seasonObsDF<-filter(siteObsDF, year==list_years[[j]])
        seasonObsDT<-siteObsDT[year==list_years[[j]]]
        weather_station<-as.character(seasonObsDT$site[[1]])
        print_detail(paste("Weather station :",weather_station))
        
        sown_date<-as.Date(seasonObsDT$date[seasonObsDT$stage_ec==0])
        print_detail(paste("Sown date :",sown_date))
        if(length(sown_date)==0){
          print_critical(paste("skipped a dataset : sown date absence"))
          PE.skipRun[[site]][yr]<<-1
          next
        }
        
        #weather_df<-try(filter(PE.weatherDB[[weather_station]], date >= as.POSIXct(sown_date) & date <= as.POSIXct(sown_date)+days(365)))
        #weather_df<-try(filter(PE.weatherDB[[lookup.weather_station(site)]], date >= as.POSIXct(sown_date) & date <= as.POSIXct(sown_date)+days(365)))
        if(length(PE.pheoWeatherDT[[weather_station]][[as.character(sown_date)]])==0){
          PE.pheoWeatherDT[[weather_station]][[as.character(sown_date)]]<<-try(PE.weatherDT[[weather_station]][date >= as.POSIXct(sown_date) & date <= as.POSIXct(sown_date)+days(150)])
        }
        weatherDT<-PE.pheoWeatherDT[[weather_station]][[as.character(sown_date)]]
        if(length(weatherDT)==0 || nrow(weatherDT)==0) {
          print_critical(paste("skipped a dataset : weather data not found [site|year|sown_date:",site,"|",yr,"|",sown_date,"]"))
          PE.skipRun[[site]][yr]<<-2
          next
        }
        PE.modelExecutionCount<<-PE.modelExecutionCount+1
        resultDF[[site]][[yr]] <- do.call(modelFUN,list(parameters_df = param_def,
                                               weather_data_dt = weatherDT,end_stage=end_stage,sown_date = sown_date, verbose=FALSE))
        if(length(resultDF[[site]][[yr]])==0) {
          print_critical(paste("Simulation failed for [site|year:", site,"|", yr,"]"))
          PE.skipRun[[site]][yr]<<-3
          next
        }
        
        day_se<-analysis.obsDay.err.rmse(simDF = resultDF[[site]][[yr]],obsDF = seasonObsDT,end_stage = end_stage)^2
        gs_se<-analysis.gs.err.rmse(simDF = resultDF[[site]][[yr]],obsDF = seasonObsDT,end_stage = end_stage)^2
        if(length(day_se)==0 || is.nan(day_se) || length(gs_se)==0 || is.nan(gs_se)){
          print_critical(paste("Unable to calculate square error [site|year:", site,"|", yr,"]"))
          next
        }
        
        list_day_se[[site]][yr]<-day_se
        list_gs_se[[site]][yr]<-gs_se
      }
  }
  rmse_day<-sqrt(mean(unlist(list_day_se)))
  rmse_gs<-sqrt(mean(unlist(list_gs_se)))
  
  if(!is.null(param_custom)){
    print_progress(paste("[",PE.iteration,"] finish ",paste(param_custom_opti,collapse = "|"),"=",paste(param_custom,collapse = "|"),"[ RMSE_Day=",rmse_day," RMSE_GS=",rmse_gs,"]"))
  }
  
  if(store_rs){
    i<-ifelse(PE.iteration==1,"start","end")
    if(!exists("PE.resultObj.working")) PE.resultObj.working<<-list()
    if(!is.null(param_custom)){
      PE.resultObj.working$param_custom_opti[[i]]<<-param_custom_opti
      PE.resultObj.working$param_custom[[i]]<<-param_custom
    }
    PE.resultObj.working$RMSE[[i]]<<-list(day=rmse_day,gs=rmse_gs)
    PE.resultObj.working$resultDF[[i]]<<-resultDF
  }
  if(return=="RMSE"){
    return(rmse_day+rmse_gs)
  } else if(return=="DETAIL"){
    return(list(list_day_se=list_day_se,
                list_gs_se=list_gs_se,
                RMSE=rmse_day+rmse_gs))
  }
}