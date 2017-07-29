
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
cwm.calibrate <- function(str_weather_files,
                           str_param_file,
                           str_measured_files,
                           str_outfile = NULL,
                           conf_id = 1,
                           sown_date = NULL,
                           weather_actual_end = NULL, end_stage=99,trail_start=NULL,trail_end=NULL,store_rs=FALSE){
  
  if(!exists("PE.resultObj"))PE.resultObj<<-list()
  
  #Preload all weather files
  weather_data_df<-load.weather.data(str_weather_files,weather_actual_end = weather_actual_end,
                                     start_date = sown_date)
  
  #Preload all measured data
  list_measured_data<-load.phenology.data(str_measured_files)
  
  #load parameters
  parameters_def <- new('CWmParameterSet')
  parameters_def <- cwm.set_param(str_param_file, parameters_def, conf_id)
  
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
  param_lower[[i]]<-c(30)
  param_upper[[i]]<-c(110)
  parscale[[i]]<-c(1000)
  end_stage[i]<-60
  
  i=i+1
  param_opti[[i]]<-c("p1d",	"p1v")
  param_init[[i]]<-c(1.5,	4)
  param_lower[[i]]<-c(0,0)
  param_upper[[i]]<-c(3,8)
  parscale[[i]]<-c(100,100)
  end_stage[i]<-30
  
  i=i+1
  param_opti[[i]]<-c("plastochron"	)
  param_init[[i]]<-c(49.14)
  param_lower[[i]]<-c(0)
  param_upper[[i]]<-c(58)
  parscale[[i]]<-c(1000)
  end_stage[i]<-40
  
  i=i+1
  param_opti[[i]]<-c("t_sum_internode","ph39")
  param_init[[i]]<-c(72,101)
  param_lower[[i]]<-c(0,0)
  param_upper[[i]]<-c(150,200)
  parscale[[i]]<-c(100,1000)
  end_stage[i]<-40
  
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
  for(i in 1: length(param_opti)){#i=trail #trail
    
    OLS<-calibrate.optim(par=param_init[[i]], 
                        fn=calibrate.cwm.rmse,
                        lower = param_lower[[i]], upper = param_upper[[i]],parscale=parscale[[i]],
                        param_custom_opti=param_opti[[i]],list_weather_data=list_weather_data,param_def=parameters_def,
                        list_measured_data=list_measured_data,end_stage=as.numeric(end_stage[i]),store_rs)
    #put optim par into parameter
    parameters_def<-cwm.param.replace(parameters_def,param_opti[[i]],OLS$par)
    parameters_def_hist<-rbind(parameters_def_hist,as.data.frame(parameters_def))
  }
  
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
#' @example  wang.calibrate(c("./Weather/W_100EA002_2016.xlsx","./Weather/W_nafferton_2017.xlsx"), "./Parameters/Wang_Parameters.xlsx",c("./Phenology/P_WindsorWest_2016.xlsx"),end_stage=40)
wang.calibrate <- function(str_weather_files,
                           str_param_file,
                           str_measured_files,
                           str_outfile = NULL,
                           conf_id = 1,
                           sown_date = NULL,
                           weather_actual_end = NULL, end_stage=99, trail=1,store_rs=FALSE){
  
  if(!exists("PE.resultObj"))PE.resultObj<<-list()
  
  #Preload all weather files
  weather_data_df<-load.weather.data(str_weather_files,weather_actual_end = weather_actual_end,
                 start_date = sown_date)
  
  #Preload all measured data
  list_measured_data<-load.phenology.data(str_measured_files)
  
  #load parameters
  parameters_def <- new('WangParameterSet')
  parameters_def <- wang.set_param(str_param_file, parameters_def, conf_id)
  
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
  param_upper[[i]]<-c(2,1.9,2)
  parscale[[i]]<-c(100,100,100)
  end_stage[i]<-40
  
  i=i+1
  param_opti[[i]]<-c("dev_v_min_day", "photo_crit",  "photo_opp")
  param_init[[i]]<-c(38,5,17.7)
  param_lower[[i]]<-c(10,0,0)
  param_upper[[i]]<-c(150,24,24)
  parscale[[i]]<-c(1000,1000,1000)
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
  for(i in 1: length(param_opti)){#i=trail #trail
    
    OLS<-calibrate.optim(par=param_init[[i]], 
                         fn=calibrate.wang.rmse,
                         lower = param_lower[[i]], upper = param_upper[[i]],parscale=parscale[[i]],
                         param_custom_opti=param_opti[[i]],list_weather_data=list_weather_data,param_def=parameters_def,
                         list_measured_data=list_measured_data,end_stage=as.numeric(end_stage[i]),store_rs)
    #put optim par into parameter
    parameters_def<-wang.param.replace(parameters_def,param_opti[[i]],OLS$par)
    parameters_def_hist<-rbind(parameters_def_hist,as.data.frame(parameters_def))
  }
  
  return(parameters_def_hist)
  
}


calibrate.optim <- function(par, fn, lower, upper,parscale,param_custom_opti,list_weather_data,param_def,list_measured_data,end_stage,store_rs){
  
  PE.iteration<<-0
  PE.resultObj.working<<-list()
  OLS_lo <- 
                try(optim(par=lower+(upper-lower)*0.2, 
                          fn=fn,lower = lower, upper = upper, method="Nelder-Mead", control = list(parscale=parscale),
                          param_custom_opti=param_custom_opti,list_weather_data=list_weather_data,param_def=param_def,
                          list_measured_data=list_measured_data,end_stage=end_stage)
                )
  
  PE.resultObj.working.lo<-PE.resultObj.working
  
  PE.iteration<<-0
  PE.resultObj.working<<-list()
  OLS_mi <- 
                try(optim(par=par, 
                          fn=fn,lower = lower, upper = upper, method="Nelder-Mead", control = list(parscale=parscale),
                          param_custom_opti=param_custom_opti,list_weather_data=list_weather_data,param_def=param_def,
                          list_measured_data=list_measured_data,end_stage=end_stage)
                )
  PE.resultObj.working.mi<-PE.resultObj.working
  
  PE.iteration<<-0
  PE.resultObj.working<<-list()
  OLS_hi <- 
                try(optim(par=upper-(upper-lower)*0.2, 
                          fn=fn,lower = lower, upper = upper, method="Nelder-Mead", control = list(parscale=parscale),
                          param_custom_opti=param_custom_opti,list_weather_data=list_weather_data,param_def=param_def,
                          list_measured_data=list_measured_data,end_stage=end_stage)
                )
  PE.resultObj.working.hi<-PE.resultObj.working
  
  print_progress(paste("RMSE of each guess: ",OLS_lo$value,"|",OLS_mi$value,"|",OLS_hi$value))
  print_keypoint(OLS_lo$par)
  print_keypoint(OLS_mi$par)
  print_keypoint(OLS_hi$par)
  lowest<-min(OLS_lo$value,OLS_mi$value,OLS_hi$value)
  if(OLS_lo$value==lowest){
    print_keypoint("Use par from low guess")
    PE.resultObj.working<-PE.resultObj.working.lo
    OLS<-OLS_lo
    if(store_rs) PE.resultObj[[length(PE.resultObj)+1]]<<-PE.resultObj.working.lo
  }else
    if(OLS_mi$value==lowest){
      print_keypoint("Use par from mid guess")
      PE.resultObj.working<-PE.resultObj.working.mi
      OLS<-OLS_mi
      if(store_rs) PE.resultObj[[length(PE.resultObj)+1]]<<-PE.resultObj.working.mi
    }else
      if(OLS_hi$value==lowest){
        print_keypoint("Use par from high guess")
        PE.resultObj.working<-PE.resultObj.working.hi
        OLS<-OLS_hi
        if(store_rs) PE.resultObj[[length(PE.resultObj)+1]]<<-PE.resultObj.working.hi
      }
  
  ##only show plot of first site, checking
  #print(plot.Sim_Obs(PE.resultObj.working$resultDF[[1]][[1]],list_measured_data[[1]]))
  i=length(PE.resultObj.working$resultDF)
  #for(i in 1:length(PE.resultObj.working$resultDF)){
  print(plot.Sim_Obs(simDFs=list(PE.resultObj.working$resultDF[[1]][[1]],PE.resultObj.working$resultDF[[i]][[1]]),simDFs_leg = list("start","end"),obsDF=list_measured_data[[1]],title=paste(param_custom_opti,collapse = ",")))
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
  return(calibrate.rmse(modelFUN=cwm.main, paramFUN=cwm.param.replace, ...))
}

calibrate.wang.rmse <- function(...){
  return(calibrate.rmse(modelFUN=wang.main, paramFUN=wang.param.replace, ...))
}

calibrate.rmse <-function(modelFUN,paramFUN,param_custom, param_custom_opti,list_weather_data,param_def, list_measured_data, end_stage, retain_rs=TRUE){
  param_def<-do.call(paramFUN,list(param_def,param_custom_opti,param_custom))
  
  # execute model for all available measured sites
  resultDF<-list()
  list_se<-list()
  list_sites<-names(list_measured_data)
  for(i in 1:length(list_sites)){
    measured_df<-list_measured_data[[list_sites[i]]]
    sown_date<-as.Date(measured_df$date[measured_df$stage_ec==0])
    
    weather_df<-list_weather_data[[lookup.weather_station(list_sites[i])]]
    
    resultDF[[list_sites[i]]] <- do.call(modelFUN,list(parameters_df = param_def,
                                           weather_data_df = weather_df,end_stage=end_stage,sown_date = sown_date, verbose=FALSE))
    
    list_se[list_sites[i]]<-analysis.obsDay.err.rmse(simDF = resultDF[[i]],obsDF = measured_df,end_stage = end_stage)^2
  }
  
  rmse<-sqrt(do.call(sum,list_se)/length(list_se))
  print(paste(paste(param_custom_opti,collapse = "|"),"=",paste(param_custom,collapse = "|"),"[ RMSE=",rmse,"]"))
  
  if(retain_rs){
    PE.iteration<<-ifelse(!exists("PE.iteration"),1,PE.iteration+1)
    if(!exists("PE.resultObj.working")) PE.resultObj.working<<-list()
    PE.resultObj.working$param_custom_opti[[PE.iteration]]<<-param_custom_opti
    PE.resultObj.working$param_custom[[PE.iteration]]<<-param_custom
    PE.resultObj.working$RMSE[PE.iteration]<<-rmse
    PE.resultObj.working$resultDF[[PE.iteration]]<<-resultDF
  }
  return(rmse)
}