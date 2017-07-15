
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
#' @example  wang.calibrate(c("./Weather/W_100EA002_2016.xlsx","./Weather/W_nafferton_2017.xlsx"),"./Parameters/Wang_Parameters.xlsx",c("./Phenology/P_WindsorWest_2016.xlsx"),end_stage=87)
wang.calibrate <- function(str_weather_files,
                           str_param_file,
                           str_measured_files,
                           str_outfile = NULL,
                           conf_id = 1,
                           sown_date = NULL,
                           weather_actual_end = NULL, end_stage=99){
  remove(PE.iteration)
  PE.iteration<<-1
  remove(PE.resultObj)
  PE.resultObj<<-list()
  
  #Preload all weather files
  list_weather_data<-list()
  for(i in 1:length(str_weather_files)){
     weather_data_df <-
    load_weather(str_weather_files[i],
                 weather_actual_end = weather_actual_end,
                 start_date = sown_date)
     list_weather_data[[as.character(weather_data_df$site[1])]]<-weather_data_df
  }
 
  #Preload all measured data
  list_measured_data<-list()
  for(i in 1:length(str_measured_files)){
    measured_df <- read_excel(str_measured_files)
    #assume site specified as the same in whole column
    list_measured_data[[as.character(measured_df$site[1])]]<-measured_df
  }
  
  #load parameters
  parameters_def <- new('WangParameterSet')
  parameters_def <- wang.set_param(str_param_file, parameters_def, conf_id)
  
  #Parameter to estimate (work around of optim unable to capture par name)
  param_opti<-c("temp_v_opt")
  param_init<-c(24)
  
  # system.time(OLS1p <- 
  #               try(nls(0~wang.rmse(param_init,param_opti,weather_data_df,parameters_def,measured_df,end_stage),
  #                       start= list(param_init=param_init))))
  
  system.time(OLS2p <- 
                try(optim(par=param_init,
                          fn=wang.rmse,
                          param_custom_opti=param_opti,list_weather_data=list_weather_data,param_def=parameters_def,
                          list_measured_data=list_measured_data,end_stage=end_stage)
                    )
              )
  return(OLS2p)
  
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
wang.rmse <- function(param_custom, param_custom_opti,list_weather_data,param_def, list_measured_data, end_stage
){
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
  if("temp_if_min" %in% param_custom_opti) param_def@temp_cardinal["IF","MIN"]<-param_custom["temp_if_min"]
  if("temp_if_opt" %in% param_custom_opti) param_def@temp_cardinal["IF","OPT"]<-param_custom["temp_if_opt"]
  if("temp_if_max" %in% param_custom_opti) param_def@temp_cardinal["IF","MAX"]<-param_custom["temp_if_max"]
  
  if("temp_emerg_sum" %in% param_custom_opti) param_def@temp_emerg_sum<-param_custom["temp_emerg_sum"]
  if("temp_base" %in% param_custom_opti) param_def@temp_base<-param_custom["temp_base"]
  if("photo_crit" %in% param_custom_opti) param_def@photo_crit<-param_custom["photo_crit"]
  if("photo_sig" %in% param_custom_opti) param_def@photo_sig<-param_custom["photo_sig"]
  if("photo_sen" %in% param_custom_opti) param_def@photo_sen<-param_custom["photo_sen"]
  if("photo_opp" %in% param_custom_opti) param_def@photo_opp<-param_custom["photo_opp"]
  if("vern_base" %in% param_custom_opti) param_def@vern_base<-param_custom["vern_base"]
  if("vern_full" %in% param_custom_opti) param_def@vern_full<-param_custom["vern_full"]
  if("leave_prim_mx" %in% param_custom_opti) param_def@leave_prim_mx<-param_custom["leave_prim_mx"]
  if("leave_app_mx" %in% param_custom_opti) param_def@leave_app_mx<-param_custom["leave_app_mx"]
  if("stage_fi_fl_itv" %in% param_custom_opti) param_def@stage_fi_fl_itv<-param_custom["stage_fi_fl_itv"]
  if("node_mx" %in% param_custom_opti) param_def@node_mx<-param_custom["node_mx"]
  if("dev_v_max_rate" %in% param_custom_opti) param_def@dev_v_max_rate<-param_custom["dev_v_max_rate"]
  if("dev_r_max_rate" %in% param_custom_opti) param_def@dev_r_max_rate<-param_custom["dev_r_max_rate"]
  
  # execute model for all available measured sites
  resultDF<-list()
  list_se<-list()
  list_sites<-names(list_measured_data)
  for(i in 1:length(list_sites)){
    measured_df<-list_measured_data[[list_sites[i]]]
    sown_date<-as.Date(measured_df$date[measured_df$stage_ec==0])
    
    weather_df<-list_weather_data[[lookup.weather_station(list_sites[i])]]
    
    resultDF[[i]] <- wang.main(parameters_df = param_def,
                        weather_data_df = weather_df,end_stage=end_stage,sown_date = sown_date)
    
    list_se[list_sites[i]]<-rmse_day(resultDF[[i]],measured_df,end_stage)^2
  }
  
  rmse<-sqrt(do.call(sum,list_se)/length(list_se))
  print(
    paste(names(param_custom),param_custom," RMSE:",rmse))
  
  PE.iteration<<-PE.iteration+1
  PE.resultObj$param_custom_opti[PE.iteration]<<-param_custom_opti
  PE.resultObj$param_custom[PE.iteration]<<-param_custom
  PE.resultObj$RMSE[PE.iteration]<<-rmse
  PE.resultObj$resultDF[PE.iteration]<<-resultDF
  
  return(rmse)
}

cwm.rmse <- function( param_custom, param_custom_opti, weather_data_df,  param_def, measured_df, end_stage
){
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
  
  # execute model for all available measured sites
  resultDF<-list()
  list_se<-list()
  list_sites<-names(list_measured_data)
  for(i in 1:length(list_sites)){
    measured_df<-list_measured_data[[list_sites[i]]]
    sown_date<-as.Date(measured_df$date[measured_df$stage_ec==0])
    
    weather_df<-list_weather_data[[lookup.weather_station(list_sites[i])]]
    
    resultDF[[i]] <- cwm.main(parameters_df = param_def,
                             weather_data_df = weather_df,end_stage=end_stage)
    
    list_se[list_sites[i]]<-rmse_day(resultDF[[i]],measured_df,end_stage)^2
  }
  
  rmse<-sqrt(do.call(sum,list_se)/length(list_se))
  return(rmse)
}

rmse_day <- function(resultDF,measured_set,end_stage) {
  list_se<-c()
  #mode 1: single stage
  # measured_stage <- measured_set[1,"stage_ec"]
  # result<-subset(resultDF,stage_ec>=measured_stage & stage_ec<measured_stage+1)
  # rmse<-(measured_set[1,"day"]-mean(result$day))^2
  # rmse_array<-append(rmse_array,rmse)
  #mode 2: multiple stages
  for(i in 1:nrow(measured_set)){
    measured_row <- measured_set[i,]
    
    #skip sown date record
    if(measured_row$stage_ec==0||measured_row$day==0)next
    
    # control for phasic estimation
    if(measured_row$stage_ec>end_stage)break
    
    result<-subset(resultDF,stage_ec>=measured_row$stage_ec & stage_ec<next_ec(measured_row$stage_ec))
    error<-0
    if(min(result$day)>measured_row$day)error <- min(result$day)-measured_row$day
    if(max(result$day)<measured_row$day)error <- max(result$day)-measured_row$day
    
    se<-(error)^2
    list_se<-append(list_se,se)
  }
  rmse<-sqrt(sum(list_se)/length(list_se))
  return(rmse)
}
  
