
#' Model parameter estimation on one dataset
#' 
wang.calibrate <- function(str_weather_file,
                           str_conf_file,
                           str_measured_file,
                           str_outfile = NULL,
                           conf_id = 1,
                           sown_date = NULL,
                           weather_actual_end = NULL){
  weather_data_df <-
    load_weather(str_weather_file,
                 weather_actual_end = weather_actual_end,
                 start_date = sown_date)
  
  parameters <- new('WangParameterSet')
  parameters <- wang.set_conf(str_conf_file, parameters, conf_id)
  
  measured_df <- read_excel(str_measured_file)
  
  #Parameter to estimate
  parameters_opti["temp_v_opt"]<- 24
  system.time(OLS1p <- 
                try(nls(0~wang.rmse(weather_data_df,parameters_def,measured_df,parameters_opti),
                        start= list(parameters_opti=parameters_opti))))
  
  return(OLS1p)
  
}

#'=================================================
#' Wrapper function for models 
#' 1. fix parameters on each iteration
#' 2. call model on each measured context
#' 3. extract the simulated values with measured values
#' ================================================
#' @param weather_data_df 
#' @param parameter_df 
#' @param measured_set 
#' @param temp_cardinal 
#' @param temp_emerg_sum 
#' @param temp_base 
#' @param photo_crit 
#' @param photo_sig 
#' @param photo_sen 
#' @param photo_opp 
#' @param vern_base 
#' @param vern_full 
#' @param leave_prim_mx 
#' @param leave_app_mx 
#' @param stage_fi_fl_itv 
#' @param node_mx 
#' @param stage_vr_ec_table 
#' @param dev_v_max_rate 
#' @param dev_r_max_rate 
wang.rmse <- function( weather_data_df,  parameter_df, measured_set, 
                       parameters_opti
){
  #replace default values in conf file
  if("temp_v_min"  %in% names(parameters_opti)) parameter_df@temp_cardinal["V","MIN"]<-temp_v_min
  if("temp_v_opt"  %in% names(parameters_opti)) parameter_df@temp_cardinal["V","OPT"]<-temp_v_opt
  if("temp_v_max"  %in% names(parameters_opti)) parameter_df@temp_cardinal["V","MAX"]<-temp_v_max
  if("temp_r_min"  %in% names(parameters_opti)) parameter_df@temp_cardinal["R","MIN"]<-temp_r_min
  if("temp_r_opt"  %in% names(parameters_opti)) parameter_df@temp_cardinal["R","OPT"]<-temp_r_opt
  if("temp_r_max"  %in% names(parameters_opti)) parameter_df@temp_cardinal["R","MAX"]<-temp_r_max
  if("temp_vn_min"  %in% names(parameters_opti)) parameter_df@temp_cardinal["VN","MIN"]<-temp_vn_min
  if("temp_vn_opt"  %in% names(parameters_opti)) parameter_df@temp_cardinal["VN","OPT"]<-temp_vn_opt
  if("temp_vn_max"  %in% names(parameters_opti)) parameter_df@temp_cardinal["VN","MAX"]<-temp_vn_max
  if("temp_if_min"  %in% names(parameters_opti)) parameter_df@temp_cardinal["IF","MIN"]<-temp_if_min
  if("temp_if_opt"  %in% names(parameters_opti)) parameter_df@temp_cardinal["IF","OPT"]<-temp_if_opt
  if("temp_if_max"  %in% names(parameters_opti)) parameter_df@temp_cardinal["IF","MAX"]<-temp_if_max
  
  if("temp_emerg_sum"  %in% names(parameters_opti)) parameter_df@temp_emerg_sum<-temp_emerg_sum
  if("temp_base"  %in% names(parameters_opti)) parameter_df@temp_base<-temp_base
  if("photo_crit"  %in% names(parameters_opti)) parameter_df@photo_crit<-photo_crit
  if("photo_sig"  %in% names(parameters_opti)) parameter_df@photo_sig<-photo_sig
  if("photo_sen"  %in% names(parameters_opti)) parameter_df@photo_sen<-photo_sen
  if("photo_opp"  %in% names(parameters_opti)) parameter_df@photo_opp<-photo_opp
  if("vern_base"  %in% names(parameters_opti)) parameter_df@vern_base<-vern_base
  if("vern_full"  %in% names(parameters_opti)) parameter_df@vern_full<-vern_full
  if("leave_prim_mx"  %in% names(parameters_opti)) parameter_df@leave_prim_mx<-leave_prim_mx
  if("leave_app_mx"  %in% names(parameters_opti)) parameter_df@leave_app_mx<-leave_app_mx
  if("stage_fi_fl_itv"  %in% names(parameters_opti)) parameter_df@stage_fi_fl_itv<-stage_fi_fl_itv
  if("node_mx"  %in% names(parameters_opti)) parameter_df@node_mx<-node_mx
  if("dev_v_max_rate"  %in% names(parameters_opti)) parameter_df@dev_v_max_rate<-dev_v_max_rate
  if("dev_r_max_rate"  %in% names(parameters_opti)) parameter_df@dev_r_max_rate<-dev_r_max_rate
  
  resultDF <- wang.main(parameters_df = parameter_df,
                        weather_data_df = weather_data_df)

  return(rmse_day(resultDF,measured_set))
}

cwm.rmse <- function( weather_data_df,  parameter_df, measured_set, 
                      parameters_opti
){
  #replace default values in conf file
  if("temp_base"  %in% names(parameters_opti)) parameter_df@temp_base<-temp_base
  if("p1d"  %in% names(parameters_opti)) parameter_df@p1d<-p1d
  if("p1dt"  %in% names(parameters_opti)) parameter_df@p1dt<-p1dt
  if("p1v"  %in% names(parameters_opti)) parameter_df@p1v<-p1v
  if("p1vd"  %in% names(parameters_opti)) parameter_df@p1vd<-p1vd
  if("phyllochron"  %in% names(parameters_opti)) parameter_df@phyllochron<-phyllochron
  if("plastochron"  %in% names(parameters_opti)) parameter_df@plastochron<-plastochron
  if("gs_flp"  %in% names(parameters_opti)) parameter_df@gs_flp<-gs_flp
  if("t_sum_internode"  %in% names(parameters_opti)) parameter_df@t_sum_internode<-t_sum_internode
  if("ph39"  %in% names(parameters_opti)) parameter_df@ph39<-ph39
  if("p5"  %in% names(parameters_opti)) parameter_df@p5<-p5
  if("p9"  %in% names(parameters_opti)) parameter_df@p9<-p9
  if("s1_therm_day"  %in% names(parameters_opti)) parameter_df@s1_therm_day<-s1_therm_day
  if("s4_therm_day"  %in% names(parameters_opti)) parameter_df@s4_therm_day<-s4_therm_day
  if("s6_therm_day"  %in% names(parameters_opti)) parameter_df@s6_therm_day<-s6_therm_day
  if("leave_prim_init"  %in% names(parameters_opti)) parameter_df@leave_prim_init<-leave_prim_init
  if("leave_emerg_init"  %in% names(parameters_opti)) parameter_df@leave_emerg_init<-leave_emerg_init
  
  resultDF <- cwm.main(parameters_df = parameter_df,
                       weather_data_df = weather_data_df)
  
  return(rmse_day(resultDF,measured_set))
}

rmse_day <- function(resultDF,measured_set) {
  rmse_v<-c()
  #mode 1: single stage
  # measured_stage <- measured_set[1,"stage_ec"]
  # result<-subset(resultDF,stage_ec>=measured_stage & stage_ec<measured_stage+1)
  # rmse<-(measured_set[1,"day"]-mean(result$day))^2
  # rmse_array<-append(rmse_array,rmse)
  #mode 2: multiple stages
  for(i in 1:measured_set){
    measured_row <- measured_set[i,]
    
    result<-subset(resultDF,stage_ec>=measured_row$stage_ec & stage_ec<next_ec(measured_row$stage_ec))
    error<-0
    if(min(result$day)>measured_row$day)error <- min(result$day)-measured_row$day
    if(max(result$day)<measured_row$day)error <- max(result$day)-measured_row$day
    
    rmse<-(error)^2
    rmse_v<-append(rmse_v,rmse)
  }
  return(sum(rmse_v))
}
  