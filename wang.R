library(readxl)
library(xlsx)
library(dplyr)
library(data.table)
library(lubridate)
## Coding of WE's Small grain cereal phenology model
## Author: Russell Kong
## Date: 16 JUN 2017
## version: 0

#'############################
#'# master script to execute
#'############################
#'
#' @param str_weather_file weather file location
#' @param str_param_file parameter file location
#' @param str_outfile output prediction file location
#' @param conf_id row id of parameters set to use from the parameter file
#' @param sown_date the start date of simulation 
#' @param weather_actual_end_date define end date of actual weather data reading, continue with predicted data
#' @param weather_actual_end_stage define stage to use forecast weather data
#' @param parameters_df custom dataframe for parameters. For calibration use. Ignore file location on applied
#' @param weather_data_dt custom dataframe for weather data For calibration use, performance improvement on multiple iteration
#'
#' @return data.frame (result of prediction)
#' @export
#'
#' @examples we.main("weather.xlsx","Properties.xlsx", "out8.xlsx",1,"2016-02-01")
#' @examples we.main("./Weather/W_100EA002_2016.xlsx","./Parameters/WE_Parameters.xlsx", str_outfile="out8.xlsx",conf_id=4,sown_date="2016-04-21")
#' we.main(str_weather_file = "./Weather/W_100EA002_2016.xlsx",str_weather_forecast_file = "./Weather/CocklePark_forecast_2016.xlsx",weather_actual_end_stage = 50, str_param_file = "./Parameters/WE_Parameters.xlsx", conf_id=7,sown_date="2016-04-21")
we.main <-
  function(str_weather_file=NULL,str_weather_forecast_file=NULL,
           str_param_file,
           str_outfile = NULL,
           conf_id = 1,
           sown_date = NULL,
           weather_actual_end_date = NULL,
           weather_actual_end_stage= NULL,
           parameters_df = NULL,
           weather_data_dt = NULL,
           weather_forecast_dt = NULL,
           end_stage = 99,
           verbose=TRUE,
           mode="Normal") {
    
    #' initation
    sown_date<-as.POSIXct(sown_date,tz = "UTC")
    if(!is.null(weather_actual_end_date))
      weather_actual_end_date<-as.POSIXct(weather_actual_end_date,tz = "UTC")
    
    ## set parameters
    if (is.null(parameters_df)) {
      parameters <- new('WEParameterSet')
      parameters <- we.set_param(str_param_file, parameters, conf_id)
    }else{
      parameters <- parameters_df
    }
    
    
    ## load Weather data
    weather_data=NULL
    if (!is.null(weather_data_dt)) {
      weather_data <- weather_data_dt[date >= sown_date & date <= sown_date+days(365)]
    }else if(!is.null(str_weather_file)) {
      weather_data <-
        load.weather(str_weather_file,
                     start_date = sown_date)
    }
    weather_forecast=NULL
    if (!is.null(weather_forecast_dt)){
      weather_forecast <- weather_forecast_dt[date >= sown_date & date <= sown_date+days(365)]
    } else if(!is.null(str_weather_forecast_file)) {
      weather_forecast <-
        load.weather(str_weather_forecast_file,
                     start_date = sown_date,
                     mode="forecast")
    }
    if(!exists("weather_data") && !exists("weather_forecast")){stop("No input weather")}
    
    
    ## init variables
    dailyWeather <- new('Weather')
    
    # define parameters
    dailyPrediction <- new('WEPrediction')
    dailyPrediction<-as.data.frame(dailyPrediction)
    dailyPrediction$stage_dev <- -1
    dailyPrediction$stage_ec <- 0
    
    progress <- as.list(365)
    if(mode=="ProjectForecast" && !is.null(weather_forecast)){
      forecastProgress<-as.list(365)#section 2
    }
    #' Modelling Loop
    if(verbose)print_detail(paste("Number available weather days: ", nrow(weather_data)))
    if(verbose)print_progress("Start Processing: ")
    i=1
    cur_date=sown_date
    while (dailyPrediction$stage_dev < 2 &&
           dailyPrediction$stage_ec < next_ec(end_stage) && 
           i < 365 ) {
      
      if(!is.null(weather_data) && 
         (is.null(weather_actual_end_stage) || dailyPrediction$stage_ec<weather_actual_end_stage) &&
         (is.null(weather_actual_end_date) || cur_date<weather_actual_end_date)
          ){
        weatherRow <- weather_data[date == cur_date,]
        if(nrow(weatherRow)==0 && !is.null(weather_forecast)){
            weatherRow <- weather_forecast[date == cur_date,]
        }
      }else if(!is.null(weather_forecast)){
        weatherRow <- weather_forecast[date == cur_date,]
      }
      if(nrow(weatherRow)==0){
        print_critical(paste("No matching weather data for the date|",cur_date))
        break
      }
      
      ## bind daily weather data
      dailyWeather <- weatherRow#as.Weather(weatherRow, dailyWeather)
      ## process the model
      dailyPrediction$day <- i - 1
      dailyPrediction$date <- dailyWeather$date
      if(i>1)#skip sown day
        dailyPrediction <-
          we.process(dailyPrediction, parameters, dailyWeather)
      ## projection of EC stage
      dailyPrediction <-
        we.f_stage_ec(dailyPrediction, parameters)
      ##Log result
      dailyPrediction$weather_source <- dailyWeather$source
      # if (i > 1) {
      #   resultDF <- rbind(resultDF, as.data.frame(dailyPrediction))
      # } else{
        # resultDF <- as.data.frame(dailyPrediction)
      # }
      progress[[i]] <- dailyPrediction
      
      #Section 2: project forecast to 9 days
      if(mode=="ProjectForecast" && !is.null(weather_forecast)){
        forecastPrediction<-dailyPrediction
        for(j in 1:9){
          # single forecast file used, ideal if daily forecast of each following day from the cur_date is available
          forecastWeatherRow <- weather_forecast[date == cur_date+days(j),]
          if(nrow(forecastWeatherRow)==0) {
            print_critical(paste("No matching weather data for forecast date|",cur_date,cur_date+days(j))) 
            break
          }
          forecastPrediction$day <- i-1+j
          forecastPrediction$date <- cur_date+days(j)
          forecastPrediction$weather_source <- forecastWeatherRow$source
          forecastPrediction<-we.process(forecastPrediction,parameters,forecastWeatherRow )
        }
        #only the ninth day retained
        forecastProgress[[i]]<-forecastPrediction
      }
      
      if(verbose)print_detail(
        paste(
          "Day ",
          i,
          " Stage ",
          dailyPrediction$stage_dev,
          "[",
          dailyPrediction$stage_ec,
          "]"
        )
      )
      ## log into csv file
      #we.write_prediction(dailyPrediction)
      i<-i+1
      cur_date<-cur_date+days(1)
    }
    
    resultDF<-rbindlist(progress)
    if(mode=="ProjectForecast" && !is.null(weather_forecast)){
      forecastResultDF<-rbindlist(forecastProgress)
    }
    
    #' Output handling
    if (!is.null(str_outfile)){
      writeResult(paste("./output/WE",weather_data[1,]$site,str_outfile,sep = "_"), resultDF, parameters)
      if(mode=="ProjectForecast" && !is.null(weather_forecast)){
        writeResult(paste("./output/WE",weather_data[1,]$site,str_outfile,sep = "_"), forecastResultDF, parameters,sheet="forecast")
      }
    }
    if(verbose)print_progress(paste("Row processed: ", i))
    
    if(mode=="ProjectForecast" && !is.null(weather_forecast)){
      return(list(result=resultDF,forecast=forecastResultDF))
    }
    
    return(resultDF)
  }

#'##########################
##phase module
#'##########################
#' process the prediction by one time frame
#' output: Prediction object
we.process<-function(dailyPrediction,parameters,dailyWeather){
  if(dailyPrediction$stage_dev< -0.5){
    dailyPrediction<- we.germination(dailyPrediction,parameters,dailyWeather)
  }else if(dailyPrediction$stage_dev<0){
    dailyPrediction<- we.f_emerge(dailyPrediction,parameters,dailyWeather)
    #  }else if(dailyPrediction$stage_dev<0.4){
    #    we.f_terminal_spikelet()
  }else if(dailyPrediction$stage_dev<1){
    dailyPrediction<- we.f_anthesis(dailyPrediction,parameters,dailyWeather)
    if(dailyPrediction$stage_dev>=0 && dailyPrediction$stage_dev<0.45){
      dailyPrediction<-we.f_leave(dailyPrediction,parameters,dailyWeather)
      dailyPrediction<-we.f_tiller(dailyPrediction,parameters,dailyWeather)
    } else if(dailyPrediction$stage_dev>=0.45 && dailyPrediction$stage_dev<0.65){
      dailyPrediction<-we.f_node(dailyPrediction,parameters,dailyWeather)
    }
  }else if(dailyPrediction$stage_dev<2){
    dailyPrediction<- we.f_maturity(dailyPrediction,parameters,dailyWeather)
  }
  dailyPrediction<-we.f_stage_ec(dailyPrediction, parameters)
  
  return(dailyPrediction)
}
## ref. P2., (J.R.Kiniry 1986- CERES-Maize) Assume same as CW model such that 1 day
we.germination<-function(dailyPrediction,parameters,dailyWeather){
  dailyPrediction<- we.f_vern_resp(dailyPrediction,parameters,dailyWeather)
  
  dailyPrediction$dev_em_rate<- 0.5
  dailyPrediction$stage_dev<- dailyPrediction$stage_dev+dailyPrediction$dev_em_rate
  return(dailyPrediction)
}
##ref. P.3
we.f_emerge<-function(dailyPrediction,parameters,dailyWeather){
  dailyPrediction<- we.f_vern_resp(dailyPrediction,parameters,dailyWeather)
  
  dailyPrediction$dev_em_rate<- (dailyWeather$temp_avg-parameters@temp_base)*0.5 / parameters@temp_emerg_sum
  dailyPrediction$stage_dev<- dailyPrediction$stage_dev+dailyPrediction$dev_em_rate
  return(dailyPrediction)
}

we.f_terminal_spikelet<-function(){
  
}

##ref. P.3
we.f_anthesis<-function(dailyPrediction,parameters,dailyWeather){
  dailyPrediction<- we.f_temp_resp(dailyPrediction,parameters,dailyWeather,"V")
  dailyPrediction<- we.f_photo_resp(dailyPrediction,parameters,dailyWeather)
  dailyPrediction<- we.f_vern_resp(dailyPrediction,parameters,dailyWeather)
  dailyPrediction$dev_v_rate<- 1/parameters@dev_v_min_day *dailyPrediction$temp_resp_rate *dailyPrediction$photo_resp_rate *dailyPrediction$vern_resp_rate
  dailyPrediction$stage_dev<- dailyPrediction$stage_dev + dailyPrediction$dev_v_rate
  if(is.nan(dailyPrediction$dev_v_rate)){
    print_debug("hold!!! dailyPrediction$dev_v_rate is NaN")
  }
  return(dailyPrediction)
}

##ref. P.3
we.f_maturity<-function(dailyPrediction,parameters,dailyWeather){
  dailyPrediction<- we.f_temp_resp(dailyPrediction,parameters,dailyWeather,"R")
  dailyPrediction<- we.f_photo_resp(dailyPrediction,parameters,dailyWeather)
  #dailyPrediction<- we.f_vern_resp(dailyPrediction,parameters,dailyWeather)
  dailyPrediction$dev_r_rate<- 1/parameters@dev_r_min_day *dailyPrediction$temp_resp_rate
  dailyPrediction$stage_dev<- dailyPrediction$stage_dev + dailyPrediction$dev_r_rate
  if(is.nan(dailyPrediction$dev_v_rate)){
    print_debug("hold!!! dailyPrediction$dev_v_rate is NaN")
  }
  return(dailyPrediction)
}

#'##########################
##SUB function
#'##########################

## f(T) - Calculate temperature respond
## input: p_temp_cardinal, vr_temp_avg, phaseInd{V,R,VN,IF}
## output: vr_temp_resp_rate, _alpha 
## ref: P.4,8
we.f_temp_resp <-function(dailyPrediction,parameters,dailyWeather,phaseInd){
  if(dailyWeather$temp_avg>=parameters@temp_cardinal[phaseInd,"MIN"] && dailyWeather$temp_avg<=parameters@temp_cardinal[phaseInd,"MAX"]){
    temp_avg<-dailyWeather$temp_avg
    temp_max<-parameters@temp_cardinal[phaseInd,"MAX"]
    temp_min<-parameters@temp_cardinal[phaseInd,"MIN"]
    temp_opt<-parameters@temp_cardinal[phaseInd,"OPT"]
    
    if(temp_max<=temp_opt || temp_opt<=temp_min){warning("Temperature min,opt,max not in sequence",c(phaseInd,temp_max,temp_opt,temp_min))}
    alpha<-log(2)/log((temp_max-temp_min)/(temp_opt-temp_min))
    dailyPrediction$alpha<-alpha
    
    temp_resp_rate<-(2*(temp_avg-temp_min)^alpha*(temp_opt-temp_min)^alpha-(temp_avg-temp_min)^(2*alpha))/(temp_opt-temp_min)^(2*alpha)
    
  }else{
    temp_resp_rate<-0
  }
  if(is.nan(temp_resp_rate)){
    print_debug("hold!!! temp_resp_rate is NaN")
  }
  
  if(phaseInd %in% c("V","R")){
    dailyPrediction$temp_resp_rate<- temp_resp_rate
  } else if(phaseInd %in% c("VN")){
    dailyPrediction$vern_temp_resp<- temp_resp_rate
  } else if(phaseInd %in% c("IF")){
    dailyPrediction$leave_temp_resp<- temp_resp_rate
  }
  
  return(dailyPrediction)
}

## f(h_php) - Calculate photoperiod respond
## input: vr_photo_len, p_photo_crit, p_photo_sig, p_photo_opp
## output: _omega, vr_photo_resp_rate
## ref. P.6,7
we.f_photo_resp <-function(dailyPrediction,parameters,dailyWeather){
  ##init assignment of omega (defined or parameters deduced variable)
  if(length(dailyPrediction$omega)==0||dailyPrediction$omega==0){
    if(length(parameters@photo_sen)==0 || is.na(parameters@photo_sen)){
      omega<-4/abs(parameters@photo_opp-parameters@photo_crit)
    }else{
      omega<-parameters@photo_sen
    }
  }else{
    omega<-dailyPrediction$omega
  }
  
  photo_resp_rate<-min(1,max(0,1-exp(-parameters@photo_sig*omega*(dailyWeather$photo_len-parameters@photo_crit))))
  
  dailyPrediction$omega<-omega
  dailyPrediction$photo_resp_rate <-photo_resp_rate
  return(dailyPrediction)
}

## f(V) - Calculate vernerisation respond
## input: p_vern_base, p_vern_full, vr_vern_temp_sum, p_temp_cardinal, vr_temp_avg
## output: vr_vern_resp_rate, vr_vern_temp_sum
## ref: P5,6
we.f_vern_resp <-function(dailyPrediction,parameters,dailyWeather){
  dailyPrediction <- we.f_temp_resp(dailyPrediction,parameters,dailyWeather,"VN")
  if(parameters@vern_full==0){ ## no reduction from vernerisation
    dailyPrediction$vern_temp_sum<-1
    dailyPrediction$vern_resp_rate<-1
    
  }else{
    
    vern_temp_resp <-dailyPrediction$vern_temp_resp
    
    vern_temp_sum<- dailyPrediction$vern_temp_sum + vern_temp_resp
    vern_resp_rate<- min(1, 
                         max(0,
                             (vern_temp_sum-parameters@vern_base)/(parameters@vern_full/parameters@vern_base)
                         )
    )
    dailyPrediction$vern_temp_sum<-vern_temp_sum
    dailyPrediction$vern_resp_rate<-vern_resp_rate
  }
  if(is.nan(dailyPrediction$vern_temp_resp)){
    print_debug("hold!!! dailyPrediction$vern_temp_resp is NaN")
  }
  return(dailyPrediction)
}

## f(leave)
## input: vr_photo_resp_rate, p_temp_cardinal,vr_temp_avg, p_leave_prim_mx, p_leave_app_mx,vr_dev_v_rate,p_stage_fi_fl_itv,vr_stage_dev){
## output: vr_leave_prim_rate,vr_leave_app_rate,vr_leave_unemerge,vr_leave_sum
##assumption: after floral initiation, no new leaf primodia initiation
we.f_leave <-function(dailyPrediction,parameters,dailyWeather){
  if(dailyPrediction$stage_dev<0||dailyPrediction$stage_dev>0.65)return(dailyPrediction);
  
  ##R(If,app), A(if)
  if(dailyPrediction$stage_dev<=0.2){ ## b4 floral initiation
    ##R(prim,ini), A(if,u)
    dailyPrediction<-we.f_temp_resp(dailyPrediction,parameters,dailyWeather,"IF")
    temp_resp<-dailyPrediction$leave_temp_resp
    
    photo_resp<-dailyPrediction$photo_resp_rate
    
    leave_prim_rate<- parameters@leave_prim_mx*temp_resp*photo_resp
    leave_app_rate<- parameters@leave_app_mx*temp_resp*photo_resp
    leave_unemerge<-dailyPrediction$leave_unemerge+ leave_prim_rate -leave_app_rate
    leave_sum<-dailyPrediction$leave_sum+ leave_app_rate
    
    dailyPrediction$leave_prim_rate<-leave_prim_rate
    dailyPrediction$leave_app_rate<-leave_app_rate
    dailyPrediction$leave_sum<-leave_sum
    dailyPrediction$leave_unemerge<-leave_unemerge
  }else if(dailyPrediction$stage_dev<=0.65){ ## b4 flag leave
    leave_app_rate<- dailyPrediction$dev_v_rate*dailyPrediction$leave_unemerge/(0.65-0.2) #stage_fi_fl_itv
    leave_sum<-dailyPrediction$leave_sum+ leave_app_rate
    
    dailyPrediction$leave_app_rate<-leave_app_rate  
    dailyPrediction$leave_sum<-leave_sum
  }
  return(dailyPrediction)
}

## f(node)
## happen about terminal spikelet, assume stop at emd of vegetative stage
## input: p_node_mx,p_temp_cardinal,vr_temp_avg,vr_photo_resp_rate,vr_stage_dev
## output: vr_node_rate, vr_node_sum
we.f_node <-function(dailyPrediction,parameters,dailyWeather){
  if(dailyPrediction$stage_dev<0.45||dailyPrediction$stage_dev>1)return(dailyPrediction);
  node_rate<-parameters@node_mx*dailyPrediction$temp_resp_rate*dailyPrediction$photo_resp_rate
  node_sum<-dailyPrediction$node_sum+node_rate
  
  dailyPrediction$node_rate<-node_rate
  dailyPrediction$node_sum<-node_sum
  return(dailyPrediction)
}

## f(tiller)
## input: vr_leave_sum
## output: vr_till_main_sum 
we.f_tiller <-function(dailyPrediction,parameters,dailyWeather){
  if(dailyPrediction$stage_dev<0||dailyPrediction$stage_dev>0.45)return(dailyPrediction);
  till_main_sum<- max(0,dailyPrediction$leave_sum-2.5)
  dailyPrediction$till_main_sum<-till_main_sum
  return(dailyPrediction)
}

## function to calculate EC stage
## input: vr_stage_dev,vr_leave_sum,vr_node_sum,vr_till_main_sum,p_stage_vr_ec_tbl
## output: vr_stage_ec
we.f_stage_ec <-function(dailyPrediction,parameters){
  stage_dev<-dailyPrediction$stage_dev
  stage_ec<-0
  if(stage_dev<0){
    stage_ec<-10*(1+stage_dev)
  }else if(stage_dev<0.45){
    if(length(dailyPrediction$till_main_sum)==0 || dailyPrediction$till_main_sum==0){
      stage_ec<-min(20,10+dailyPrediction$leave_sum)
    }else{
      stage_ec<-min(30,20+dailyPrediction$till_main_sum)
    }
  }else if(stage_dev<0.65){
    stage_ec<-min(40,30+dailyPrediction$node_sum)
  }else if(stage_dev<=0.90){
    stage_ec<-40+10*(stage_dev-0.65)/(0.9-0.65)
  }else if(stage_dev<=1){
    stage_ec<-50+10*(stage_dev-0.9)/(1-0.9)
  }else if(stage_dev<=1.15){
    stage_ec<-60+10*(stage_dev-1)/(1.15-1)
  }else if(stage_dev<=1.5){
    stage_ec<-70+10*(stage_dev-1.15)/(1.5-1.15)
  }else if(stage_dev<=1.95){
    stage_ec<-80+10*(stage_dev-1.5)/(1.95-1.5)
  }else{
    stage_ec<-min(92,90+2*(stage_dev-1.95)/(2-1.95))
  }
  dailyPrediction$stage_ec<-stage_ec
  return(dailyPrediction)
}

#'#########################
## misc functions
#'#########################

## read param
we.set_param <-function(str_param_file, parameters, conf_id=1){
  parameters@model<-as.character("we")
  
  parameter_table <- read.xlsx(str_param_file,sheetName = "parameters")
  parameter_table <- filter(parameter_table, id==conf_id)
  
  if(nrow(parameter_table)==0) stop("No param in file match selected conf_id", paste(str_param_file,conf_id,sep = "|"))
  if(nrow(parameter_table)>1) warning("Multiple param set selected, check param file. first row selected")
  
  row=1
  parameters@conf_id<-as.numeric(parameter_table[row,"id"])
  parameters@temp_emerg_sum<-as.numeric(parameter_table[row,"temp_emerg_sum"])
  parameters@temp_base<-as.numeric(parameter_table[row,"temp_base"])
  parameters@photo_crit<-as.numeric(parameter_table[row,"photo_crit"])
  parameters@photo_sig<-as.numeric(parameter_table[row,"photo_sig"])
  parameters@photo_sen<-as.numeric(parameter_table[row,"photo_sen"])
  parameters@photo_opp<-as.numeric(parameter_table[row,"photo_opp"])
  #parameters@vern_base<-as.numeric(parameter_table[row,"vern_base"])
  
  parameters@vern_full<-as.numeric(parameter_table[row,"vern_full"])
  parameters@vern_base<-parameters@vern_full*0.2 ##Assumed ref. EW paper
  parameters@leave_prim_mx<-as.numeric(parameter_table[row,"leave_prim_mx"])
  parameters@leave_app_mx<-as.numeric(parameter_table[row,"leave_app_mx"])
  parameters@node_mx<-as.numeric(parameter_table[row,"node_mx"])
  parameters@dev_v_min_day<-as.numeric(parameter_table[row,"dev_v_min_day"])
  parameters@dev_r_min_day<-as.numeric(parameter_table[row,"dev_r_min_day"])
  temp_table <- read.xlsx(str_param_file,sheetName = "temp_cardinal")
  
  parameters@temp_cardinal <- data.frame(temp_table[,-1])
  rownames(parameters@temp_cardinal)<-temp_table$phase
  
  
  #  stage_table <- read.xlsx(file=str_param_file, header=TRUE, sheetName = "stage_vr_ec_table")
  #  parameters@stage_vr_ec_table <- stage_table
  
  return(parameters)
}