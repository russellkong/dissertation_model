## Coding of CWm's Small grain cereal phenology model
## Author: Russell Kong
## Date: 19 JUN 2017
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
#' @param weather_actual_end define end date of actual weather data reading, continue with predicted data
#' @param parameters_df custom dataframe for parameters. For calibration use. Ignore file location on applied
#' @param weather_data_df custom dataframe for weather data For calibration use, performance improvement on multiple iteration
#'
#' @return data.frame (result of prediction)
#' @export
#'
#' @examples cwm.main("weather.xlsx","CWM_Properties.xlsx", "out8.xlsx",1,"2016-02-01")
#' naffertonSimDF=cwm.main(weather_data_df = nafferton2016,str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 9,sown_date = "2016-04-17")
cwm.main <-
  function(str_weather_file,
           str_param_file,
           str_outfile = NULL,
           conf_id = 1,
           sown_date = NULL,
           weather_actual_end = NULL,
           parameters_df = NULL,
           weather_data_df = NULL,
           end_stage = 99,
           verbose=TRUE) {
    
    #' initation
    
    ## set parameters
    if (is.null(parameters_df)) {
      parameters <- new('CWmParameterSet')
      parameters <- cwm.set_param(str_param_file, parameters, conf_id)
    } else{
      parameters <- parameters_df
    }
    
    
    ## load Weather data
    if (is.null(weather_data_df)) {
      weather_data <-
        load.weather(str_weather_file,
                     weather_actual_end = weather_actual_end,
                     start_date = sown_date)
    } else{
      weather_data <- filter(weather_data_df, date >= as.POSIXct(sown_date) & date <= as.POSIXct(sown_date)+days(365))
    }
    
    ## init variables
    dailyWeather <- new('Weather')
    
    # define parameters
    dailyPrediction <- new('CWmPrediction')
    dailyPrediction@stage_dev <- 8
    dailyPrediction@stage_ec <- 0
    dailyPrediction@leave_prim <- parameters@leave_prim_init
    dailyPrediction@leave_emerg <- parameters@leave_emerg_init
    
    progress <- as.list(nrow(weather_data))
    
    #' Modelling Loop
    if(verbose)print_detail(paste("Number available weather days: ", nrow(weather_data)))
    if(verbose)print_progress("Start Processing: \n")
    
    for (i in 1:nrow(weather_data)) {
      weatherRow <- weather_data[i, ]
      ## bind daily weather data
      dailyWeather <- as.Weather(weatherRow, dailyWeather)
      ## process the model
      dailyPrediction@day <- i - 1
      dailyPrediction@date <- dailyWeather@date
      if(i>1)
        dailyPrediction <-
          cwm.process(dailyPrediction, parameters, dailyWeather)
      
      ##Log result
      dailyPrediction@weather_source <- dailyWeather@source
      # if (i > 1) {
      #   resultDF <- rbind(resultDF, as.data.frame(dailyPrediction))
      # } else{
      # resultDF <- as.data.frame(dailyPrediction)
      # }
      progress[[i]] <- dailyPrediction
      
      if(verbose)print_detail(
        paste(
          "Day ",
          i,
          " Stage ",
          dailyPrediction@stage_dev,
          "[",
          dailyPrediction@stage_ec,
          "]"
        )
      )
      ## log into csv file
      #wang.write_prediction(dailyPrediction)
      if (dailyPrediction@stage_ec > 99 || dailyPrediction@stage_ec >= next_ec(end_stage) || i > 1000)
        break
    }
    
    resultDF<-do.call(rbind,lapply(progress,"as.data.frame"))
    
    #' Output handling
    if (!is.null(str_outfile))
      writeResult(paste("./output/CWm",weather_data[1,]$site,str_outfile,sep = "_"), resultDF, parameters)
    #write.xlsx(resultDF,file=str_outfile,append=TRUE)
    if(verbose)print_progress(paste("Row processed: ", i))
    return(resultDF)
    ## close infile
    ## close outfile
  }

#'##########################
##phase module
#'##########################
#' process the prediction by one time frame
#' GS: 7,8,9,10->1,2,3,4,5,6
#' output: Prediction object
cwm.process<-function(dailyPrediction,parameters,dailyWeather){
  
  if (dailyPrediction@stage_dev < 2) {
    dailyPrediction <- cwm.f_s1(dailyPrediction, parameters, dailyWeather)
  } else if (dailyPrediction@stage_dev < 3) {
    dailyPrediction <- cwm.f_s2(dailyPrediction, parameters, dailyWeather)
  } else if (dailyPrediction@stage_dev < 4) {
    dailyPrediction <- cwm.f_s3(dailyPrediction, parameters, dailyWeather)
  } else if (dailyPrediction@stage_dev < 5) {
    dailyPrediction <- cwm.f_s4(dailyPrediction, parameters, dailyWeather)
  } else if (dailyPrediction@stage_dev < 6) {
    dailyPrediction <- cwm.f_s5(dailyPrediction, parameters, dailyWeather)
  } else if (dailyPrediction@stage_dev < 7) {
    dailyPrediction <- cwm.f_s6(dailyPrediction, parameters, dailyWeather)
  } else if (dailyPrediction@stage_dev < 9) {
    dailyPrediction <- cwm.f_s8(dailyPrediction, parameters, dailyWeather)
  } else if (dailyPrediction@stage_dev < 10) {
    dailyPrediction <- cwm.f_s9(dailyPrediction, parameters, dailyWeather)
  } 
  
  return(dailyPrediction)
}

cwm.f_s8<-function(dailyPrediction,parameters,dailyWeather){
  #cumulate vernalization
  dailyPrediction<-cwm.f_vern(dailyPrediction,parameters,dailyWeather)
  
  #calculate rates
  stage_dev_rate<-max(0,dailyWeather@temp_avg-parameters@temp_base)/parameters@p9
  stage_dev<-dailyPrediction@stage_dev+stage_dev_rate
  
  stage_ec_rate<-5*max(0,dailyWeather@temp_avg-parameters@temp_base)/parameters@p9
  stage_ec<-dailyPrediction@stage_ec+stage_ec_rate
 
  dailyPrediction@stage_dev_rate<-stage_dev_rate
  dailyPrediction@stage_dev<-stage_dev
  
  dailyPrediction@stage_ec_rate<-stage_ec_rate
  dailyPrediction@stage_ec<-stage_ec
  
  return(dailyPrediction)
}
cwm.f_s9<-function(dailyPrediction,parameters,dailyWeather){
  dailyPrediction<-cwm.f_s8(dailyPrediction,parameters,dailyWeather)
  if(dailyPrediction@stage_dev>10)dailyPrediction@stage_dev<-1
  return(dailyPrediction)
}

cwm.f_s1<-function(dailyPrediction,parameters,dailyWeather){
  #P.64 accumulation of leave formation temperature start from emerge
  dailyPrediction<-cwm.f_leave(dailyPrediction,parameters,dailyWeather)
  
  
  dailyPrediction<-cwm.f_vern(dailyPrediction,parameters,dailyWeather)
  dailyPrediction<-cwm.f_photo(dailyPrediction,parameters,dailyWeather)
  
  #'Developmental units
  #'  DU = TT*VF*DF*LIF2    ! NB. Changed from Ceres 3.5
  #'    ! DU = TT*AMIN1(VF,DF)*LIF2    ! deplicated since Ceres 3.5
  #'
  stage_dev_rate<-(max(0,dailyWeather@temp_avg-parameters@temp_base) * min(dailyPrediction@photo_resp_rate,dailyPrediction@vern_resp_rate))/(parameters@s1_therm_day*parameters@phyllochron/95)
  stage_dev<-dailyPrediction@stage_dev+stage_dev_rate
  
  stage_ec_rate<-max(0,dailyWeather@temp_avg-parameters@temp_base)/parameters@phyllochron
  stage_ec<-dailyPrediction@stage_ec+stage_ec_rate
  
  dailyPrediction@stage_dev_rate<-stage_dev_rate
  dailyPrediction@stage_dev<-stage_dev
  
  dailyPrediction@stage_ec_rate<-stage_ec_rate
  dailyPrediction@stage_ec<-stage_ec
  
  return(dailyPrediction)
}
cwm.f_leave<-function(dailyPrediction,parameters,dailyWeather){
  #end of leaf primordia formation
  if(dailyPrediction@stage_dev < parameters@gs_flp){
    dailyPrediction@leave_prim<-dailyPrediction@leave_prim+max(0,dailyWeather@temp_avg-parameters@temp_base)/parameters@plastochron
  }
  
  dailyPrediction@leave_emerg<-dailyPrediction@leave_emerg+max(0,dailyWeather@temp_avg-parameters@temp_base)/parameters@phyllochron
  
  #define fl value at GS=2
  if(dailyPrediction@stage_dev>=2 && dailyPrediction@fl==0)
    dailyPrediction@fl<-dailyPrediction@leave_prim-2-dailyPrediction@leave_emerg
  
  return(dailyPrediction)
}
cwm.f_s2<-function(dailyPrediction,parameters,dailyWeather){
  #stage_dev_rate<-(max(0,dailyWeather@temp_avg-parameters@temp_base) * (3*parameters@phyllochron) #CW original
  dailyPrediction<-cwm.f_leave(dailyPrediction,parameters,dailyWeather)
  
  stage_dev_rate<-(max(0,dailyWeather@temp_avg-parameters@temp_base)) / (dailyPrediction@fl*parameters@phyllochron+parameters@ph39)
  stage_dev<-dailyPrediction@stage_dev+stage_dev_rate
  
  stage_ec<-dailyPrediction@stage_ec
  if(dailyPrediction@leave_prim-2<dailyPrediction@leave_emerg){
    if(dailyPrediction@stage_ec<37) stage_ec<-37
    stage_ec_rate<-min(2*max(0,dailyWeather@temp_avg-parameters@temp_base)/parameters@ph39,40-stage_ec)
  }else{
    stage_ec_rate<-max(0,dailyWeather@temp_avg-parameters@temp_base)/parameters@t_sum_internode
  }
  stage_ec<-max(30,stage_ec+stage_ec_rate)
  
  dailyPrediction@stage_dev_rate<-stage_dev_rate
  dailyPrediction@stage_dev<-stage_dev
  
  dailyPrediction@stage_ec_rate<-stage_ec_rate
  dailyPrediction@stage_ec<-stage_ec
  
  return(dailyPrediction)
}
cwm.f_s3<-function(dailyPrediction,parameters,dailyWeather){
  stage_dev_rate<-max(0,dailyWeather@temp_avg-parameters@temp_base)/(2*parameters@phyllochron)
  stage_dev<-dailyPrediction@stage_dev+stage_dev_rate
  
  stage_ec_rate<-(4+1.7*(stage_dev-3))*10-dailyPrediction@stage_ec ##deduction for delta??
  stage_ec<-max(40,dailyPrediction@stage_ec+stage_ec_rate)
  
  dailyPrediction@stage_dev_rate<-stage_dev_rate
  dailyPrediction@stage_dev<-stage_dev
  
  dailyPrediction@stage_ec_rate<-stage_ec_rate
  dailyPrediction@stage_ec<-stage_ec
  return(dailyPrediction)
}
cwm.f_s4<-function(dailyPrediction,parameters,dailyWeather){
  stage_dev_rate<-max(0,dailyWeather@temp_avg-parameters@temp_base)/parameters@s4_therm_day
  stage_dev<-dailyPrediction@stage_dev+stage_dev_rate
  
  stage_ec_rate<-(5.7+1.4*(stage_dev-4))*10-dailyPrediction@stage_ec ##deduction for delta??
  stage_ec<-max(57,dailyPrediction@stage_ec+stage_ec_rate)
  
  dailyPrediction@stage_dev_rate<-stage_dev_rate
  dailyPrediction@stage_dev<-stage_dev
  
  dailyPrediction@stage_ec_rate<-stage_ec_rate
  dailyPrediction@stage_ec<-stage_ec
  return(dailyPrediction)
}
cwm.f_s5<-function(dailyPrediction,parameters,dailyWeather){
  stage_dev_rate<-max(0,(dailyWeather@temp_avg-parameters@temp_base-1))/((parameters@p5+21.5)/0.05)
  stage_dev<-dailyPrediction@stage_dev+stage_dev_rate
  
  stage_ec_rate<-(7.1+1.9*(stage_dev-5))*10-dailyPrediction@stage_ec ##deduction for delta??
  stage_ec<-max(71,dailyPrediction@stage_ec+stage_ec_rate)
  
  dailyPrediction@stage_dev_rate<-stage_dev_rate
  dailyPrediction@stage_dev<-stage_dev
  
  dailyPrediction@stage_ec_rate<-stage_ec_rate
  dailyPrediction@stage_ec<-stage_ec
  return(dailyPrediction)
}
cwm.f_s6<-function(dailyPrediction,parameters,dailyWeather){
  stage_dev_rate<-max(0,dailyWeather@temp_avg-parameters@temp_base)/parameters@s6_therm_day
  stage_dev<-dailyPrediction@stage_dev+stage_dev_rate
  
  stage_ec_rate<-(9+(stage_dev-6))*10-dailyPrediction@stage_ec ##deduction for delta??
  stage_ec<-max(90,dailyPrediction@stage_ec+stage_ec_rate)
  
  dailyPrediction@stage_dev_rate<-stage_dev_rate
  dailyPrediction@stage_dev<-stage_dev
  
  dailyPrediction@stage_ec_rate<-stage_ec_rate
  dailyPrediction@stage_ec<-stage_ec
  return(dailyPrediction)
}

#' function to handle vernalization
#' cumulate Vernalization days from germination
#' Ritchie 1991
#' RDR = 1 - k(50 - V); P1V = K * 183 - 0.55; useful scale 0-8
#'
#' @param dailyPrediction 
#' @param parameters 
#' @param dailyWeather 
cwm.f_vern<-function(dailyPrediction,parameters,dailyWeather){
  # values ref. Ritchie 1991
  v_opt_min=0
  v_opt_max=7
  v_max=18
  v_dever_temp=30 
  v_dever_day=10
  v_dever_rate=0.5
  
  if(dailyPrediction@stage_dev_rate<2 && dailyPrediction@vern_resp_rate<1){ #before anthesis and vernalization not fulfil yet
    # calculate Relative Vernalization Effectiveness, vernalization days
    if(dailyWeather@temp_avg<v_opt_min ) dailyPrediction@vern_eff<-0
    else if( dailyWeather@temp_avg<=v_opt_max) dailyPrediction@vern_eff<-1
    else if(dailyWeather@temp_avg<=v_max)dailyPrediction@vern_eff<-(dailyWeather@temp_avg-v_opt_max)/(v_max-v_opt_max)
    else dailyPrediction@vern_eff<-0
    
    #devernalization
    if(dailyWeather@temp_avg>v_dever_temp && dailyPrediction@vern_cum_day<v_dever_day)dailyPrediction@vern_eff<-(dailyWeather@temp_avg-v_dever_temp)*v_dever_rate
    
    dailyPrediction@vern_cum_day<-min(parameters@p1vd, max(0,dailyPrediction@vern_cum_day+dailyPrediction@vern_eff)) #0 < vd <= p1dv 
  }
  #vernalization rate calucation only required between GS 1-2
  if(dailyPrediction@stage_dev_rate>0 && dailyPrediction@stage_dev_rate<2){ 
    # parameters@p1vd>=dailyPrediction@vern_cum_day
    dailyPrediction@vern_resp_rate<- 1-((parameters@p1v+0.55)/183)*(parameters@p1vd-dailyPrediction@vern_cum_day)
  }
  return(dailyPrediction)
}

#'
#'Ritchis 1991
#'RDR = 1 - C(20-P)^2; P1D=C*500; useful scale 0-3
cwm.f_photo<-function(dailyPrediction,parameters,dailyWeather){
  dailyPrediction@photo_resp_rate<- 1-(parameters@p1d/500)*(parameters@p1dt-dailyWeather@photo_len)^2
  
  #DSSAT f90 code: long day plants
  #parameters@p1d/10000
  #DSSAT f90 code: short day plants
  # AMAX1(0.0,AMIN1(1.0,1.0-(ABS(P1D)/1000)*(DAYLT-P1DT)))
  return(dailyPrediction)
}

#'#########################
## misc functions
#'#########################

## read param
cwm.set_param <-function(str_param_file, parameters, conf_id=1){
  parameters@model<-as.character("cwm")
  
  parameter_table <- read.xlsx(str_param_file,sheetName = "parameters")
  parameter_table <- filter(parameter_table, id==conf_id)
  
  if(nrow(parameter_table)==0) stop("No param in file match selected conf_id", paste(str_param_file,conf_id,sep = "|"))
  if(nrow(parameter_table)>1) warning("Multiple param set selected, check param file. first row selected")
  
  row=1
  parameters@conf_id<-as.numeric(as.numeric(parameter_table[row,"id"]))
  parameters@temp_base<-as.numeric(parameter_table[row,"temp_base"])
  parameters@p9<-as.numeric(parameter_table[row,"p9"])
  parameters@phyllochron<-as.numeric(parameter_table[row,"phyllochron"])
  parameters@p1d<-as.numeric(parameter_table[row,"p1d"])
  parameters@p1dt<-as.numeric(parameter_table[row,"p1dt"])
  parameters@p1v<-as.numeric(parameter_table[row,"p1v"])
  parameters@p1vd<-as.numeric(parameter_table[row,"p1vd"])
  parameters@plastochron<-as.numeric(parameter_table[row,"plastochron"])
  parameters@gs_flp<-as.numeric(parameter_table[row,"gs_flp"])
  parameters@t_sum_internode<-as.numeric(parameter_table[row,"t_sum_internode"])
  parameters@ph39<-as.numeric(parameter_table[row,"ph39"])
  parameters@p5<-as.numeric(parameter_table[row,"p5"])
  parameters@s1_therm_day<-as.numeric(parameter_table[row,"s1_therm_day"])
  parameters@s4_therm_day<-as.numeric(parameter_table[row,"s4_therm_day"])
  parameters@s6_therm_day<-as.numeric(parameter_table[row,"s6_therm_day"])
  parameters@leave_prim_init<-as.numeric(parameter_table[row,"leave_prim_init"])
  parameters@leave_emerg_init<-as.numeric(parameter_table[row,"leave_emerg_init"])
  
  return(parameters)
}


# ## write header
# wang.write_header <-function(data){
#   slotnames <- slotNames(x) 
#   wang.write_output(slotnames)
# }
# ## write prediction
# wang.write_prediction <-function(data){
#   wang.write_output(data)
# }

## write header and content
# input: data.frame
cwm.write_output <-function(data){
  dataDF<-as.data.frame(data)
  write.csv(dataDF,file="out.csv",append=TRUE, sep=",")
}
