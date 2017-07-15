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
cwm.main <-
  function(str_weather_file,
           str_param_file,
           str_outfile = NULL,
           conf_id = 1,
           sown_date = NULL,
           weather_actual_end = NULL,
           parameters_df = NULL,
           weather_data_df = NULL,
           end_stage = 99) {
    library(xlsx)
    
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
        load_weather(str_weather_file,
                     weather_actual_end = weather_actual_end,
                     start_date = sown_date)
    } else{
      weather_data <- subset(weather_data_df, date >= as.POSIXct(sown_date))
    }
    
    ## init variables
    dailyWeather <- new('Weather')
    
    # define parameters
    dailyPrediction <- new('CWmPrediction')
    
    #dailyPrediction<- init_prediction(dailyPrediction)
    dailyPrediction@stage_dev <- 8
    dailyPrediction@leave_prim <- parameters@leave_prim_init
    dailyPrediction@leave_emerg <- parameters@leave_emerg_init
    
    #' Modelling Loop
    print(paste("Number available weather days: ", nrow(weather_data)))
    cat("Start Processing: \n")
    for (i in 1:nrow(weather_data)) {
      weatherRow <- weather_data[i, ]
      ## bind daily weather data
      dailyWeather <- set_weather(weatherRow, dailyWeather)
      ## process the model
      dailyPrediction@day <- i
      dailyPrediction@date <- dailyWeather@date
      dailyPrediction <-
        cwm.process(dailyPrediction, parameters, dailyWeather)
      
      ##Log result
      dailyPrediction@weather_source <- dailyWeather@source
      if (exists("resultDF")) {
        resultDF <- rbind(resultDF, as.data.frame(dailyPrediction))
      } else{
        resultDF <- as.data.frame(dailyPrediction)
      }
      
      print(
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
      if (dailyPrediction@stage_ec >= 99 || dailyPrediction@stage_ec >=end_stage || i > 1000)
        break
    }
    
    #' Output handling
    if (!is.null(str_outfile))
      writeResult(str_outfile, resultDF, parameters)
    #write.xlsx(resultDF,file=str_outfile,append=TRUE)
    print(paste("Row processed: ", i))
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
  leave_prim<-dailyPrediction@leave_prim+max(0,dailyWeather@temp_avg-parameters@temp_base)/parameters@plastochron
  leave_emerg<-dailyPrediction@leave_emerg+max(0,dailyWeather@temp_avg-parameters@temp_base)/parameters@phyllochron
  dailyPrediction@leave_prim<-leave_prim
  dailyPrediction@leave_emerg<-leave_emerg
  
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
cwm.f_s2<-function(dailyPrediction,parameters,dailyWeather){
  #stage_dev_rate<-(max(0,dailyWeather@temp_avg-parameters@temp_base) * (3*parameters@phyllochron) #CW original
  leave_prim<-dailyPrediction@leave_prim+max(0,dailyWeather@temp_avg-parameters@temp_base)/parameters@plastochron
  leave_emerg<-dailyPrediction@leave_emerg+max(0,dailyWeather@temp_avg-parameters@temp_base)/parameters@phyllochron
  fl<-leave_prim-2-leave_emerg
  
  stage_dev_rate<-(max(0,dailyWeather@temp_avg-parameters@temp_base)) / (fl*parameters@phyllochron+parameters@ph39)
  stage_dev<-dailyPrediction@stage_dev+stage_dev_rate
  
  stage_ec<-dailyPrediction@stage_ec
  if(leave_prim-2<leave_emerg){
    if(dailyPrediction@stage_ec<37) stage_ec<-37
    stage_ec_rate<-min(2*max(0,dailyWeather@temp_avg-parameters@temp_base)/parameters@ph39,40-stage_ec)
  }else{
    stage_ec_rate<-max(0,dailyWeather@temp_avg-parameters@temp_base)/parameters@t_sum_internode
  }
  stage_ec<-max(30,stage_ec+stage_ec_rate)
  
  dailyPrediction@leave_prim<-leave_prim
  dailyPrediction@leave_emerg<-leave_emerg
  dailyPrediction@fl<-fl
  
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
#'
#'http://nowlin.css.msu.edu/wheat_book/CHAPTER2.html
#'RDR = 1 - k(50 - V)

cwm.f_vern<-function(dailyPrediction,parameters,dailyWeather){
  dailyPrediction@vern_resp_rate<- 1-(parameters@p1v/10000)*(parameters@p1vd-dailyWeather@temp_avg)
  return(dailyPrediction)
}

#'
#'http://nowlin.css.msu.edu/wheat_book/CHAPTER2.html
#'RDR = 1 - C(20-P)^2
cwm.f_photo<-function(dailyPrediction,parameters,dailyWeather){
  #DSSAT f90 code: long day plants
  dailyPrediction@photo_resp_rate<- 1-(parameters@p1d/10000)*(parameters@p1dt-dailyWeather@photo_len)^2
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
  parameter_table <- subset(parameter_table, id==conf_id)
  
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
