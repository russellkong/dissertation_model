#'##################################################
#'Result
#'##################################################
#'============================================================
#' 1. Weather file
#'============================================================
#' calculate the rmse between two weather files / diff data.frame
#' 
#' @param str_fore_file 
#' @param str_act_file 
#' @param forecastDF 
#' @param actualDF 
#' @param start_date 
#' @param end_date 
analysis.temp.err.rmse <- function(str_fore_file=NULL,str_act_file=NULL,forecastDF=NULL, actualDF=NULL, start_date=NULL,end_date=NULL,comDF=NULL){
  if(is.null(comDF)) comDF<-analysis.input.temp_err(...)
  
  rmse<-sqrt(mean(with(comDF,ifelse(is.na(err),0,err^2)))) #no NA suppose to exist
  return(rmse)
}
#' display plot of variance and cumlative error along the selected period
analysis.temp.err.plot<- function(tempDiffDF){
  print(
    ggplot(tempDiffDF)+
    geom_line(aes(x=date,y=temp_avg_act,colour="Act. Temp."),size=1.5)+
    geom_line(aes(x=date,y=temp_avg_fore,colour="Fore. Temp."),size=1.5)+
    geom_line(aes(x=date,y=err,colour="error"),linetype="dashed")+
    geom_line(aes(x=date,y=cum_err,colour="cum_err"),linetype="dashed") +
      xlab("Temperature (oC)") +
      ggtitle("analysis.temp.err.plot")
  )
  #' between dataset
  cor.test(tempDiffDF$temp_avg_fore, tempDiffDF$temp_avg_act, alternative="two.sided", method="pearson")
  scatterplot(temp_avg_fore~temp_avg_act, reg.line=lm, smooth=FALSE, spread=FALSE, 
              boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=tempDiffDF)
  
  #' On the deviation
  #shapiro-wilk normality test
  shapiro.test(tempDiffDF$err)
  #One-sample Kolmogorov-Smirnov test
  ks.test(tempDiffDF$err,pnorm,alternative="two.sided")
}

analysis.temp.err <- function(str_fore_file=NULL,str_act_file=NULL,forecastDF=NULL, actualDF=NULL, start_date=NULL,end_date=NULL){
  if(!is.null(str_fore_file)){
    forecastDF<-load.weather(str_weather_file = str_fore_file,start_date = start_date,end_date =end_date)
  }
  if(!is.null(str_act_file)){
    actualDF<-load.weather(str_weather_file = str_act_file,start_date = start_date,end_date =end_date)
  }
  forecastDF<-filter(forecastDF,date>=start_date & date<=end_date)
  actualDF<-filter(actualDF,date>=start_date & date<=end_date)
  
  tempDiffDF<-merge(forecastDF[,c("date","temp_min","temp_avg","temp_max")],actualDF[,c("date","temp_min","temp_avg","temp_max")],by="date",all=TRUE,suffixes = c("_fore","_act"))
  
  tempDiffDF$err<-with(tempDiffDF, temp_avg_fore-temp_avg_act)
  tempDiffDF$cum_err<-with(tempDiffDF, cumsum(ifelse(is.na(err),0,err)))
  return(tempDiffDF)
}

#'
#'Calculate the daily min, avg, max, sd of hourly or more frequent temperature record
#'output to "OUTPUT_DAILY" sheet in the same file
analysis.temp.daily_var<-function(str_weather_file_hourly){
  
}


#'============================================================
#' 2. photoperiod
#' ===========================================================
analysis.photo.err <- function(){
  
}

#' ===========================================================
#' 3. model GS day
#' ===========================================================
#' A. Obs to sim(measured)
#' -----------------------------------------------------------
#' Calculate RMSE of min. days difference between a simulated result set to observed data
#' Input of start_stage and end_stage as control for phasic analysis
#'
#' @param simDF 
#' @param obsDF 
#' @param start_stage 
#' @param end_stage 
analysis.obsDay.err.rmse <- function(obsDF,simDF,start_stage=0,end_stage=99) {
  day_err_df<-analysis.obsDay.err(simDF=simDF,obsDF=obsDF,start_stage=start_stage,end_stage=end_stage)
  errList<-c()
  for(i in 1:nrow(day_err_df)){
    obs_row <- day_err_df[i,]
    
    #skip sown date record and perform range selection
    if(obs_row$stage_ec==0||obs_row$stage_ec<start_stage||obs_row$day==0)next
    # perform range selection
    if(obs_row$stage_ec>end_stage)break
    
    errList<-append(errList,obs_row$err)
  }
  rmse<-sqrt(mean(errList^2))
  return(rmse)
}
analysis.obsDay.err <- function(obsDF,simDF,simDFs,start_stage=0,end_stage=99) {
  #day_err_df<-c()
  #mode 1: single stage
  # measured_stage <- measured_set[1,"stage_ec"]
  # result<-subset(resultDF,stage_ec>=measured_stage & stage_ec<measured_stage+1)
  # rmse<-(measured_set[1,"day"]-mean(result$day))^2
  # rmse_array<-append(rmse_array,rmse)
  #mode 2: multiple stages
  
  #init err column
  obsDF$err<- NA_real_
  
  for(i in 1:nrow(obsDF)){
    obs_row <- obsDF[i,]
    
    #skip sown date record and perform range selection
    if(obs_row$stage_ec==0||obs_row$stage_ec<start_stage||obs_row$day==0)next
    
    # perform range selection
    if(obs_row$stage_ec>end_stage)break
    
    result<-simDF[stage_ec>=obs_row$stage_ec & stage_ec<next_ec(obs_row$stage_ec)]
    if(nrow(result)==0){
      if(max(simDF$stage_ec)>obs_row$stage_ec){
        #case 1: stage skipped in simulation
        # use min day of next available stage
        result<-simDF[simDF$stage_ec>=next_ec(obs_row$stage_ec),][1,]
      }else{
        #case 2: stage unreachable
        # keep it inf? use the last available day?
        result<-tail(simDF,1)
      }
    }
    # If the date range of the simulated result of a stage is not containing the measured day, count error
    error<-0
    if(min(result$day)>obs_row$day)error <- min(result$day)-obs_row$day
    if(max(result$day)<obs_row$day)error <- max(result$day)-obs_row$day
    
    obsDF[i,"err"]<-error
    #day_err_df<-append(day_err_df,error)
  }
  return(obsDF)
}


#' Calculate RMSE of stage difference between a simulated result set to observed data on the observed day.
#' Input of start_stage and end_stage as boundary for phasic analysis
analysis.gs.err.rmse <- function(obsDF,simDF,start_stage=0,end_stage=99) {
  day_err_df<-analysis.gs.err(simDF=simDF,obsDF=obsDF,start_stage=start_stage,end_stage=end_stage)
  errList<-c()
  for(i in 1:nrow(day_err_df)){
    obs_row <- day_err_df[i,]
    
    #skip sown date record and perform range selection
    if(obs_row$stage_ec==0||obs_row$stage_ec<start_stage||obs_row$day==0)next
    # perform range selection
    if(obs_row$stage_ec>end_stage)break
    
    errList<-append(errList,obs_row$err)
  }
  rmse<-sqrt(mean(errList^2))
  return(rmse)
}
analysis.gs.err <- function(obsDF,simDF,simDFs,start_stage=0,end_stage=99) {

  #init err column
  obsDF$err<-NA_real_
  
  for(i in 1:nrow(obsDF)){
    obs_row <- obsDF[i,]
    
    #skip sown date record and perform range selection
    if(obs_row$stage_ec==0||obs_row$stage_ec<start_stage||obs_row$day==0)next
    
    # perform range selection
    if(obs_row$stage_ec>end_stage)break
    
    result<-simDF[day==obs_row$day]
    if(nrow(result)==0){
        #case 1: day unreachable
        # keep it inf? use the last available day?
        result<-tail(simDF,1)
    }
    # If the date range of the simulated result of a stage is not containing the measured day, count error
    error<-0
    error <- result$stage_ec-obs_row$stage_ec
    
    obsDF[i,"err"]<-error
    #day_err_df<-append(day_err_df,error)
  }
  return(obsDF)
}
#'--------------------------------------------------------
#' B. Sim(measured), sim(forecast)
#' C. Sim(measured_photo), sim(calculated_photo)
#' -----------------------------------------------------------
#' calculate the start day diff and duration of stages between simulated results
#' return a data.frame with c("stage", "DF_name", "first_day", "duration", "diff_du", "diff_fd")
#' between models, or weather sources
#' 
#' @param baseDF the subject to be compared by simDFList 
#' @param simDFList list of the objects need comparsion, names to identify model_source
analysis.model.sim_diff<-function(baseDF, simDFList){
  simDiffDF<-data.frame()
  simDFList<-list(baseDF,simDFList)
  for(i in 1:length(simDFList)){
    
    simDFList[[i]]$stage_ec<-sapply(simDFList[[i]]$stage_ec, FUN= rounddown_ec)
    
    tmpDF<-simDFList[[i]] %>% 
          group_by(stage_ec) %>%
          summarise(duration=n(),first_day=min(day))
    
    if(i==1){
      simDiffDF<-tmpDF
    }else{
      tmpDF$diff_du<-simDiffDF$duration - tmpDF$duration
      tmpDF$diff_fd<-simDiffDF$first_day - tmpDF$first_day
      names(tmpDF)[2:5]<-paste(names(tmpDF)[2:5],names(simDFList[i]),sep = "_")
      simDiffDF<-merge(x=simDiffDF,y=tmpDF,by="stage_ec",all = TRUE)
    }
  }

  return(simDiffDF)
}

analysis.model.sim_diff.plot<-function(simDiffDF){
  
}

#'##################################################
#'Discussion
#'##################################################
#'
#'Calcuate cumuative temperature error to deviation of simulated growth stage
#'
analysis.model.temp_gs_err_corr<-function(str_act_temp,str_fore_temp,actTempDF,foreTempDF,obsDF,simDF){
  start_date<-obsDF[1,]$date
  end_date<-obsDF[nrow(obsDF),]$date
  temp_err_df<-analysis.temp.err(str_act_file = str_act_temp,str_fore_file =  str_fore_temp,actualDF = actTempDF, forecastDF = foreTempDF,start_date = start_date,end_date = end_date)
  temp_err_df$day<-0:(nrow(temp_err_df)-1) #start_date is the sown date
  day_err_df<-analysis.obsDay.err(simDF = simDF, obsDF = obsDF)
  day_temp_err_df<-merge(x=temp_err_df[,c("date","day","err","cum_err")],y=day_err_df[,c("day","site","stage_ec","err")],by="day",all = TRUE, suffixes = c("_temp","_day"))
  cor.test(day_temp_err_df$cum_err, day_temp_err_df$err_day, alternative="two.sided", method="pearson")
  scatterplot(err_day~cum_err, reg.line=lm, smooth=FALSE, spread=FALSE, 
              boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=day_temp_err_df)
  return(day_temp_err_df)
}