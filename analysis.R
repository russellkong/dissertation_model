#' calculate the rmse between two weather files
#' display plot of variance along the selected period
#' 
#' @param str_fore_file 
#' @param str_act_file 
#' @param forecastDF 
#' @param actualDF 
#' @param start_date 
#' @param end_date 
analysis.rmse_temp <- function(str_fore_file=NULL,str_act_file=NULL,forecastDF=NULL, actualDF=NULL, start_date=NULL,end_date=NULL){
  if(!is.null(str_fore_file)){
    forecastDF<-load_weather(str_weather_file = str_fore_file,start_date = start_date,end_date =end_date)
  }
  if(!is.null(str_act_file)){
    actualDF<-load_weather(str_weather_file = str_act_file,start_date = start_date,end_date =end_date)
  }
  comDF<-merge(forecastDF[,c("date","temp_min","temp_avg","temp_max")],actualDF[,c("date","temp_min","temp_avg","temp_max")],by="date",all=TRUE)
  
  comDF$dv<-with(comDF, temp_avg.x-temp_avg.y)
  comDF$cum_dv<-with(comDF, cumsum(ifelse(is.na(dv),0,dv)))
  
  print(
    ggplot(comDF)+
    geom_line(aes(x=date,y=temp_avg.x),colour="red",size=1.5)+
    geom_line(aes(x=date,y=temp_avg.y),colour="blue",size=1.5)+
    geom_line(aes(x=date,y=dv),colour="red",linetype="dashed")+
    geom_line(aes(x=date,y=cum_dv),colour="blue",linetype="dashed")
  )
  #' between dataset
  cor.test(comDF$temp_avg.x, comDF$temp_avg.y, alternative="two.sided", method="pearson")
  scatterplot(temp_avg.y~temp_avg.x, reg.line=lm, smooth=FALSE, spread=FALSE, 
              boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=comDF)
  
  #' On the deviation
  #shapiro-wilk normality test
  shapiro.test(comDF$dv)
  #One-sample Kolmogorov-Smirnov test
  ks.test(comDF$dv,pnorm,alternative="two.sided")
  
  rmse<-sqrt(mean(with(comDF,ifelse(is.na(dv),0,dv^2))))
  return(comDF)
}


#' Calculate RMSE between a simulated result set to observed data
#' Input of start_stage and end_stage as control for phasic analysis
#' @param resultDF 
#' @param measured_set 
#' @param end_stage 
analysis.rmse_day <- function(simDF,obsDF,start_stage=0,end_stage=99) {
  list_se<-c()
  #mode 1: single stage
  # measured_stage <- measured_set[1,"stage_ec"]
  # result<-subset(resultDF,stage_ec>=measured_stage & stage_ec<measured_stage+1)
  # rmse<-(measured_set[1,"day"]-mean(result$day))^2
  # rmse_array<-append(rmse_array,rmse)
  #mode 2: multiple stages
  for(i in 1:nrow(obsDF)){
    obs_row <- obsDF[i,]
    
    #skip sown date record and perform range selection
    if(obs_row$stage_ec==0||obs_row$stage_ec<start_stage||obs_row$day==0)next
    
    # perform range selection
    if(obs_row$stage_ec>end_stage)break
    
    result<-filter(simDF,stage_ec>=obs_row$stage_ec & stage_ec<next_ec(obs_row$stage_ec))
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
    
    se<-(error)^2
    list_se<-append(list_se,se)
  }
  rmse<-sqrt(sum(list_se)/length(list_se))
  return(rmse)
}

#'
#'Calculate the daily min, avg, max, sd of hourly or more frequent temperature record
#'output to "OUTPUT_DAILY" sheet in the same file
analysis.temp.daily_var<-function(str_weather_file_hourly){
  
}

#'
#'Calcuate cumuative temperature varience to deviation of simulated growth stage