

#' Title
#'
#' @param weatherRow
#' @param weatherObj
#'
#' @return
#' @export
#'
#' @examples
as.Weather <- function(weatherRow, weatherObj=NULL) {
  #dailyWeather@date<-as.character(weatherRow$date)
  if(is.null(weatherObj))weatherObj<-new("Weather")
    weatherObj@date <-
    as.POSIXct(weatherRow$date, format = "%Y-%m-%d")
  weatherObj@temp_avg <- as.numeric(weatherRow$temp_avg)
  weatherObj@temp_min <- as.numeric(weatherRow$temp_min)
  weatherObj@temp_max <- as.numeric(weatherRow$temp_max)
  weatherObj@photo_len <- as.numeric(weatherRow$photo_len)
  weatherObj@source <- as.character(weatherRow$source)
  return(weatherObj)
}

#' Title
#'
#' @param str_weather_file excel file with weather data
#' @param start_date start of the weather data
#' @param end_date end of the loaded datafile
#' @param weather_actual_end input for case to merge actual & predict weather in a file
#' @param maxRow maximum amount of row loaded from excel sheet. [default: 3650 (10years)]
#'
#' @return
#' @export
#'
#' @examples
load.weather <-
  function(str_weather_file,
           start_date = NULL,
           end_date =NULL,
           weather_actual_end = NULL,
           maxRow = 3650) {
    sheets <- excel_sheets(str_weather_file)
    if (!"actual" %in% sheets && !"forecast" %in% sheets) {
      stop("No weather sheet 'actual' or 'forecast' in the file:",str_weather_file)
    }
    
    if(!is.null(start_date)) start_date<-as.POSIXct(start_date)
    if(!is.null(end_date)) end_date<-as.POSIXct(end_date)
    if(!is.null(weather_actual_end)) end_date<-as.POSIXct(weather_actual_end)
    
    if ("actual" %in% sheets) {
      actual_weather_data <-
        read_excel(str_weather_file, sheet = "actual", n_max = maxRow)
      if(!is.null(start_date))
        actual_weather_data <-filter(actual_weather_data, actual_weather_data$date>=start_date)
      
      if(!is.null(end_date))
        actual_weather_data <-filter(actual_weather_data, actual_weather_data$date<=end_date)
        
      if (is.null(weather_actual_end)){
        weather_actual_end<-actual_weather_data$date[nrow(actual_weather_data)]
      } else{
        actual_weather_data <-filter(actual_weather_data, actual_weather_data$date<=weather_actual_end)
      }
      
      actual_weather_data$source<-with(actual_weather_data,"actual")
    }
    if ("forecast" %in% sheets) {
      forecast_weather_data <-
        read_excel(str_weather_file, sheet = "forecast", n_max = maxRow)
      
      actual_weather_data <-filter(forecast_weather_data, forecast_weather_data$date>weather_actual_end)
      
      if(!is.null(start_date))
        actual_weather_data <-filter(forecast_weather_data, forecast_weather_data$date>=start_date)
      
      if(!is.null(end_date))
        actual_weather_data <-filter(forecast_weather_data, forecast_weather_data$date<=end_date)
      
      forecast_weather_data$source<-with(forecast_weather_data,"forecast")
    }
    
    # date selection
    # if (!is.null(start_date)) {
    #   if(exists("actual_weather_data"))actual_weather_data <- subset(actual_weather_data, date >= as.POSIXct(start_date))
    #   if(exists("forecast_weather_data"))forecast_weather_data <- subset(forecast_weather_data, date >= as.POSIXct(start_date))
    # }
    # if (!is.null(weather_actual_end)) {
    #   if(exists("actual_weather_data"))actual_weather_data <- subset(actual_weather_data, date <= as.POSIXct(weather_actual_end))
    #   if(exists("forecast_weather_data"))forecast_weather_data <- subset(forecast_weather_data, date > as.POSIXct(weather_actual_end))
    # }
    
    ##merge actual with forecast data
    if(exists("actual_weather_data") && exists("forecast_weather_data")){
      weather_data <-
        rbind.data.frame(actual_weather_data,forecast_weather_data)
    }else if(exists("actual_weather_data")){
        weather_data <- actual_weather_data
    }else if(exists("forecast_weather_data")){
        weather_data <- forecast_weather_data
    }
    
    #check if any missing day
    if(nrow(weather_data)<as.numeric(difftime(weather_data$date[nrow(weather_data)],weather_data$date[1],"days"))){
      warning("The weather file has missing date!!! Simulation will be wrong. Please check the file.")
    }
      
    return(weather_data)
  }

#' load a list of weather files
#' 
#' @param str_weather_files 
load.weather.data<-function(str_weather_files,...){
  
  #Preload all weather files
  list_weather_data<-list()
  for(i in 1:length(str_weather_files)){
    weather_data_df <-
      load.weather(str_weather_files[i],...)
    list_weather_data[[as.character(weather_data_df$site[1])]]<-weather_data_df
  }
  return(list_weather_data)
}
load.phenology.data<-function(str_measured_files){
  
  #Preload all measured data
  list_measured_data<-list()
  for(i in 1:length(str_measured_files)){
    measured_df <- read_excel(str_measured_files[i])
    #assume site specified as the same in whole column
    list_measured_data[[as.character(measured_df$site[1])]]<-measured_df
  }
  return(list_measured_data)
  
}

#' Extract 150 days weather data from weather file for each phenology observation season
#' Purpose: performance workaround for the slow filter and data.table subset behaviour
#'
#' @param stdPhenology[['site']](date,stage_ec,...)
#' @param stdWeather[['weather_station_id']]{site,date,...}
#'
#' @return No return
#' @export PE.phenoWeather[['weather_station_id']][['sown_date']] - Global object
#'
#' @examples
load.phenoWeather.data<-function(stdPhenology,stdWeather){
  #Preload all weather files
  PE.weatherDT<<-lapply(FUN=as.data.table,stdWeather) #performance handling
  PE.weatherDT<<-lapply(FUN=setindex,PE.weatherDT,"date")
  #Preload all measured data
  PE.phenologyDT<<-lapply(FUN=as.data.table,stdPhenology) #performance handling
  
  if(!exists("PE.pheoWeatherDT"))PE.pheoWeatherDT<<-list()
  list_sites<-names(PE.phenologyDT)
  for(i in 1:length(list_sites)){
    site<-as.character(list_sites[i])
    siteObsDT<-PE.phenologyDT[[site]]
    list_years<-unique(sapply(FUN=year,siteObsDT[,'date']))
    
    for(j in 1:length(list_years)){
      yr<-as.character(list_years[j])
      print_progress(paste("Start process site ",site," for year ",yr))
      
      #seasonObsDF<-filter(siteObsDF, year==list_years[[j]])
      seasonObsDT<-siteObsDT[year==list_years[[j]]]
      weather_station<-as.character(seasonObsDT$site[[1]])
      print_detail(paste("Weather station :",weather_station))
      
      sown_date<-as.Date(seasonObsDT$date[seasonObsDT$stage_ec==0])
      print_detail(paste("Sown date :",sown_date))
      if(length(sown_date)==0){
        print_critical(paste("skipped a dataset : sown date absence"))
        next
      }
      if(!weather_station %in% names(PE.pheoWeatherDT)) PE.pheoWeatherDT[[weather_station]]<-list()
      if(!sown_date %in% names(PE.pheoWeatherDT[[weather_station]])){
        PE.pheoWeatherDT[[weather_station]][[as.character(sown_date)]]<<-
          try(PE.weatherDT[[weather_station]][date >= as.POSIXct(sown_date) & date <= as.POSIXct(sown_date)+days(150)])
      }
    }
  }
}
writeResult<-function(str_outfile,result,parameters){
  write.xlsx(as.data.frame(parameters),file=str_outfile,append=TRUE,sheetName = "Parameters")
  write.xlsx(result,file=str_outfile,append=TRUE,sheetName = "Result")
}

#' Retrieve the trivial EC stage achieved by a simulated continuous EC value.
#'
#' @param stage_ec 
#'
#' @return
#' @export
#'
#' @examples
rounddown_ec<-function(stage_ec){
  #available_stages<-c(1,3,5,7,9,10:37,39,41,43,45,49,51,53,55,57,59,61,65,69,71,73,75,77,83,85,87,91:99)
  
  stage_ec<-floor(stage_ec)
  if(stage_ec<available_stages[1]){
    warning("input value too small, no EC stage matched|",stage_ec)
    return(stage_ec)
  }
  if(stage_ec %in% available_stages)return(stage_ec)
  
  high<-length(available_stages)
  low<-1
  while(high-low>1){
    mid<-round((high+low)/2)
    if(stage_ec>available_stages[mid]){
      low<-mid
    }
    if(stage_ec<available_stages[mid]){
      high<-mid
    }
  }
  return(as.numeric(available_stages[min(high,low)]))
}


#' Get the next trivial EC stage of the input value.
#'
#' @param stage_ec 
#'
#' @return
#' @export
#'
#' @examples
next_ec<-function(stage_ec){
  #available_stages<-c(1,3,5,7,9,10:37,39,41,43,45,49,51,53,55,57,59,61,65,69,71,73,75,77,83,85,87,91:99)
  stage_ec<-rounddown_ec(stage_ec)
  loc<-match(stage_ec,available_stages)
  if(length(available_stages)==loc){
    warning("Ceiling stage value reached, no next EC stage available",stage_ec)
    return(available_stages[loc])
  }
  return(available_stages[loc+1])
}


#' Find corresponding weather station code of the samplied site from Mapping file
#'
#' @param site 
#'
#' @return
#' @export
#'
#' @examples
lookup.weather_station<-function(site){
  if(!exists("LU.site_station_table"))LU.site_station_table<<-read_excel(str_mapping_table,sheet="Site_WeatherStation");
  site_station<-as.character(LU.site_station_table$weather_station[LU.site_station_table$site==site])
  if(length(site_station)==0){
    warning("No weather station mapped to the site in Mapping file",str_mapping_table)
    site_station<-site
    #remove(LU.site_station_table)
  }else if(length(site_station)>1){
    warning("More than one weather station mapped to the site in Mapping file",str_mapping_table)
    #remove(LU.site_station_table)
    site_station<-site
  }
  return(site_station)
}

print_debug<-function(...){
  if(print_level>=9){
    print(...)
  }
}
print_detail<-function(...){
  if(print_level>=5){
    print(...)
  }
}
print_progress<-function(...){
  if(print_level>=3){
    print(...)
  }
}
print_keypoint<-function(...){
  if(print_level>=1){
    print(...)
  }
}
print_critical<-function(...){
  print(...)
}