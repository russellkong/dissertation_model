
library(xlsx)
library(readxl)
library(geosphere)

#' Function to add photo_len column to weather data in excel file
#' "OUTPUT" sheet created as a copy of orignal weather file with the new col
#' The latitude of the site must be specified in Mapping.xlsx's WeatherStation_Location sheet
#' set options(java.parameters = "-Xmx1000m") on outOfMemory
#' @param str_weather_file 
#' @param sheet 
#'
#' @return
#' @export
#'
#' @examples
data.add_photo_len.xlsx<-function(str_weather_file,sheet=NULL){

  sheets <- excel_sheets(str_weather_file)
  if (!is.null(sheet) && sheet %in% sheets) {
    weather_data <- read_excel(str_weather_file, sheet = sheet)
  }else{
    weather_data <- read_excel(str_weather_file, sheet = 1)
  }
  weather_data<-data.add_photo_len(weather_data)
  
  write.xlsx(weather_data,file=str_weather_file,sheetName = "OUTPUT",append = TRUE)
}
data.add_photo_len<-function(weatherDF, lat=NULL){

  #lookup lat of the weather station
  if(is.null(lat)){
    ws_loc <- read_excel(str_mapping_table,sheet="WeatherStation_Location");
    lat<-ws_loc$lat[ws_loc$weather_station==weatherDF$site[1]]
  }
  
  if(length(lat)==0)stop("Site's location not found in Mapping table",str_mapping_table)
  
  weatherDF$photo_len <- daylength(lat,as.character(weatherDF$date))
  return(weatherDF)
}
#'
#'Fill up weather dataset with missing data by replacement data
#'10day forecast < 3day forecast <- actual
#'
#' @param problemDF 
#' @param replacementDF 
#' @example nafferton2016<-load.weather("./Weather/W_nafferton_2015_2016.xlsx","2016-01-01")
#' @example S100EA002_2016<-load.weather("./Weather/W_100EA002_2016.xlsx","2016-01-01")
#' @example nafferton2016<-data.fill_missing_date(nafferton2016,S100EA002_2016)
data.fill_missing_date <- function(problemDF,replacementDF){
  startDate<-problemDF$date[1]
  endDate<-problemDF$date[nrow(problemDF)]
  #check if any missing day
  if(nrow(problemDF)<as.numeric(difftime(endDate,startDate,"days"))){
    dateList<-data.frame("date"=seq.POSIXt(startDate, endDate, by="days"))
    problemDF<-merge(dateList,problemDF,by="date",all = TRUE)
    for(i in 1:nrow(problemDF)){
      if(is.na(problemDF$temp_avg[i])){
        curDate<-problemDF$date[i]
        problemDF[i,]<-filter(replacementDF,date==curDate)[1,names(problemDF)]
      }
    }
  }
  return(problemDF)
}

tool.insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}