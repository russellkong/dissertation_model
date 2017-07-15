
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
tool.add_photo_len<-function(str_weather_file,sheet=NULL){
  
  sheets <- excel_sheets(str_weather_file)
  if (!is.null(sheet) && sheet %in% sheets) {
    weather_data <- read_excel(str_weather_file, sheet = sheet)
  }else{
    weather_data <- read_excel(str_weather_file, sheet = 1)
  }
  
  #lookup lat of the weather station
  ws_loc <- read_excel(str_mapping_table,sheet="WeatherStation_Location");
  lat<-ws_loc$lac[ws_loc$weather_station==weather_data$site[1]]
  
  if(length(lat)==0)stop("Site's location not found in Mapping table",str_mapping_table)
  
  weather_data$photo_len <- daylength(lat,as.character(weather_data$date))
  write.xlsx(weather_data,file=str_weather_file,append=TRUE,sheetName = "OUTPUT")
}
