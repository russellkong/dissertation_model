setClass(
  "Weather",
  representation(
    date = "POSIXct",
    temp_avg = "numeric",
    temp_min = "numeric",
    temp_max = "numeric",
    photo_len = "numeric",
    source = "character"
  )
)

setMethod("as.data.frame", "Weather",
          function(x,
                   row.names = NULL,
                   optional = FALSE,
                   ...) {
            slotnames <- slotNames(x)
            slotlist <-
              data.frame(rbind(replicate(length(slotnames), 0)))
            names(slotlist) <- slotnames
            for (i in slotnames) {
              if (length(slot(x,  i)) > 0) {
                if (is(slot(x,  i), "POSIXct")) {
                  slotlist[1, i] <-
                    as.character(as.POSIXct(slot(x,  i), origin = "1970-01-01"))
                }
                else {
                  slotlist[1, i] <- slot(x,  i)
                }
              }
            }
            return(slotlist)
          })
setClass(
  "Prediction",
  representation(
    day = "numeric",
    date = "POSIXct",
    stage_dev = "numeric",
    stage_ec = "numeric",
    weather_source = "character"
  )
)

#' Title
#'
#' @param Prediction
#'
#' @return
#' @export
#'
#' @examples
setMethod("as.data.frame", "Prediction",
          function(x,
                   row.names = NULL,
                   optional = FALSE,
                   ...) {
            slotnames <- slotNames(x)
            slotlist <-
              data.frame(rbind(replicate(length(slotnames), 0)))
            names(slotlist) <- slotnames
            for (i in slotnames) {
              if (length(slot(x,  i)) > 0) {
                if (is(slot(x,  i), "POSIXct")) {
                  slotlist[1, i] <-
                    as.character(as.POSIXct(slot(x,  i), origin = "1970-01-01"))
                }
                else {
                  slotlist[1, i] <- slot(x,  i)
                }
              }
            }
            return(slotlist)
          })


#' Title
#'
#' @slot id numeric. 
#'
#' @return
#' @export
#'
#' @examples
setClass("ParameterSet",
         representation(conf_id = "numeric", model="character", weather_file="character"))

setMethod("as.data.frame", "ParameterSet",
          function(x,
                   row.names = NULL,
                   optional = FALSE,
                   ...) {
            slotnames <- slotNames(x)
            slotlist <-
              data.frame(rbind(replicate(length(slotnames), 0)))
            names(slotlist) <- slotnames
            for (i in slotnames) {
              if (length(slot(x,  i)) > 0) {
                if (is(slot(x,  i), "POSIXct")) {
                  slotlist[1, i] <-
                    as.character(as.POSIXct(slot(x,  i), origin = "1970-01-01"))
                }else if (is(slot(x,  i), "data.frame")) {
                  slotlist[1, i] <- paste(as.character(slot(x,  i)),collapse = "|")
                } else {
                  slotlist[1, i] <- slot(x,  i)
                }
              }
            }
            return(slotlist)
          })



#' Title
#'
#' @param weatherRow
#' @param dailyWeather
#'
#' @return
#' @export
#'
#' @examples
set_weather <- function(weatherRow, dailyWeather) {
  #dailyWeather@date<-as.character(weatherRow$date)
  dailyWeather@date <-
    as.POSIXct(weatherRow$date, format = "%m/%d/%Y %H:%M")
  dailyWeather@temp_avg <- as.numeric(weatherRow$temp_avg)
  dailyWeather@temp_min <- as.numeric(weatherRow$temp_min)
  dailyWeather@temp_max <- as.numeric(weatherRow$temp_max)
  dailyWeather@photo_len <- as.numeric(weatherRow$photo_len)
  dailyWeather@source <- as.character(weatherRow$source)
  return(dailyWeather)
}

#' Title
#'
#' @param str_weather_file
#' @param enddate_actual
#' @param endRow created to reduce data imput to reasonable amount on excel sheet too big with null rows
#' @return
#' @export
#'
#' @examples
load_weather <-
  function(str_weather_file,
           start_date = NULL,
           weather_actual_end = NULL,
           endRow = 1000) {
    sheets <- excel_sheets(str_weather_file)
    if (!"actual" %in% sheets && !"forecast" %in% sheets) {
      stop("No weather sheet 'actual' or 'forecast' in the file:",str_weather_file)
    }
    
    if ("actual" %in% sheets) {
      actual_weather_data <-
        read_excel(str_weather_file, sheet = "actual", n_max = endRow)
      actual_weather_data$source<-with(actual_weather_data,"actual")
      if (is.null(weather_actual_end)) weather_actual_end<-as.character(actual_weather_data[[nrow(actual_weather_data),"date"]])
    }
    if ("forecast" %in% sheets) {
      forecast_weather_data <-
        read_excel(str_weather_file, sheet = "forecast", n_max = endRow)
      forecast_weather_data$source<-with(forecast_weather_data,"forecast")
    }
    
    # date selection
    if (!is.null(start_date)) {
      if(exists("actual_weather_data"))actual_weather_data <- subset(actual_weather_data, date >= as.POSIXct(start_date))
      if(exists("forecast_weather_data"))forecast_weather_data <- subset(forecast_weather_data, date >= as.POSIXct(start_date))
    }
    if (!is.null(weather_actual_end)) {
      if(exists("actual_weather_data"))actual_weather_data <- subset(actual_weather_data, date <= as.POSIXct(weather_actual_end))
      if(exists("forecast_weather_data"))forecast_weather_data <- subset(forecast_weather_data, date > as.POSIXct(weather_actual_end))
    }
    
    ##merge actual with forecast data
    if(exists("actual_weather_data") && exists("forecast_weather_data")){
      weather_data <-
        rbind.data.frame(actual_weather_data,forecast_weather_data)
    }else if(exists("actual_weather_data")){
        weather_data <- actual_weather_data
    }else if(exists("forecast_weather_data")){
        weather_data <- forecast_weather_data
    }
    
    return(weather_data)
  }

writeResult<-function(str_outfile,result,parameters){
  write.xlsx(as.data.frame(parameters),file=str_outfile,append=TRUE,sheetName = "Parameters")
  write.xlsx(result,file=str_outfile,append=TRUE,sheetName = "Result")
}

rounddown_ec<-function(stage_ec){
  available_stages<-c(1,3,5,7,9,10:37,39,41,43,45,49,51,53,55,57,59,61,65,69,71,73,75,77,83,85,87,91:99)
  
  stage_ec<-floor(stage_ec)
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
  return(available_stages[min(high,low)])
}

next_ec<-function(stage_ec){
  available_stages<-c(1,3,5,7,9,10:37,39,41,43,45,49,51,53,55,57,59,61,65,69,71,73,75,77,83,85,87,91:99)
  stage_ec<-floor(stage_ec)
  loc<-match(stage_ec,available_stages)
  return(available_stages[loc+1])
}