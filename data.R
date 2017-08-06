load.raw_data<-function(){
  soiltempMeta<-readRDS("./Malte/soilTempMeta.rds")
  weatherMeta<-readRDS("./Malte/weatherDataMeta.rds")
  phenologyData<-readRDS("./Malte/pheFilteredListReduced.rds")
  soiltempData<-readRDS("./Weather/raw/soilTempData.rds")
  weatherData<-readRDS(("./Weather/raw/weatherDatareduced.rds"))
  # filter phenology data [only spring barley]
  sbPheData<-phenologyData[['207']][['annual']]
  stdWeatherDF<-data.weather.germany(weatherData)
  stdPhenologyDF<-data.phenology.germany(phenologyData)
}
data.weather.germany<-function(weatherData){
  return(lapply(FUN=data.weather.preprocess.germany,weatherData))
}
data.weather.preprocess.germany<-function(siteWeatherData){
  library(dplyr)
  weatherDF<-select(siteWeatherData$data,STATIONS_ID,MESS_DATUM,SDK,TMK,TXK,TNK)
  colnames(weatherDF)<-c("site","date","photo_len","temp_avg","temp_max","temp_min")
  if(any(is.na(weatherDF[,'photo_len']))){
    weatherDF<-data.add_photo_len(weatherDF,lat=siteWeatherData[['loc']]['LAT'])
    weatherDF$source<-with(siteWeatherData,"actual_temp;cal_daylength")
  }else{
    weatherDF$source<-with(siteWeatherData,"actual_temp;actual_light")
  }
  return(weatherDF)
}
data.phenology.germany<-function(phenologyData){
  return(lapply(FUN=data.data.phenology.preprocess.germany,phenologyData$`207`$annual))
}
data.data.phenology.preprocess.germany<-function(sitePhenologyData){
  phenologyDF<-select(sitePhenologyData$data,WTH_id,HRVYR,DATE,BBCH)
  phenologyDF<- phenologyDF %>% group_by(HRVYR) %>% mutate(day=as.numeric(difftime(DATE,min(DATE),units="days")))
  phenologyDF<-ungroup(phenologyDF)
  colnames(phenologyDF)<-c("site","year","date","stage_ec","day")
  #"data"      "daterange" "name"      "loc"       "msl"       "weather"   "soilTemp" 
  #
  #DATE Phase_id BBCH HRVYR  Periode Qualitaetsniveau Eintrittsdatum_QB WTH_id SLT_id
  return(phenologyDF)
}