### main.R ###
## Author: Russell Kong
## Date: 16 JUN 2017
## version: 0

install_pkg<-function(){
  options(repos="http://www.stats.bris.ac.uk/R/")
  #high performance excel reader
  install.packages("readxl")
  #excel writer
  install.packages("xlsx")
  #data maniplation
  install.packages("dplyr")
  #Graph
  install.packages("ggplot2")
  #
  install.packages("geosphere")
  install.packages("doParallel")
  install.packages("foreach")
  install.packages("lubridate")
  install.packages("data.table")
  install.packages("doSNOW")
}
set_parallel_env<-function(){
  library(doParallel)
  library(doSNOW)
  library(foreach)
  
  cl<-makeCluster(detectCores(),outfile="./parConsoleOutput.txt") #change the 2 to your number of CPU cores
  
  registerDoSNOW(cl)
}
end_parallel_env<-function(){
  stopCluster(cl)
}
load.debugsource<-function(){
  debugSource("~/dissertation_model/class.R")
  debugSource("~/dissertation_model/common.R")
  debugSource("~/dissertation_model/wang.R")
  debugSource("~/dissertation_model/cwm.R")
  debugSource("~/dissertation_model/analysis.R")
  debugSource("~/dissertation_model/plot.R")
  debugSource("~/dissertation_model/tools.R")
  debugSource("~/dissertation_model/parameter_estimation.R")
}
init_env<-function(){
  library(readxl)
  library(xlsx)
  library(dplyr)
  library(tidyr)
  library(data.table)
  
  setwd('~/dissertation_model')
  source("class.R")
  source("common.R")
  source("wang.R")
  source("cwm.R")
  source("analysis.R")
  source("plot.R")
  source("tools.R")
  source("parameter_estimation.R")
  
  #common.R:lookup.weather_station
  #tools.R:tool.add_photo_len
  str_mapping_table<<-"./Parameters/Mapping.xlsx"
  
  #common.R:rounddown_ec,next_ec
  available_stages<<-c(0,1,3,5,7,9,10:37,39,41,43,45,49,51,53,55,57,59,61,65,69,71,73,75,77,83,85,87,91:99)
  
  #varbose output level 1(keypoint),3(progress),5(detail),9(debug)
  print_level<<-3
  
  setwd('~/dissertation_data')
  
  stdWeatherDT<<-readRDS("./Weather/formatedSiteWeather.rds")
  stdPhenoWeatherDT<<-readRDS("./Weather/filteredPhenoWeather.rds")
  stdPhenologyDT<<-readRDS("./Phenology/formatedPhenology.rds")
}

compile_fun<-function(){
  library(compiler)
  enableJIT(2)
}
clear_env<-function(){
  #common.R:lookup.weather_station
  remove(LU.site_station_table)
  
  #parameter_estimation.R: we.rmse
  remove(PE.resultObj,pos = ".GlobalEnv" )
  remove(PE.iteration,pos = ".GlobalEnv")
}

cal.parameter<-function(){
  result_b4=cwm.main("./Weather/W_100EA002_2016.xlsx","./Parameters/CWm_Parameters.xlsx",conf_id = 1, sown_date = "2016-04-17")
  result_opm=cwm.main("./Weather/W_100EA002_2016.xlsx","./Parameters/CWm_Parameters.xlsx",conf_id = 15, sown_date = "2016-04-17")
  obsDF<- read_excel("./Phenology/P_WindsorWest_2016.xlsx")
  plot.EC(we_df = result_b4,cwm_df = result_opm)
  plot.Sim_Obs(simDFs = c(result_b4,result_opm),simDFs_leg = c("Before","After"),obsDF)
  analysis.gs.err.rmse(obsDF = obsDF,simDF = result_b4)
  analysis.obsDay.err.rmse(obsDF = obsDF,simDF = result_opm)
}

load_prof <-function(str){
  library(profvis)
  profvis({eval(parse(text=str))})
}
