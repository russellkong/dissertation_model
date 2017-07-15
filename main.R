### main.R ###
## Author: Russell Kong
## Date: 16 JUN 2017
## version: 0
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

library(readxl)
library(xlsx)
library(dplyr)
library(ggplot2)

source("class.R")
source("common.R")
source("wang.R")
source("cwm.R")
source("analysis.R")
source("parameter_estimation.R")

init_env<-function(){
  #common.R:lookup.weather_station
  #tools.R:tool.add_photo_len
  str_mapping_table<<-"./Parameters/Mapping.xlsx"
  
  #common.R:rounddown_ec,next_ec
  available_stages<<-c(1,3,5,7,9,10:37,39,41,43,45,49,51,53,55,57,59,61,65,69,71,73,75,77,83,85,87,91:99)
  
  #varbose output level 1(keypoint),3(progress),5(detail),9(debug)
  print_level<<-9
}

clearAll<-function(){
  #common.R:lookup.weather_station
  remove(LU.site_station_table)
  
  #parameter_estimation.R: wang.rmse
  remove(PE.resultObj)
  remove(PE.iteration)
}