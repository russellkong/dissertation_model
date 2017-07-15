### main.R ###
## Author: Russell Kong
## Date: 16 JUN 2017
## version: 0

install.packages("readxl")
install.packages("xlsx")
install.packages("dplyr")
install.packages("ggplot2")

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

#'lookup.weather_station
site_station_table<-read_excel("./Parameters/Site_WeatherStation.xlsx");

#'rounddown_ec
#'next_ec
available_stages<-c(1,3,5,7,9,10:37,39,41,43,45,49,51,53,55,57,59,61,65,69,71,73,75,77,83,85,87,91:99)
