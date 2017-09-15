
cwm.eachYearParameter<-function(yearList,optInSites=NULL){
  PE.modelExecutionCount<<-0
  # avaDF=read_excel('./AvailablePhenologySeason.xlsx',sheet="Sheet1")
  # list_ava_years<-as.list(unique(avaDF[,'year']))$year
  # 
  # for(i in 1:length(list_ava_years)){
  #   year=list_ava_years[i]
  #   if(year %in% yearList){
  #     print(paste("Process year ",year))
  # cwm_par<-cwm.calibrate(str_param_file = "./Parameters/CWm_Parameters.xlsx",
  #                list_site_weather = stdWeatherDT, list_site_phenology = stdPhenologyDT, conf_id=10,
  #                start_year = year,end_year = year,optInSites = optInSites)
  # saveRDS(cwm_par,paste('./Parameters/cwmCalParHistYr',as.character(year),'.rds',sep = ""))
  #   }
  # }
  # "717","662","5404"
  #,"4933"
  optInSites=c(4466,4887,5404,15480)
  # optInSites=c(10153,
  #              10419,
  #              10688,
  #              10824,
  #              1691,
  #              2261,
  #              3023,
  #              3815,
  #              4933,
  #              5100,
  #              5250,
  #              5404,
  #              5440,
  #              5705,
  #              662,
  #              717,
  #              7784,
  #              8046,
  #              8416,
  #              8724)
  for(i in 1:length(optInSites)){
    site=as.character(optInSites[i])
    tryCatch({
      
      print(paste("Process site ",site))
      cwm_par<-cwm.calibrate(str_param_file = "./Parameters/CWm_Parameters.xlsx",
                             list_site_weather = stdWeatherDT, list_site_phenology = stdPhenologyDT, conf_id=10,
                             start_year = 1981,end_year = 9999,optInSites = c(site))
      saveRDS(cwm_par,paste('./Parameters/cwmCalParHistFr81Site',as.character(site),'.rds',sep = ""))
    }, error = function(e) {
      print(e)
    })
  }
}
we.eachYearParameter<-function(yearList,optInSites=NULL,optOutSites=NULL){
  PE.modelExecutionCount<<-0
  # avaDF=read_excel('./AvailablePhenologySeason.xlsx',sheet="Sheet1")
  # list_ava_years<-as.list(unique(avaDF[,'year']))$year
  # for(i in 1:length(list_ava_years)){
  #   year=list_ava_years[i]
  #   if(year %in% yearList){
  #     print(paste("Process year ",year))
  #     we_par<-we.calibrate(str_param_file = "./Parameters/Wang_Parameters.xlsx",
  #                                       list_site_weather = stdWeatherDT, list_site_phenology = stdPhenologyDT, conf_id=6,
  #                                       start_year = year,end_year = year,optOutSites = optOutSites,optInSites = optInSites)
  #     saveRDS(we_par,paste('./Parameters/weCalParHistYr',as.character(year),'.rds',sep = ""))
  #   }
  # }
  #"717","662","5404"
  #,"4933"
  # optInSites=c(10153,10419,10688,10824,1691,2261, 3023,3815,4933,5100,5250,5404,5440,5705,662,717,7784,8046,8416,8724)
  optInSites=c(4466,4887,5404,15480)
  for(i in 1:length(optInSites)){
    site=as.character(optInSites[i])
    tryCatch({
      print(paste("Process site ",site))
      we_par<-we.calibrate(str_param_file = "./Parameters/Wang_Parameters.xlsx",
                           list_site_weather = stdWeatherDT, list_site_phenology = stdPhenologyDT, conf_id=7,
                           start_year = 1981,end_year = 9999,optInSites = c(site))
      saveRDS(we_par,paste('./Parameters/wangCalParHistFr81Site',as.character(site),'.rds',sep = ""))
    }, error = function(e) {
      print(e)
    })
  }
}
#'put each par RDS file into a single data.table
#'rdsFileList<-list.files(pattern = "^cwmCalParHistFr81Site")
#'
data.consolidateYearlyPar<-function(rdsFileList){
  parDT=NULL
  for(i in 1:length(rdsFileList)){
    filename=rdsFileList[i]
    content<-readRDS(rdsFileList[i])
    par<-tail(content,1)
    par$fileyear<-filename
    if(is.null(parDT)){
      parDT<-par
    }else{
      parDT<-rbind(parDT,par)
    }
  }
  return(parDT)
}