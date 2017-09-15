#' Data loading
#' weather_cockle_for_2016
#' weather_cockle_for_2017

w_fore=list()
w_fore[['coc_2016']]=load.weather("./Weather/W_CocklePark_forecast_2016.xlsx",mode = "forecast")
w_fore[['coc_2017']]=load.weather("./Weather/W_CocklePark_forecast_2017.xlsx",mode = "forecast")
w_fore[['naf_2016']]=load.weather("./Weather/W_nafferton_forecast_2016.xlsx",mode = "forecast")
w_fore[['naf_2017']]=load.weather("./Weather/W_nafferton_forecast_2017.xlsx",mode = "forecast")

#' weather_cockle_act_2016
#' weather_cockle_act_2017
#' weather_windsor_act_2016
#' weather_nafferton_act_2016
#' weather_nafferton_act_2017 

w_act=list()
w_act[['coc_2016']]=load.weather("./Weather/W_CocklePark_actual_2016.xlsx")
w_act[['naf_2016']]=load.weather("./Weather/W_nafferton_actual_2015_2016.xlsx",start_date = "2016-01-01")
w_act[['naf_2017']]=load.weather("./Weather/W_nafferton_actual_2017.xlsx")

w_act[['win_2016']]=load.weather("./Weather/W_100EA002_2016.xlsx")

w_act[['naf_2004']]=load.weather("./Weather/W_nafferton_actual_2004.xlsx")
w_act[['naf_2007']]=load.weather("./Weather/W_nafferton_actual_2007.xlsx")
w_act[['naf_2008']]=load.weather("./Weather/W_nafferton_actual_2008.xlsx")
#' Observation loading
#' phen_windsor_2016
#' phen_nafferton_2017 (TBC)
o_win_2016=read_excel("./Phenology/P_WindsorWest_2016.xlsx")
#o_naf_2017=read_excel("./Phenology/P_nafferton_2017.xlsx")
o_naf<-list()
o_naf_yrs<-c(2004,2007,2008,2017)
for(i in 1:length(o_naf_yrs)){
  yr=as.character(o_naf_yrs[i])
  o_naf[[yr]]=read_excel("./Phenology/P_nafferton.xlsx",sheet = yr)
}

#' Result Data Creation
#' 
#' To each model
#' 1. apply 2016 & 2017 forecast from cockle park farm (germany parameters >2000)
#' sim_{model}_{Site}_{weather_Type}_{stage}_{weather_type}_{stage}_{period}
#' sim_cwm_Cockle_For_99_2016
#' sim_cwm_Cockle_For_99_2017
#' sim_we_Cockle_For_99_2016
#' sim_we_Cockle_For_99_2017
#' 
sim_fore_99=list()
sim_fore_99[['cwm_coc_2016']]=cwm.main(weather_forecast_dt = w_fore[['coc_2016']],
                                 str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
                                 sown_date = "2016-04-17", end_stage = 92)
sim_fore_99[['cwm_coc_2017']]=cwm.main(weather_forecast_dt = w_fore[['coc_2017']],
                                 str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
                                 sown_date = "2017-04-25", end_stage = 92)
sim_fore_99[['cwm_naf_2016']]=cwm.main(weather_forecast_dt = w_fore[['naf_2016']],
                                 str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
                                 sown_date = "2016-04-17", end_stage = 92)
sim_fore_99[['cwm_naf_2017']]=cwm.main(weather_forecast_dt = w_fore[['naf_2017']],
                                 str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
                                 sown_date = "2017-04-25", end_stage = 92)

sim_fore_99[['we_coc_2016']]=we.main(weather_forecast_dt = w_fore[['coc_2016']],
                                 str_param_file = "./Parameters/WE_Parameters.xlsx",conf_id = 7,
                                 sown_date = "2016-04-17", end_stage = 92)
sim_fore_99[['we_coc_2017']]=we.main(weather_forecast_dt = w_fore[['coc_2017']],
                                 str_param_file = "./Parameters/WE_Parameters.xlsx",conf_id = 7,
                                 sown_date = "2017-04-25", end_stage = 92)
sim_fore_99[['we_naf_2016']]=we.main(weather_forecast_dt = w_fore[['naf_2016']],
                                 str_param_file = "./Parameters/WE_Parameters.xlsx",conf_id = 7,
                                 sown_date = "2016-04-17", end_stage = 92)
sim_fore_99[['we_naf_2017']]=we.main(weather_forecast_dt = w_fore[['naf_2017']],
                                 str_param_file = "./Parameters/WE_Parameters.xlsx",conf_id = 7,
                                 sown_date = "2017-04-25", end_stage = 92)
for(i in 1:length(names(sim_fore_99))){
  sheet=names(sim_fore_99)[i]
  write.xlsx(sim_fore_99[[sheet]],sheetName = sheet,file="./sim_fore_99.xlsx", append = T)
}
#' 2.apply 2016 & 2017 actual from cockle park farm (germany parameters >2000)
#' sim_cwm_Cockle_Act_99_2016
#' sim_cwm_Cockle_Act_99_2017
#' sim_we_Cockle_Act_99_2016
#' sim_we_Cockle_Act_99_2017
sim_act_99=list()
sim_act_99[['cwm_naf_2004']]=cwm.main(weather_data_dt = w_act[['naf_2004']],
                                     str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
                                     sown_date = "2004-04-07", end_stage = 92)
sim_act_99[['cwm_naf_2007']]=cwm.main(weather_data_dt = w_act[['naf_2007']],
                                     str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
                                     sown_date = "2007-04-11", end_stage = 92)
sim_act_99[['cwm_naf_2008']]=cwm.main(weather_data_dt = w_act[['naf_2008']],
                                     str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
                                     sown_date = "2008-04-24", end_stage = 92)

sim_act_99[['cwm_naf_2008']]=cwm.main(weather_data_dt = w_act[['naf_2008']],
                                      str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
                                      sown_date = "2008-04-24", end_stage = 92)

sim_act_99[['cwm_win_2016']]=cwm.main(weather_data_dt = w_act[['win_2016']],
                                  str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
                                  sown_date = "2016-04-17", end_stage = 92)
# sim_act_99[[cwm_coc_2017]]=cwm.main(weather_actcast_dt = w_coc_act_2017,
#                                   str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
#                                   sown_date = "2016-04-25")
sim_act_99[['cwm_naf_2016']]=cwm.main(weather_data_dt = w_act[['naf_2016']],
                                  str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
                                  sown_date = "2016-04-17", end_stage = 92)
sim_act_99[['cwm_naf_2017']]=cwm.main(weather_data_dt = w_act[['naf_2017']],
                                  str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
                                  sown_date = "2017-04-25", end_stage = 92)

sim_act_99[['we_naf_2004']]=we.main(weather_data_dt = w_act[['naf_2004']],
                                      str_param_file = "./Parameters/WE_Parameters.xlsx",conf_id = 7,
                                      sown_date = "2004-04-07", end_stage = 92)
sim_act_99[['we_naf_2007']]=we.main(weather_data_dt = w_act[['naf_2007']],
                                      str_param_file = "./Parameters/WE_Parameters.xlsx",conf_id = 7,
                                      sown_date = "2007-04-11", end_stage = 92)
sim_act_99[['we_naf_2008']]=we.main(weather_data_dt = w_act[['naf_2008']],
                                      str_param_file = "./Parameters/WE_Parameters.xlsx",conf_id = 7,
                                      sown_date = "2008-04-24", end_stage = 92)
sim_act_99[['we_win_2016']]=we.main(weather_data_dt = w_act[['win_2016']],
                                    str_param_file = "./Parameters/WE_Parameters.xlsx",conf_id = 7,
                                    sown_date = "2016-04-17", end_stage = 92)

sim_act_99[['we_coc_2016']]=we.main(weather_data_dt = w_act[['coc_2016']],
                               str_param_file = "./Parameters/WE_Parameters.xlsx",conf_id = 7,
                               sown_date = "2016-04-17", end_stage = 92)
# sim_act_99[['we_coc_2017']]=we.main(weather_forecast_dt = w_coc_act_2017,
#                                str_param_file = "./Parameters/WE_Parameters.xlsx",conf_id = 7,
#                                sown_date = "2016-04-25")
sim_act_99[['we_naf_2016']]=we.main(weather_data_dt = w_act[['naf_2016']],
                               str_param_file = "./Parameters/WE_Parameters.xlsx",conf_id = 7,
                               sown_date = "2016-04-17", end_stage = 92)
sim_act_99[['we_naf_2017']]=we.main(weather_data_dt = w_act[['naf_2017']],
                               str_param_file = "./Parameters/WE_Parameters.xlsx",conf_id = 7,
                               sown_date = "2017-04-25", end_stage = 92)
for(i in 1:length(names(sim_fore_99))){
  sheet=names(sim_act_99)[i]
  write.xlsx(sim_act_99[[sheet]],sheetName = sheet,file="./sim_act_99.xlsx", append = T)
}

#' 3. apply actual to key stage and follow by forecast (key stages: 21,31,39,59,71,87)
#' sim_cwm_Cockle_Act_21_For_99_2016
#' sim_cwm_Cockle_Act_21_For_99_2017
#' sim_we_Cockle_Act_21_For_99_2016
#' sim_we_Cockle_Act_21_For_99_2017
#' ....
sim_cwm_coc_phased_2016=list()
sim_cwm_naf_phased_2016=list()
sim_cwm_naf_phased_2017=list()
sim_we_coc_phased_2016=list()
sim_we_naf_phased_2016=list()
sim_we_naf_phased_2017=list()
stages=c(21,31,39,59,71)
for(i in 1:length(stages)){
  stage=stages[i]
  parse(text=paste("sim_cwm_coc_act_",stage,"_2016",sep=""))
  sim_cwm_coc_phased_2016[[as.character(stage)]]=cwm.main(weather_data_dt = w_act[['coc_2016']],weather_forecast_dt = w_fore[['coc_2016']],
                                   str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
                                   sown_date = "2016-04-17",weather_actual_end_stage = stage)
  # sim_act_99[[cwm_coc_2017]]=cwm.main(weather_actcast_dt = w_coc_act_2017,
  #                                   str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
  #                                   sown_date = "2016-04-25")
  sim_cwm_naf_phased_2016[[as.character(stage)]]=cwm.main(weather_data_dt = w_act[['naf_2016']],weather_forecast_dt = w_fore[['naf_2016']],
                                   str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
                                   sown_date = "2016-04-17",weather_actual_end_stage = stage)
  sim_cwm_naf_phased_2017[[as.character(stage)]]=cwm.main(weather_data_dt = w_act[['naf_2017']],weather_forecast_dt = w_fore[['naf_2017']],
                                   str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
                                   sown_date = "2017-04-25",weather_actual_end_stage = stage)
  
  sim_we_coc_phased_2016[[as.character(stage)]]=we.main(weather_data_dt = w_act[['coc_2016']],weather_forecast_dt = w_fore[['coc_2016']],
                                                        str_param_file = "./Parameters/WE_Parameters.xlsx",conf_id = 7,
                                                        sown_date = "2016-04-17",weather_actual_end_stage = stage)
  # sim_act_99[[cwm_coc_2017]]=cwm.main(weather_actcast_dt = w_coc_act_2017,
  #                                   str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
  #                                   sown_date = "2016-04-25")
  sim_we_naf_phased_2016[[as.character(stage)]]=we.main(weather_data_dt = w_act[['naf_2016']],weather_forecast_dt = w_fore[['naf_2016']],
                                                        str_param_file = "./Parameters/WE_Parameters.xlsx",conf_id = 7,
                                                        sown_date = "2016-04-17",weather_actual_end_stage = stage)
  sim_we_naf_phased_2017[[as.character(stage)]]=we.main(weather_data_dt = w_act[['naf_2017']],weather_forecast_dt = w_fore[['naf_2017']],
                                                        str_param_file = "./Parameters/WE_Parameters.xlsx",conf_id = 7,
                                                        sown_date = "2017-04-25",weather_actual_end_stage = stage)
}
save(sim_cwm_coc_phased_2016,file = "./sim_cwm_coc_phased_2016")
save(sim_cwm_naf_phased_2016,file = "./sim_cwm_naf_phased_2016")
save(sim_cwm_naf_phased_2017,file = "./sim_cwm_naf_phased_2017")
save(sim_we_coc_phased_2016,file = "./sim_we_coc_phased_2016")
save(sim_we_naf_phased_2016,file = "./sim_we_naf_phased_2016")
save(sim_we_naf_phased_2017,file = "./sim_we_naf_phased_2017")

# daily simulation with forecast to 9 days
testsets=c('cwm_coc_2016','cwm_naf_2016','cwm_naf_2017','we_coc_2016','we_naf_2016','we_naf_2017')
w_sets=c('coc_2016','naf_2016','naf_2017','coc_2016','naf_2016','naf_2017')
sim_9days=list()
sim_9days[['cwm_coc_2016']]=cwm.main(weather_data_dt = w_act[['coc_2016']],weather_forecast_dt = w_fore[['coc_2016']],
                                                        str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
                                                        sown_date = "2016-04-17",mode="ProjectForecast", end_stage = 92)


# sim_act_99[[cwm_coc_2017]]=cwm.main(weather_actcast_dt = w_coc_act_2017,
#                                   str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
#                                   sown_date = "2016-04-25")
sim_9days[['cwm_naf_2016']]=cwm.main(weather_data_dt = w_act[['naf_2016']],weather_forecast_dt = w_fore[['naf_2016']],
                                                        str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
                                                        sown_date = "2016-04-17",mode="ProjectForecast", end_stage = 92)

sim_9days[['cwm_naf_2017']]=cwm.main(weather_data_dt = w_act[['naf_2017']],weather_forecast_dt = w_fore[['naf_2017']],
                                                        str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
                                                        sown_date = "2017-04-25",mode="ProjectForecast", end_stage = 92)

sim_9days[['we_coc_2016']]=we.main(weather_data_dt = w_act[['coc_2016']],weather_forecast_dt = w_fore[['coc_2016']],
                                                      str_param_file = "./Parameters/WE_Parameters.xlsx",conf_id = 7,
                                                      sown_date = "2016-04-17",mode="ProjectForecast", end_stage = 92)

# sim_act_99[[cwm_coc_2017]]=cwm.main(weather_actcast_dt = w_coc_act_2017,
#                                   str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
#                                   sown_date = "2016-04-25")
sim_9days[['we_naf_2016']]=we.main(weather_data_dt = w_act[['naf_2016']],weather_forecast_dt = w_fore[['naf_2016']],
                                                      str_param_file = "./Parameters/WE_Parameters.xlsx",conf_id = 7,
                                                      sown_date = "2016-04-17",mode="ProjectForecast", end_stage = 92)

sim_9days[['we_naf_2017']]=we.main(weather_data_dt = w_act[['naf_2017']],weather_forecast_dt = w_fore[['naf_2017']],
                                                      str_param_file = "./Parameters/WE_Parameters.xlsx",conf_id = 7,
                                                      sown_date = "2017-04-25",mode="ProjectForecast", end_stage = 92)



#' 
#' =============================================================================================================================================================
#' Processing
#' =============================================================================================================================================================
analysis.obsDay.err(obsDF = o_naf[['2004']],simDFs = list(sim_act_99[['we_naf_2004']],sim_act_99[['cwm_naf_2004']]))
                    
#' =============================================================================================================================================================
#' Temperature
#' Diff between fore & act weather
#' diff_weather_for_act_cockle_{year} = weather_for | weather_act
#' 
diff_weather_for_act_coc_2016 = analysis.temp.err(forecastDF = w_fore[['coc_2016']], actualDF = w_act[['coc_2016']])
analysis.temp.err.plot(diff_weather_for_act_coc_2016,title="Cockle Park Farm 2016")

diff_weather_for_act_naf_2016 = analysis.temp.err(forecastDF = w_fore[['naf_2016']], actualDF = w_act[['naf_2016']],start_date = "2016-04-15",end_date = "2016-10-18")
analysis.temp.err.plot(diff_weather_for_act_naf_2016,title="Nafferton Farm 2016")

diff_weather_for_act_naf_2017 = analysis.temp.err(forecastDF = w_fore[['naf_2017']], actualDF = w_act[['naf_2017']],start_date = "2017-04-25",end_date = "2017-08-10")
analysis.temp.err.plot(diff_weather_for_act_naf_2017,title="Nafferton Farm 2017")
max(diff_weather_for_act_coc_2016$err)
max(diff_weather_for_act_naf_2016$err)
max(diff_weather_for_act_naf_2017$err)
min(diff_weather_for_act_coc_2016$err)
min(diff_weather_for_act_naf_2016$err)
min(diff_weather_for_act_naf_2017$err)
lm_result=list()
res_result=list()
lm_result[['coc_2016']]=lm(formula = temp_avg_fore ~ temp_avg_act, data = diff_weather_for_act_coc_2016)
res_result[['coc_2016']]=resid(lm_result[['coc_2016']])
write.xlsx(diff_weather_for_act_coc_2016,file = './weather_diff.xlsx',sheetName = 'diff_weather_for_act_coc_2016',append = TRUE)
write.xlsx(diff_weather_for_act_naf_2016,file = './weather_diff.xlsx',sheetName = 'diff_weather_for_act_naf_2016',append = TRUE)
write.xlsx(diff_weather_for_act_naf_2017,file = './weather_diff.xlsx',sheetName = 'diff_weather_for_act_naf_2017',append = TRUE)
#' =============================================================================================================================================================
#' Diff sim result by model between fore & act weather
#' diff_sim_{model}_cockle_{year} = cwm
#' 
#' Diff sim result by actual between model
#' 
#' Diff sim result by forecast between model
#' 
#' diff mix of act & forecast
diff_sim_fore_act=list()
diff_sim_fore_act[['cwm_coc_2016']]=analysis.model.sim_diff(baseDF = sim_act_99[['cwm_coc_2016']],
                                                           simDFs = list(sim_fore_99[['cwm_coc_2016']]),
                                                           simlabels = list('sim'))
diff_sim_fore_act[['we_coc_2016']]=analysis.model.sim_diff(baseDF = sim_act_99[['we_coc_2016']],
                                                          simDFs = list(sim_fore_99[['we_coc_2016']]),
                                                          simlabels = list('sim'))
plot.Sim_Obs.date(simDFs = list(sim_act_99[['cwm_coc_2016']],sim_fore_99[['cwm_coc_2016']],sim_act_99[['we_coc_2016']],sim_fore_99[['we_coc_2016']]),
             simDFs_leg = list('Act_CWm','Fore_CWm','Act_WE','Fore_WE'),title="2016 Cockle Park")
plot.Sim_Obs.stageFirstDay.plot(diff_sim_fore_act[['cwm_coc_2016']],title="2016 CWm Cockle Park")
plot.Sim_Obs.stageFirstDay.plot(diff_sim_fore_act[['we_coc_2016']],title="2016 WE Cockle Park")

diff_sim_fore_act[['cwm_naf_2016']]=analysis.model.sim_diff(baseDF = sim_act_99[['cwm_naf_2016']],
                                                           simDFs = list(sim_fore_99[['cwm_naf_2016']]),
                                                           simlabels = list('sim'))
diff_sim_fore_act[['we_naf_2016']]=analysis.model.sim_diff(baseDF = sim_act_99[['we_naf_2016']],
                                                          simDFs = list(sim_fore_99[['we_naf_2016']]),
                                                          simlabels = list('sim'))
plot.Sim_Obs.date(simDFs = list(sim_act_99[['cwm_naf_2016']],sim_fore_99[['cwm_naf_2016']],sim_act_99[['we_naf_2016']],sim_fore_99[['we_naf_2016']]),
             simDFs_leg = list('Act_CWm','Fore_CWm','Act_WE','Fore_WE'),title = "2016 Nafferton")
plot.Sim_Obs.stageFirstDay.plot(diff_sim_fore_act[['cwm_naf_2016']],title="2016 CWm Nafferton")
plot.Sim_Obs.stageFirstDay.plot(diff_sim_fore_act[['we_naf_2016']],title="2016 WE Nafferton")

diff_sim_fore_act[['cwm_naf_2017']]=analysis.model.sim_diff(baseDF = sim_act_99[['cwm_naf_2017']],
                                                           simDFs = list(sim_fore_99[['cwm_naf_2017']]),
                                                           simlabels = list('sim'))
diff_sim_fore_act[['we_naf_2017']]=analysis.model.sim_diff(baseDF = sim_act_99[['we_naf_2017']],
                                                          simDFs = list(sim_fore_99[['we_naf_2017']]),
                                                          simlabels = list('sim'))
plot.Sim_Obs.date(simDFs = list(sim_act_99[['cwm_naf_2017']],sim_fore_99[['cwm_naf_2017']],sim_act_99[['we_naf_2017']],sim_fore_99[['we_naf_2017']]),
             simDFs_leg = list('Act_CWm','Fore_CWm','Act_WE','Fore_WE'),title = "2017 Nafferton")
plot.Sim_Obs.stageFirstDay.plot(diff_sim_fore_act[['cwm_naf_2017']],title="2017 CWm Nafferton")
plot.Sim_Obs.stageFirstDay.plot(diff_sim_fore_act[['we_naf_2017']],title="2017 WE Nafferton")

write.xlsx(diff_sim_fore_act[['cwm_coc_2016']],file = './model_sim_diff.xlsx',sheetName = 'cwm_coc_2016',append = TRUE)
write.xlsx(diff_sim_fore_act[['we_coc_2016']],file = './model_sim_diff.xlsx',sheetName = 'we_coc_2016',append = TRUE)
write.xlsx(diff_sim_fore_act[['cwm_naf_2016']],file = './model_sim_diff.xlsx',sheetName = 'cwm_naf_2016',append = TRUE)
write.xlsx(diff_sim_fore_act[['we_naf_2016']],file = './model_sim_diff.xlsx',sheetName = 'we_naf_2016',append = TRUE)
write.xlsx(diff_sim_fore_act[['cwm_naf_2017']],file = './model_sim_diff1.xlsx',sheetName = 'cwm_naf_2017',append = TRUE)
write.xlsx(diff_sim_fore_act[['we_naf_2017']],file = './model_sim_diff1.xlsx',sheetName = 'we_naf_2017',append = TRUE)

#' diff between models on a site
mo_site_yr_sets=c('cwm_coc_2016','we_coc_2016','cwm_naf_2016','we_naf_2016','cwm_naf_2017','we_naf_2017')
site_yr_sets=c('coc_2016','naf_2016','naf_2017')
diff_model_fore=list()
for(i in 1:length(site_yr_sets)){
  # i=6
  set=site_yr_sets[i]
  we_set=paste("we",set,sep="_")
  cwm_set=paste("cwm",set,sep="_")
  print(set)
  diff_model_fore[[set]]=analysis.model.sim_diff(baseDF = sim_fore_99[[we_set]],
                                                    simDFs = list(sim_fore_99[[cwm_set]]),
                                                    simlabels = list('cwm'))
  
  p3<-ggplot(diff_model_fore[[set]], aes(x=stage_ec,y=diff_fd_cwm))+
    geom_point(shape=18,size=2)+
    xlab("Growth Stage (BBCH)") +
    ylab("Diff. First Appearence Day [cwm-we] (day)") +
    ggtitle(set)+ 
    scale_x_continuous(breaks = c(seq(0,150,by=10)))+
    scale_y_continuous(breaks = c(seq(-100,100,by=5)))+
    geom_smooth(method=loess,   # Add linear regression lines
                #se=FALSE,    # Don't add shaded confidence region
                fullrange=TRUE) # Extend regression lines
  print(p3)
}
#Diff error between model to compare the sensitivity of temp diff to gs error in each model at each stage
diff_diff_model_fore_act=list()
for(i in 1:length(site_yr_sets)){
    # i=6
    set=site_yr_sets[i]
    we_set=paste("we",set,sep="_")
    cwm_set=paste("cwm",set,sep="_")
    print(set)
  
  diff_diff_model_fore_act[[set]]=merge(diff_sim_fore_act[[cwm_set]][,c("stage_ec","diff_du_sim","diff_fd_sim")],diff_sim_fore_act[[we_set]][,c("stage_ec","diff_du_sim","diff_fd_sim")],by="stage_ec",suffix=c("_cwm","_we"))
  diff_diff_model_fore_act[[set]]=mutate(diff_diff_model_fore_act[[set]],diff_model_fd_err= diff_fd_sim_cwm - diff_fd_sim_we)

  p4<-ggplot(diff_diff_model_fore_act[[set]], aes(x=stage_ec,y=diff_model_fd_err))+
    geom_point(shape=18,size=2)+
    xlab("Growth Stage (BBCH)") +
    ylab("Diff. 1st App. Day Error [cwm-we] (day)") +
    ggtitle(set)+ 
    scale_x_continuous(breaks = c(seq(0,150,by=10)))+
    scale_y_continuous(breaks = c(seq(-100,100,by=1)))+
    geom_smooth(method=loess,   # Add linear regression lines
                #se=FALSE,    # Don't add shaded confidence region
                fullrange=TRUE) # Extend regression lines
  print(p4)
  }
#'=============================================================================================================================================================
#'#' create censitive of modelled stage to temperature varience
library(ggplot2)
key_stages=c(21,31,39,59,71)
key_stages_lab=paste("BBCH",key_stages,sep="_")
for(i in 1:length(testsets)){
  # i=6
  set=testsets[i]
  wset=w_sets[i]
  print(set)
  sim_9days[[set]]$diff_sim=analysis.model.sim_diff(baseDF = sim_9days[[set]]$result,
                                                        simDFs = list(sim_9days[[set]]$forecast),
                                                        simlabels = list('sim'))

  plot.Sim_Obs.stageFirstDay.plot(sim_9days[[set]]$diff_sim,title=set)
}

testsets=c('cwm_coc_2016','cwm_naf_2016','cwm_naf_2017','we_coc_2016','we_naf_2016','we_naf_2017')
w_sets=c('coc_2016','naf_2016','naf_2017','coc_2016','naf_2016','naf_2017')
# 9-days analysis
for(i in 1:length(testsets)){
  # i=6
  set=testsets[i]
  wset=w_sets[i]
  print(paste(set,"|",wset))
  # sim_9days[[set]]$merge=full_join(sim_9days[[set]]$result[,c('day','date', 'stage_ec')],sim_9days[[set]]$forecast[,c('day', 'stage_ec')],by="day",all=TRUE,suffix=c('.act','.fore'))
  # sim_9days[[set]]$merge=left_join(sim_9days[[set]]$merge,w_act[[wset]][,c('date','temp_avg')],by='date')
  # sim_9days[[set]]$merge=left_join(sim_9days[[set]]$merge,w_fore[[wset]][,c('date','temp_avg')],by='date',suffix=c('.act','.fore'))
  # sim_9days[[set]]$merge=mutate(sim_9days[[set]]$merge,stage_ec.diff_9days = stage_ec.fore-stage_ec.act)
  # sim_9days[[set]]$merge=mutate(sim_9days[[set]]$merge,temp_avg.diff = temp_avg.fore-temp_avg.act)
  # sim_9days[[set]]$merge=mutate(sim_9days[[set]]$merge,temp_avg.diff_9days = temp_avg.diff+
  #                                                                                            lag(temp_avg.diff,n=1L,default=0)+
  #                                                                                            lag(temp_avg.diff,n=2L,default=0)+
  #                                                                                            lag(temp_avg.diff,n=3L,default=0)+
  #                                                                                            lag(temp_avg.diff,n=4L,default=0)+
  #                                                                                            lag(temp_avg.diff,n=5L,default=0)+
  #                                                                                            lag(temp_avg.diff,n=6L,default=0)+
  #                                                                                            lag(temp_avg.diff,n=7L,default=0)+
  #                                                                                            lag(temp_avg.diff,n=8L,default=0))
  # 
  # sim_9days[[set]]$merge=mutate(sim_9days[[set]]$merge,temp_stage_9days_diff_ratio = stage_ec.diff_9days/temp_avg.diff_9days)
  # 
  # write.xlsx(sim_9days[[set]]$result,file="./sim_9days1.xlsx",sheetName = paste(set,"result"),append=TRUE)
  # write.xlsx(sim_9days[[set]]$forecast,file="./sim_9days1.xlsx",sheetName =  paste(set,"forecast"),append=TRUE)
  # write.xlsx(sim_9days[[set]]$merge,file="./sim_9days1.xlsx",sheetName =  paste(set,"merge"),append=TRUE)

  filterSet=
    #sim_9days[[set]]$merge
    # filter(sim_9days[[set]]$merge,temp_stage_diff_ratio<100 & temp_stage_diff_ratio>-100)
   filter(sim_9days[[set]]$merge,temp_stage_9days_diff_ratio>-0.5 & temp_stage_9days_diff_ratio<0.5)
  print(
    ggplot(filterSet,
           aes(x=stage_ec.act,y=temp_stage_9days_diff_ratio))+
      geom_point(shape=18,size=2)+
      ggtitle(paste("9 days sensitivity (",set,")")) +
      ylab("ΔTemp(oC):ΔForecast(BBCH) (1:y)") +
      xlab("Actual GS (BBCH)") +
      scale_x_continuous(breaks = c(seq(0,150,by=10))) +
      # scale_y_continuous(breaks = seq(-150,150,by=0.5)) +
      geom_vline(xintercept=key_stages, linetype=2, show.legend = T)+
      # geom_text(aes(ket_stages,0,label = ket_stages, hjust = -1))+
      annotate("text", x = key_stages, y = max(filterSet$temp_stage_9days_diff_ratio,na.rm = T), angle = -45, label = key_stages_lab,
               hjust = 0, parse = TRUE)+
      geom_smooth(method=loess,   # Add linear regression lines
                  #se=FALSE,    # Don't add shaded confidence region
                  fullrange=TRUE) # Extend regression lines
  )
  # print(
  #   ggplot(sim_9days[[set]]$merge,
  #          aes(x=stage_ec.act,y=temp_avg.diff_9days))+
  #     geom_point(shape=18,size=3)+
  #     ggtitle(paste("9 days temperature diff (",set,")")) +
  #     ylab("Temp (oC))") +
  #     xlab("Actual Stage (BBCH)")+
  #     scale_x_continuous(breaks = c(seq(0,150,by=10))) +
  #     scale_y_continuous(breaks = seq(-150,150,by=1)) +
  #     geom_vline(xintercept=key_stages, linetype=2, show.legend = T)+
  #     # geom_text(aes(ket_stages,0,label = ket_stages, hjust = -1))+
  #     annotate("text", x = key_stages, y = max(sim_9days[[set]]$merge$temp_avg.diff_9days,na.rm = T), angle = -45, label = key_stages_lab,
  #              hjust = 0, parse = TRUE)+
  #     geom_smooth(method=loess,   # Add linear regression lines
  #                 #se=FALSE,    # Don't add shaded confidence region
  #                 fullrange=TRUE) # Extend regression lines
  # )
  # print (min(sim_9days[[set]]$merge[,'temp_avg.diff_9days'],na.rm = T))
  # print(max(sim_9days[[set]]$merge[,'temp_avg.diff_9days'],na.rm = T))
}

testsets=c('cwm_coc_2016','cwm_naf_2016','cwm_naf_2017','we_coc_2016','we_naf_2016','we_naf_2017')
w_sets=c('coc_2016','naf_2016','naf_2017','coc_2016','naf_2016','naf_2017')
for(i in 1:length(testsets)){
  # i=6
  set=testsets[i]
  wset=w_sets[i]
  print(paste(set,"|",wset))
  
  print(
    ggplot(sim_9days[[set]]$merge,
           aes(x=date,y=temp_avg.diff_9days))+ #stage_ec.act
      geom_point(shape=18,size=2)+
      ggtitle(paste("9 days temperature diff. (",wset,")")) +
      ylab("Temp. Difference (fore. - act.) (oC)") +
      xlab("Actual Stage (BBCH)")+
      # scale_x_continuous(breaks = c(seq(0,150,by=10))) +
      scale_y_continuous(breaks = seq(-150,150,by=5)) +
      geom_vline(xintercept=key_stages, linetype=2, show.legend = T)+
      # geom_text(aes(ket_stages,0,label = ket_stages, hjust = -1))+
      # annotate("text", x = key_stages, y = max(sim_9days[[set]]$merge$temp_avg.diff_9days,na.rm = T), angle = -45, label = key_stages_lab,
      #          hjust = 0, parse = TRUE)+
      geom_smooth(method=loess,   # Add linear regression lines
                  #se=FALSE,    # Don't add shaded confidence region
                  fullrange=TRUE) # Extend regression lines
  )
  # print (min(sim_9days[[set]]$merge[,'temp_avg.diff_9days'],na.rm = T))
  # print(max(sim_9days[[set]]$merge[,'temp_avg.diff_9days'],na.rm = T))
}
w_sets=c('coc_2016','naf_2016','naf_2017')
# 9-days analysis
for(i in 1:length(w_sets)){
  # i=6
  wset=w_sets[i]
plot.Sim_Obs.date(simDFs = list(sim_9days[[paste('cwm',wset,sep="_")]]$result,
                                sim_9days[[paste('cwm',wset,sep="_")]]$forecast,
                                sim_9days[[paste('we',wset,sep="_")]]$result,
                                sim_9days[[paste('we',wset,sep="_")]]$forecast),
                  simDFs_leg = list('Act_CWm','Fore_CWm','Act_WE','Fore_WE'),title=wset)
}

diff_sim_9days=list()
for(i in 1:length(testsets)){
  # i=6
  set=testsets[i]
  
  diff_sim_9days[[set]]=analysis.model.sim_diff(baseDF = sim_9days[[set]]$result,
                                                         simDFs = list(sim_9days[[set]]$forecast),
                                                         simlabels = list('sim'))
  print(paste("Regression (",set,")"))
  plot.Sim_Obs.stageFirstDay.reg(diff_sim_9days[[set]],title=paste("Regression (",set,")"))
}


##################################
#Parameter validation on Nafferton 2004,2007,2008
test_sets=c("naf_2004","naf_2007","naf_2008")
test_obs=c("2004","2007","2008")
result=list()
err_result=list()
for(i in 1:3){
  wSet=w_act[[test_sets[i]]]
  oSet=o_naf[[test_obs[i]]]
  
 
  
  sown_date=oSet[1,]$date #assumed swon date on row 1
  result[[test_sets[i]]][['cwm']]=cwm.main(weather_data_dt = wSet,
           str_param_file = "./Parameters/CWm_Parameters.xlsx",conf_id = 10,
           sown_date = sown_date)
  err_result[[test_sets[i]]][['cwm']]=analysis.obsDay.err(obsDF = oSet,simDF = result[[test_sets[i]]][['cwm']])
  
  result[[test_sets[i]]][['we']]=we.main(weather_data_dt = wSet,
                  str_param_file = "./Parameters/WE_Parameters.xlsx",conf_id = 7,
                  sown_date = sown_date)
  err_result[[test_sets[i]]][['we']]=analysis.obsDay.err(obsDF = oSet,simDF = result[[test_sets[i]]][['we']])
  
  # plot.Sim_Obs(obsDF = oSet,simDFs = list(result[['cwm']],result[['we']]),simDFs_leg = list('cwm','we'),title = test_sets[i])
}
for(i in 1:3){
  set_name=test_sets[i]
  write.xlsx(result[[test_sets[i]]][['cwm']],append = T,file='./S_Naf_2004_08.xlsx',sheetName = paste(set_name,'cwm'))
             write.xlsx(result[[test_sets[i]]][['we']],append = T,file='./S_Naf_2004_08.xlsx',sheetName = paste(set_name,'we'))
}

diff_err_result=list()
for(i in 1:3){
  diff_err_result[[i]]=merge(err_result[[test_sets[i]]][['cwm']],err_result[[test_sets[i]]][['we']][,c('stage_ec','err')],suffix=c('.cwm','.we'),by='stage_ec')
}

###########################################
plot.Sim_Obs(obsDF = o_naf[['2017']],
             simDFs = list(sim_9days[['cwm_naf_2017']]$result,sim_9days[['we_naf_2017']]$result,sim_9days[['cwm_naf_2017']]$forecast,sim_9days[['we_naf_2017']]$forecast,
                           sim_fore_99[['cwm_naf_2017']], sim_fore_99[['we_naf_2017']]),
             simDFs_leg = c('CWm_Act.','WE_Act.',"CWm_9days_Fore.",'WE_9days_Fore.',"CWm_Season_Fore.",'WE_Season_Fore.'))
plot.Sim_Obs.internal(obsDF = o_naf[['2017']],
                      simDFs = list(sim_9days[['cwm_naf_2017']]$result,sim_9days[['cwm_naf_2017']]$forecast,sim_fore_99[['cwm_naf_2017']],
                      sim_9days[['cwm_naf_2016']]$result,sim_9days[['cwm_naf_2016']]$forecast,sim_fore_99[['cwm_naf_2016']]),
                      simDFs_leg = c('CWm17_Act.',"CWm17_9days_Fore.","CWm17_Season_Fore.",
                                     'CWm16_Act.',"CWm16_9days_Fore.","CWm16_Season_Fore."))
plot.Sim_Obs.internal(obsDF = o_naf[['2017']],
                      simDFs = list(sim_9days[['we_naf_2017']]$result,sim_9days[['we_naf_2017']]$forecast,sim_fore_99[['we_naf_2017']],
                                    sim_9days[['we_naf_2016']]$result,sim_9days[['we_naf_2016']]$forecast,sim_fore_99[['we_naf_2016']]),
                      simDFs_leg = c('WE17_Act.','WE17_9days_Fore.','WE17_Season_Fore.',
                                     'WE16_Act.','WE16_9days_Fore.','WE16_Season_Fore.'))

analysis.obsDay.err.rmse(obsDF = o_naf[['2017']],simDF = sim_fore_99[['cwm_naf_2017']])
analysis.obsDay.err.rmse(obsDF = o_naf[['2017']],simDF = sim_fore_99[['we_naf_2017']])
analysis.obsDay.err.rmse(obsDF = o_naf[['2017']],simDF = sim_act_99[['cwm_naf_2017']])
analysis.obsDay.err.rmse(obsDF = o_naf[['2017']],simDF = sim_act_99[['we_naf_2017']])
plot.Sim_Obs(obsDF = o_win_2016,
             simDFs = list(sim_act_99[['cwm_win_2016']], sim_act_99[['we_win_2016']]),
             simDFs_leg = c('CWm_Act.','WE_Act.'))
analysis.obsDay.err.rmse(obsDF = o_win_2016,simDF = sim_act_99[['cwm_win_2016']])
analysis.obsDay.err.rmse(obsDF = o_win_2016,simDF = sim_act_99[['we_win_2016']])
