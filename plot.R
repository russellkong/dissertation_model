

plot.EC <- function(wangDF, cwmDF) {
  rs_wang<- wangDF[,c("day","stage_ec")]
  rs_cwm<- cwmDF[,c("day","stage_ec")]
  
  rs_cwm$cwm_ec <- rs_cwm$stage_ec
  rs_wang$wang_ec <- rs_wang$stage_ec
  
  rs1<-merge(rs_cwm,rs_wang,by="day",all=TRUE)
  
  
  ggplot(rs1)+
    geom_line( aes(x=day,y=cwm_ec,colour="CWm"))+
    geom_line( aes(x=day,y=wang_ec,colour="Wang"),position=position_dodge(0.1))+
    ggtitle("Model Output Comparison") + 
    ylab("Growth Stage (BBCH)") +
    xlab("Day")
}

#' Title
#'
#' @param sim_df 
#' @param obs_df 
#'
#' @return
#' @export
#'
#' @examples plot.Sim_Obs(PE.resultObj.working$resultDF[[147]][[1]],measured_df)
plot.Sim_Obs <- function(simDF, obsDF) {
  rs_sim<- simDF[,c("day","stage_ec")]
  rs_obs<- obsDF[,c("day","stage_ec")]
  
  rs_sim$sim_ec <- rs_sim$stage_ec
  rs_obs$obs_ec <- rs_obs$stage_ec
  
  rs1<-merge(rs_sim,rs_obs,by="day",all=TRUE)
  
  ggplot(rs1)+
    geom_point( aes(x=day,y=obs_ec,colour="Observed"),shape=18,size=4)+
    geom_point( aes(x=day,y=sim_ec,colour="Simulated"),shape=4)+
    geom_line( aes(x=day,y=sim_ec,colour="Simulated"),size=1)+
    ylab("Growth Stage (BBCH)") +
    xlab("Day")
  
}


#' Plat the geographic curve to optim zone
#' HOW????
#' 
#' @return
#' @export
#'
#' @examples
plot.optim_path<-function(){
  param_map<-do.call(Map,c(c,PE.resultObj.working$param_custom))
  param_map$RMSE<-do.call(Map,c(c,PE.resultObj.working$RMSE))[[1]]
  param_df<-as.data.frame(param_map)
  
}
#' c("./Weather/W_100EA002_2016.xlsx","./Weather/W_nafferton_2017.xlsx"),"./Parameters/Wang_Parameters.xlsx",c("./Phenology/P_WindsorWest_2016.xlsx")
prepare.data<-function(str_weather_files,
                       str_measured_files){
  
  #Preload all weather files
  list_weather_data<-list()
  for(i in 1:length(str_weather_files)){
    weather_data_df <-
      load_weather(str_weather_files[i])
    list_weather_data[[as.character(weather_data_df$site[1])]]<-weather_data_df
  }
  list_weather_data<<-list_weather_data
  
  #Preload all measured data
  list_measured_data<-list()
  for(i in 1:length(str_measured_files)){
    measured_df <- read_excel(str_measured_files[i])
    #assume site specified as the same in whole column
    list_measured_data[[as.character(measured_df$site[1])]]<-measured_df
  }
  list_measured_data<<-list_measured_data
  
}

plot.wang.gradient<-function(){
  prepare.data( c("./Weather/W_100EA002_2016.xlsx"),c("./Phenology/P_WindsorWest_2016.xlsx"))
  
  #load parameters
  parameters_def <- new('WangParameterSet')
  parameters_def <- wang.set_param("./Parameters/Wang_Parameters.xlsx", parameters_def, 2)
  
  x<-seq(1, 10, length=10)
  y<-seq(30, 70, length=10)
  param_opti<-c( "temp_vn_opt","vern_full")
  
  f<-plot.wapper.wang.rmse
  z<-outer(x,y,Vectorize(f), param_custom_opti=param_opti, list_weather_data=list_weather_data,  param_def=parameters_def, list_measured_data=list_measured_data, end_stage=40,retain_rs=FALSE)
  
  nrz<-nrow(z)
  ncz<-ncol(z)
  jet.colors <-  colorRampPalette(c("midnightblue","blue",
                                    "cyan","green", "yellow","orange","red", "darkred"))
  nbcol<-64
  color<-jet.colors(nbcol)
  zfacet<-z[-1,-1]+z[-1,-ncz]+z[-nrz,-1]+z[-nrz,-ncz]
  facetcol<-cut(zfacet,nbcol)
  print(persp(x,y,z,col=color[facetcol],phi=20,theta=-60,
              ticktype="detailed",d=5,r=1,shade=0.1,expand=0.6 ) )
}

plot.cwm.gradient<-function(){
  prepare.data( c("./Weather/W_100EA002_2016.xlsx"),c("./Phenology/P_WindsorWest_2016.xlsx"))
  
  #load parameters
  parameters_def <- new('CWmParameterSet')
  parameters_def <- cwm.set_param("./Parameters/CWm_Parameters.xlsx", parameters_def, 10)
  
  x<-seq(40, 70, length=10)
  y<-seq(80, 110, length=10)
  param_opti<-c("t_sum_internode","ph39")
  end_stage<-57
  
  f<-plot.wapper.cwm.rmse
  z<-outer(x,y,Vectorize(f), param_custom_opti=param_opti, list_weather_data=list_weather_data,  param_def=parameters_def, list_measured_data=list_measured_data, end_stage=end_stage,retain_rs=FALSE)
  
  nrz<-nrow(z)
  ncz<-ncol(z)
  jet.colors <-  colorRampPalette(c("midnightblue","blue",
                                    "cyan","green", "yellow","orange","red", "darkred"))
  nbcol<-64
  color<-jet.colors(nbcol)
  zfacet<-z[-1,-1]+z[-1,-ncz]+z[-nrz,-1]+z[-nrz,-ncz]
  facetcol<-cut(zfacet,nbcol)
  print(persp(x,y,z,col=color[facetcol],phi=20,theta=30,
              ticktype="detailed",d=5,r=1,shade=0.1,expand=0.6 ) )
  
  print(which(z==min(z),arr.ind = TRUE))
}
plot.wapper.wang.rmse<-function(x,y, ...){
  return(wang.rmse(param_custom = c(x,y),...))
}
plot.wapper.cwm.rmse<-function(x,y, ...){
  return(cwm.rmse(param_custom = c(x,y),...))
}