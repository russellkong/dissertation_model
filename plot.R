
library(ggplot2)

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

#' Plot observed GS with one/list of simulated GS
#' 1. day, GS plot
#' 2. obs GS to sim GS scatter
#' @param obsDF 
#' @param simDF result from simulation
#' @param simDFs Assigned would ignore simDF
#' @param simDFs_leg Labeling of each simulated dataset
#' @param title title of the graph
#'
#' @return
#' @export
#'
#' @examples plot.Sim_Obs(PE.resultObj.working$resultDF[[147]][[1]],measured_df)
plot.Sim_Obs <- function(obsDF,simDF=NULL,simDFs=NULL,simDFs_leg=NULL, title=NULL) {
  
  #rename fore sim DF for merge
  rs_sim<-list()
  if(!is.null(simDFs)){
    if(is.null(simDFs_leg))simDFs_leg<-names(simDFs)
    for(i in 1:length(simDFs)){
      rs_sim[[i]]<-simDFs[[i]][,c("day","stage_ec")]
      
      names(rs_sim[[i]])<-c("day",paste(simDFs_leg[i],"_ec",sep = ""))
    }
  }else if(!is.null(simDF)){
    if(is.null(simDFs_leg))simDFs_leg[1]<-"result"
    rs_sim[[1]]<- simDF[,c("day","stage_ec")]
    names(rs_sim[[1]])<-c("day",paste(simDFs_leg[1],"_ec",sep = ""))
  }else{
    stop("Either simDF/simDFs must be defined")
  }
  
  #define names for observed DF for merge
  rs_obs<- obsDF[,c("day","stage_ec")]
  names(rs_obs) <-c("day","obs_ec")
  
  rs1<-rs_obs
  for(i in 1:length(rs_sim)){
    rs1<-merge(rs1,rs_sim[[i]],by="day",all=TRUE)
  }
  
  p<-ggplot(rs1)+
    geom_point( aes(x=day,y=obs_ec,colour="Observed"),shape=18,size=4)+
    
    ylab("Growth Stage (BBCH)") +
    xlab("Day") +
    ggtitle(title)
  
  p_str<-"p <- p "
  for(i in 1:length(rs_sim)){
    p_str<-paste(p_str,"+ geom_point( aes(x=day,y=",simDFs_leg[i],"_ec,colour=\"Simulated_",simDFs_leg[i],"\"),shape=4) +
              geom_line( aes(x=day,y=",simDFs_leg[i],"_ec,colour=\"Simulated_",simDFs_leg[i],"\"),size=1)",sep = "")
  }
  print_debug(parse(text=p_str))
  eval(parse(text=p_str))
  print(p)
  #'--------------------------------
  #'
 
  rs2<-gather(rs1, sources,sim_ec,-c(day,obs_ec))
  print("lm")
  lm.rs<-lm(sim_ec~obs_ec,data=rs2)
  print(summary(lm.rs))
  #print(plot(lm.rs,las=1))
  
  p2<-ggplot(rs2, aes(x=obs_ec,y=sim_ec))+
    geom_point(shape=18,size=4)+
    xlab("Observed Stage (BBCH)") +
    ylab("Simulated Stage (BBCH)") +
    #ggtitle(title)+ 
    geom_smooth(method=lm,   # Add linear regression lines
              #se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines
  
  #print_debug(parse(text=p2_str))
  #eval(parse(text=p2_str))
  print(p2)
  
  return(rs1)
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

plot.wang.gradient<-function(str_weather_files,
                             str_measured_files){
  list_weather_data<-load.weather.data( str_weather_files)
  list_measured_data<-load.phenology.data(str_measured_files)
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
  list_weather_data<-load.weather.data(c("./Weather/W_100EA002_2016.xlsx"))
  list_measured_data<-load.phenology.data(c("./Phenology/P_WindsorWest_2016.xlsx"))
  
  #load parameters
  parameters_def <- new('CWmParameterSet')
  parameters_def <- cwm.set_param("./Parameters/CWm_Parameters.xlsx", parameters_def, 9)
  
  x<-seq(11, 46, length=10)
  y<-seq(1, 2, length=10)
  param_opti<-c("plastochron", "gs_flp")#c("t_sum_internode","ph39")
  end_stage<-40
  
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
  print(persp(x,y,z,col=color[facetcol],phi=50,theta=80,
              ticktype="detailed",d=5,r=1,shade=0.1,expand=0.6 ) )
  
  print(which(z==min(z),arr.ind = TRUE))
  return(list(x=x,y=y,z=z))
}
plot.wapper.wang.rmse<-function(x,y, ...){
  return(calibrate.wang.rmse(param_custom = c(x,y),...))
}
plot.wapper.cwm.rmse<-function(x,y, ...){
  return(calibrate.cwm.rmse(param_custom = c(x,y),...))
}