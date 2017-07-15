

plot.EC <- function(wang_df, cwm_df) {
  rs_wang<- subset(wang_df,select=c(day,stage_ec))
  rs_cwm<- subset(cwm_df,select=c(day,stage_ec))
  
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
plot.Sim_Obs <- function(sim_df, obs_df) {
  rs_sim<- subset(sim_df,select=c(day,stage_ec))
  rs_obs<- subset(obs_df,select=c(day,stage_ec))
  
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
