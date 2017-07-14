

plotEC <- function(wang_df, cwm_df) {
  rs_wang<- subset(wang_df,select=c(date,stage_ec))
  rs_cwm<- subset(cwm_df,select=c(date,stage_ec))
  
  rs_cwm$model <- with(rs_cwm, "cwm")
  rs_wang$model <- with(rs_cwm, "wang")
  
  rs<-rbind.data.frame(rs_wang,rs_cwm)
  
  rs$date<-as.Date.character(rs$date,format = '%Y-%m-%d')
  
  ggplot(rs,aes(date, stage_ec,shape=model,color=model,linetype=model)) +
    geom_line(position=position_dodge(0.1)) + 
    geom_point(position=position_dodge(0.1), size=2) +
    ggtitle("Model Output Comparison") + 
    ylab("BBCH Growth Stage") +
    xlab("Date")
}


plotEC0 <- function(result1, result2) {
  ggplot() +
    geom_point(data = result1, aes(result1$date, result1$stage_ec),shape=1) +
    geom_point(data = result2, aes(result2$date, result2$stage_ec),shape=1) +
    geom_line(data = result1, aes(result1$date, result1$stage_ec),colour="blue") +
    geom_line(data = result2, aes(result2$date, result2$stage_ec),colour="red")
}
