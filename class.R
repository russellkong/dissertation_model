
setClass("WangPrediction",
         representation(
           temp_resp_rate="numeric",
           photo_resp_rate="numeric",
           vern_temp_sum="numeric",
           vern_resp_rate="numeric",
           vern_temp_resp="numeric",
           leave_prim_rate="numeric",
           leave_app_rate="numeric",
           leave_unemerge="numeric",
           leave_sum="numeric",
           leave_temp_resp="numeric",
           dev_v_rate="numeric",
           dev_r_rate="numeric",
           dev_em_rate="numeric",
           node_rate="numeric",
           node_sum="numeric",
           till_main_sum="numeric",
           alpha="numeric",
           omega="numeric"
         ),prototype(
           stage_dev=-1,
           stage_ec=0,
           temp_resp_rate=0,
           photo_resp_rate=0,
           vern_temp_sum=0,
           vern_resp_rate=0,
           vern_temp_resp=0,
           leave_prim_rate=0,
           leave_app_rate=0,
           leave_unemerge=0,
           leave_sum=0,
           leave_temp_resp=0,
           dev_v_rate=0,
           dev_r_rate=0,
           dev_em_rate=0,
           node_rate=0,
           node_sum=0,
           till_main_sum=0,
           alpha=0,
           omega=0
         ), contains = "Prediction")
setClass("WangParameterSet",
         representation(
           temp_cardinal="data.frame",
           temp_emerg_sum="numeric",
           temp_base="numeric",
           photo_crit="numeric",
           photo_sig="numeric",
           photo_sen="numeric",
           photo_opp="numeric",
           vern_base="numeric",
           vern_full="numeric",
           leave_prim_mx="numeric",
           leave_app_mx="numeric",
           node_mx="numeric",
           stage_vr_ec_table="numeric",
           dev_v_max_rate="numeric",
           dev_r_max_rate="numeric"
         ), contains = "ParameterSet")


setClass("CWmPrediction",
         representation(
           stage_dev_rate="numeric",
           stage_ec_rate="numeric",
           photo_resp_rate="numeric",
           vern_resp_rate="numeric",
           fl="numeric",
           leave_prim="numeric",
           leave_emerg="numeric"
         ),prototype(
           stage_dev=0,
           stage_ec=0,
           stage_dev_rate=0,
           stage_ec_rate=0,
           photo_resp_rate=0,
           vern_resp_rate=0,
           fl=0,
           leave_prim=4,
           leave_emerg=1
         ), contains = "Prediction")

#' Title
#'
#' @slot temp_base numeric. 
#' @slot p9 numeric. 
#' @slot phyllochron numeric. 
#' @slot p1d numeric. Photoperiod sensitivity coeff. %/10h
#' @slot p1dt numeric.  Photoperiod threshold          h
#' @slot p1v numeric.    Vernalization requirement      d
#' @slot p1vd numeric.    Sufficient Vernalization total      d
#' @slot plastochron numeric. 
#' @slot gs_flp numeric. 
#' @slot t_sum_internode numeric. 
#' @slot ph39 numeric. 
#' @slot p5 numeric. 
#' @slot s1_therm_day numeric. 
#' @slot s4_therm_day numeric. 
#' @slot s6_therm_day numeric. 
#' @slot leave_prim_init numeric. 
#' @slot leave_emerg_init numeric. 
#'
#' @return
#' @export
#'
#' @examples
setClass("CWmParameterSet",
         representation(
           #temp_cardinal="data.frame",
           #temp_emerg_sum="numeric",
           temp_base="numeric",
           p9="numeric",
           phyllochron="numeric", ##PHINT
           p1d="numeric",
           p1dt="numeric",
           p1v="numeric",
           p1vd="numeric",
           plastochron="numeric", ##PLAST
           gs_flp="numeric",
           t_sum_internode="numeric",
           ph39="numeric",
           p5="numeric",
           s1_therm_day="numeric", ##400
           s4_therm_day="numeric", ##200
           s6_therm_day="numeric", ##250
           leave_prim_init="numeric", ##4
           leave_emerg_init="numeric" ##1
         ), contains = "ParameterSet")
