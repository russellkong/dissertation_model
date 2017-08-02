setClass(
  "Weather",
  representation(
    date = "POSIXct",
    temp_avg = "numeric",
    temp_min = "numeric",
    temp_max = "numeric",
    photo_len = "numeric",
    source = "character"
  )
)

setMethod("as.data.frame", "Weather",
          function(x,
                   row.names = NULL,
                   optional = FALSE,
                   ...) {
            slotnames <- slotNames(x)
            slotlist <-
              data.frame(rbind(replicate(length(slotnames), 0)))
            names(slotlist) <- slotnames
            for (i in slotnames) {
              if (length(slot(x,  i)) > 0) {
                if (is(slot(x,  i), "POSIXct")) {
                  slotlist[1, i] <-
                    as.character(slot(x,  i))
                }
                else {
                  slotlist[1, i] <- slot(x,  i)
                }
              }
            }
            return(slotlist)
          })
setClass(
  "Prediction",
  representation(
    day = "numeric",
    date = "POSIXct",
    stage_dev = "numeric",
    stage_ec = "numeric",
    weather_source = "character"
  )
)

#' Title
#'
#' @param Prediction
#'
#' @return
#' @export
#'
#' @examples
setMethod("as.data.frame", "Prediction",
          function(x,
                   row.names = NULL,
                   optional = FALSE,
                   ...) {
            slotnames <- slotNames(x)
            slotlist <-
              data.frame(rbind(replicate(length(slotnames), 0)))
            names(slotlist) <- slotnames
            for (i in slotnames) {
              if (length(slot(x,  i)) > 0) {
                if (is(slot(x,  i), "POSIXct")) {
                  slotlist[1, i] <-
                    as.character(slot(x,  i))
                }
                else {
                  slotlist[1, i] <- slot(x,  i)
                }
              }
            }
            return(slotlist)
          })


#' Title
#'
#' @slot id numeric. 
#'
#' @return
#' @export
#'
#' @examples
setClass("ParameterSet",
         representation(conf_id = "numeric", model="character", weather_file="character"))

setMethod("as.data.frame", "ParameterSet",
          function(x,
                   row.names = NULL,
                   optional = FALSE,
                   ...) {
            slotnames <- slotNames(x)
            slotlist <-
              data.frame(rbind(replicate(length(slotnames), 0)))
            names(slotlist) <- slotnames
            for (i in slotnames) {
              if (length(slot(x,  i)) > 0) {
                if (is(slot(x,  i), "POSIXct")) {
                  slotlist[1, i] <-
                    as.character(slot(x,  i))
                }else if (is(slot(x,  i), "data.frame")) {
                  slotlist[1, i] <- paste(as.character(slot(x,  i)),collapse = "|")
                } else {
                  slotlist[1, i] <- slot(x,  i)
                }
              }
            }
            return(slotlist)
          })


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
           photo_sig="numeric", #'[1:long day plant|-1:short day plant]
           photo_sen="numeric", #'omega value photoperiod sensitivity factor
           photo_opp="numeric", #'set as constant [17.7h: long-day crops (wheat, barley, oat, rye, flax, rape),
                                #' 12.5h: maize, 12h: sorghum, 12-14h: Mungbeans]
           vern_base="numeric",
           vern_full="numeric",
           leave_prim_mx="numeric",
           leave_app_mx="numeric",
           node_mx="numeric",
           stage_vr_ec_table="numeric",
           dev_v_min_day="numeric",
           dev_r_min_day="numeric"
         ), contains = "ParameterSet")


setClass("CWmPrediction",
         representation(
           stage_dev_rate="numeric",
           stage_ec_rate="numeric",
           photo_resp_rate="numeric",
           vern_eff="numeric",
           vern_cum_day="numeric",
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
#' @slot p1d numeric. Photoperiod sensitivity coeff. [0-3] %/10h
#' @slot p1dt numeric.  Photoperiod threshold          h
#' @slot p1v numeric.    Vernalization requirement   [0-8]   d
#' @slot p1vd numeric.    Sufficient Vernalization total      d
#' @slot plastochron numeric. 
#' @slot gs_flp numeric. 
#' @slot t_sum_internode numeric. 
#' @slot ph39 numeric. 
#' @slot p5 numeric. Value for grain filling [0-8] for hermal time for maturity (TTM)
#' @slot s1_therm_day numeric. ~400 degree-days termainal spikelet
#' @slot s4_therm_day numeric. ~200 degree-days ear growth to beginning of grain filling
#' @slot s6_therm_day numeric. ~250 degree-days from maturity to harvest
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
