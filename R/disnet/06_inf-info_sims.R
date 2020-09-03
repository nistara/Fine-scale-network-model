
#' Compare outbreak information across simulations
#'
#' @param sims Individual simulation results
#' @param sim_info Simulation information files
#' @param sim_l Simulation length information
#'
#' @export
#' 
get_inf_info = function(sims, sim_info, sim_l) {
    nds = sapply(sims, function(l) {
        nds = lapply(l, function(df) {
            df = unique(df[ df$I > 0 | df$Ia > 0, "name"])        
        })
        max(1, length(unique(unlist(nds))))
    })
    inf = sapply(sim_info, function(df) {
        if( is.null(nrow(df)) & length(df) == 5 ) {
            inf = 1
        } else {
            inf = df[nrow(df), "R"] - df[1, "R"]
        }
        names(inf) = NULL
        inf
    })
    df = data.frame(nds = nds,
                    inf = inf,
                    days = sim_l,
                    stringsAsFactors = FALSE)
    df$type = ifelse(df$nds == 1, 1,
              ifelse(df$nds > 1 &df$nds < 100, 2,
              ifelse(df$nds > 100, 3, NA)))
    df
}



#' Get summary of outbreak types
#'
#' @param inf_info Information on outbreak infections wrt nodes affected,
#' outbreak lengths, and numbers infected. 
#'
#' @export
#' 
get_sim_summ = function(inf_info) {
    sim_summ_l = sapply(split(inf_info, inf_info$type), nrow)

    sim_summ_info = round(inf_info %>%
                          group_by(type) %>%
                          summarise_all(list(mean = mean,
                                             sd = sd,
                                             min = min,
                                             max = max)), 1)

    sim_summ_info$n = sim_summ_l

    as.data.frame(
        sim_summ_info[ , c("n", "type",
                           "nds_min",
                           "nds_mean",
                           "nds_sd",
                           "nds_max",
                           "days_min",
                           "days_mean",
                           "days_sd",
                           "days_max",
                           "inf_min",
                           "inf_mean",
                           "inf_sd",
                           "inf_max")])
}



# ==============================================================================
# Subsetting outbreaks
# ==============================================================================

#' Subset information for outbreaks
#'
#' @param sim_info Simulation information
#' @param inf_info Infection information for each simulation
#'
#' @export
#'
# choosing those that belong to type 3, based upon inf_info
get_outbrks_info = function(sim_info, inf_info){
    sim_info[ inf_info$type == 3 ]
}


#' Subset simulations that were outbreaks
#'
#' @param sims Simulation results
#' @param inf_info Infection information for each simulation
#'
#' @export
#'
# choosing those that belong to type 3, based upon inf_info
get_outbrks = function(sims, inf_info){
    sims[ inf_info$type == 3 ]
}


         


