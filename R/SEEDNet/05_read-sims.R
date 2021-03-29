# ==============================================================================
# Code for reading in simulation files
# ==============================================================================

#' Get simulation information function
#'
#' Reads in the simulation information files
#'
#' @param sim_dir The directory containing simulation results.
#'
#' @export
#' 
get_sim_info = function(sim_dir, nsims = NA) {
    fnames = list.files(sim_dir, pattern = "_info")
    fnames = stringr::str_sort(fnames, numeric = TRUE)
    if(!is.na(nsims)) fnames = fnames[ 1:nsims ] 
    sim_info = lapply(fnames, function(f, sim_dir) {
        readRDS(paste0(sim_dir, "/", f))
    }, sim_dir)
    return(sim_info)
}


#' Get simulation durations/lengths
#'
#' This function uses the simulation information files to get the lengths of
#' each simulations
#'
#' @param sim_info_df The simulation information files for each simulation
#'
#' @export
#' 
get_sim_l = function(sim_info_df) {
    sim_lengths = lapply(sim_info_df, function(x) {
        if( is.null(nrow(x)) & length(x) == 5 ) {
            rows = 1
        } else {
            rows = nrow(x)
        }
    })
    do.call(rbind, sim_lengths)
}


#' Read in individual simulation files
#'
#' Imports each individual simulation file for the set of simulations.
#'
#' @param sim_dir The directory containing simulation results.
#' @param silent Option to print directory and files being read in.
#'               Default is TRUE (don't print).
#' 
#' @export
#' 
get_sims = function(sim_dir, silent = FALSE, nsims = NA) {
    fnames = list.files(sim_dir, pattern = "[0-9].RDS")
    fnames = stringr::str_sort(fnames, numeric = TRUE)
    if(!is.na(nsims)) fnames = fnames[ 1:nsims ] 
    if(silent) {
        sims = lapply(fnames, function(f, sim_dir) {
            readRDS(paste0(sim_dir, "/", f))
        }, sim_dir)
    } else {

        cat("-------------------------------------------------------\n")
        cat("******** Dir: ", sim_dir, "\n")
        sims = lapply(fnames, function(f, sim_dir) {
            cat("\r\tFile: ", f)
            readRDS(paste0(sim_dir, "/", f))
        }, sim_dir)
        cat("\n")
    }
    sims
}
