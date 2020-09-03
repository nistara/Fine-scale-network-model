#' Get simulation lines for plotting
#'
#' @param i iteration over simulations to plot
#' @param f_outbrks_info location of file with outbreak information
#' @param f_outbrks_l location of file with outbreak lengths information
#'
#' @export
#' 
get_sim_lines = function(i, f_outbrks_info, f_outbrks_l) {
    # Read in outbreak info & length files----------------------------
    outbrks_info = readRDS(f_outbrks_info[i])
    outbrks_l = readRDS(f_outbrks_l[i])

    # Get sim name--------------------------------------------------------------
    sim_name = gsub("outbrks-info_", "", strsplit(f_outbrks_info[i], "/")[[1]][3])

    # getting infection info----------------------------------------------------
    n = max(outbrks_l)
    inf = lapply(outbrks_info, function(x, n) {
        inf_sum = x[ , "I"] + x[ , "Ia"]
        length(inf_sum) = n
        inf_sum
    }, n)

    inf_df = do.call(cbind, inf)
    inf_mean = rowMeans(inf_df, na.rm = TRUE)
    inf_median = apply(inf_df, 1, median, na.rm = TRUE)

    list(sim_name = sim_name,
         inf_df = inf_df,
         inf_mean = inf_mean,
         inf_median = inf_median)

}
