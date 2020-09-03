#' Main simulation function
#'
#' This function wraps up all simulation sub-functins and applies them to
#' the intermediate sim files (created by `disnet_sim_setup`
#'
#' @param nsims The number of simulations to run. Default is 10
#' @param nsteps The number of timesteps you'd like each simulation to run
#' for. Default = 1000. It's set to a large value so that outbreaks can
#' run their course.
#' @param sim_input The file setup/created by disnet_sim_setup, which has
#' incorprated the foi, infection seeding, and/or vaccination campaign.
#' @param sim_output_dir The directory you'd like to save the simulation results to.
#'
#' @examples
#' f = system.file("sample_data", "g.rds", package = "SEEDNet")
#' g = readRDS(f)
#' g_comm = disnet_commuting(g)
#' nodes = igraph::vcount(g_comm)
#' set.seed(890)
#' seed_nd = igraph::vertex_attr(g_comm, "name", sample(1:nodes, 1))
#' for_sim = disnet_sim_setup(g_comm, seed_nd = seed_nd, output_dir = NA)
#' simres = disnet_simulate(sim_input = for_sim)

#' @export

disnet_simulate = function(nsims = 10,
                   nsteps = 1000,
                   sim_input = sim_intermed,
                   sim_output_dir = getOption("disnetOutputDir", NA),
                   parallel = FALSE,
                   seed = 0)
{
    # create directory to store results in
    if(!is.na(sim_output_dir) && !dir.exists(sim_output_dir)) {
        dir.create(sim_output_dir, recursive = TRUE)
    }
    # start simulation message
    message("\nStarting simulations\n")
    # set seed to ensure replicability
    # Clark: Let the user set the seed before they call this function.
    set.seed(seed)
    # simulations
    if(parallel) {
        this_lapply = future.apply::future_lapply
        future.seed = seed
    } else {
        this_lapply = lapply
        future.seed = NULL
    }
    
    
    tmp = getIdxAcomp2Groups(sim_input$vert_list)
    sim_res = this_lapply(1:nsims, disnet_sim_lapply,
                     nsteps,
                     start_TS = sim_input$start_TS,
                     vert_list = sim_input$vert_list,
                     j_out = sim_input$j_out,
                     params = sim_input$params,
                     sim_dir = sim_output_dir,
                     idx = NULL,
                     acomp2_sub = NULL,
                     groups = NULL,
                     future.seed = future.seed
                     )
    return(sim_res)
}



getIdxAcomp2Groups =
function(vert_list)
{
    comp2_sub = vert_list[[3]]
    comp2_sub_l = do.call(rbind, lapply(comp2_sub, length))
    comp2_sub[ comp2_sub_l %in% 0 ] = list(data.frame(name = NA, comp2_sub = 0))
    
    rowIds = unlist(lapply(comp2_sub, `[[`, 1))   # doesn't get used after this computation.
    groups = rep(1:length(comp2_sub), sapply(comp2_sub, nrow))
    acomp2_sub = unlist(lapply(comp2_sub, `[[`, 2))
    idx = match(rowIds, vert_list[[1]]$name)
    list(idx = idx, acomp2_sub = acomp2_sub, groups = groups)
}


# *** For testing simulations
if(FALSE)
{

    sim_input = for_sim
    sim = 1
    nsteps = 100
    start_TS = sim_input$start_TS
    vert_list = sim_input$vert_list
    j_out = sim_input$j_out
    params = sim_input$params
    sim_output_dir = "data/sim-res/test"
    sim_dir = sim_output_dir
    # idx = tmp$idx
    # acomp2_sub = tmp$acomp2_sub
    # groups = tmp$groups

}


