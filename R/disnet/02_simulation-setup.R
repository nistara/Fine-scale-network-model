#' Set up graph file to run simulations upon 
#'
#' @details
#' 1. Reads in an `igraph` object (with commuting proportions previously
#'      calculated)
#' 
#' 2. Calculates and attaches  "Effective Population" to graph
#' 
#' 3. Vaccinates target nodes with vaccination proportion:
#'    vaccination proportions = vaccination coverage * vaccination efficacy
#'    The proportion of a node that is vaccinated is removed from the
#'    Susceptible compartment and added to the Recovered compartment
#' 
#' 3. Seeds a node with specified number of infectious individuals (taking them
#'      out from the Susceptibles and adding to Infectious compartment
#' 
#' 4. Creates the FOI structure/scaffold used to quickly calculate FOI
#'      (FOI = Force of Infection, acting on an individual in a node)
#'
#' @param g The network object created after commuting rates were calculated
#' @param r0 Reproduction number
#' @param latent_period Period between infection and becoming infectious
#' @param inf_period Duration of infectious period
#' @param tau Return rate (from commuting)
#' @param r_beta Reduction in infectiousness due to being aymptomatic inf
#' @param p_a Probability of asymptomatic infection
#' @param seed_nd The node which you want to seed/initiate infection in
#' @param seed_no The number of infected people you want to start inf with
#' @param vacc Whether a vaccination campaign is underway or not
#' @param output_dir Directory to store intermediate simulation data in.
#' @param vacc_eff Vaccination efficacy
#' @param vacc_cov Vaccination coverage
#' @param vacc_nd Node you want to vaccinate in 
#'
#' @export
#'
#' @examples
#' f = system.file("sample_data", "g.rds", package = "SEEDNet")
#' g = readRDS(f)
#' g_comm = disnet_commuting(g)
#' nodes = igraph::vcount(g_comm)
#' set.seed(890)
#' seed_nd = igraph::vertex_attr(g_comm, "name", sample(1:nodes, 1))
#' for_sim = disnet_sim_setup(g_comm, seed_nd = seed_nd, output_dir = NA)



disnet_sim_setup = function(g,
                            r0 = 1.44,            # Pourbohloul
                            latent_period = 2.62, # Tuite
                            inf_period = 3.38,    # Tuite
                            tau = 3,              # Return rate
                            r_beta = 0.50,        # Longini 2005
                            p_a = 1/3,            # Lonigni 2005
                            seed_nd = NA, 
                            seed_no = 1,
                            vacc = FALSE,
                            output_dir = getOption("disnetOutputDir", NA),
                            vacc_eff = c(80, 90),
                            vacc_cov = c(80, 100),
                            vacc_nd = "890",
                            ...)
{
    if( is.na(seed_nd) ) stop("Please provide seed node")
    mu = 1/inf_period
    beta = (r0 * mu)/((r_beta * p_a) + (1 - p_a)) # from balcan pg 143

    # Calculate effective population and add it as a vertex attribute to graph
    g = disnet_eff_pop(g, tau)

    # Calculate sigma by tau values (to avoid recalculation each time)
    g = disnet_add_sigmas(g, tau)

    # Force of infection: May the force be with you ^^ *************************
    vert_info = disnet_vert_info(g, beta)

    # j_in information for lambda_jj part of FOI formula
    j_in = disnet_j_in(vert_info, g)

    # j_out information for lambda_ji part of FOI formula
    j_out = disnet_j_out(vert_info, g)

    # component 1 and 2 (sub)
    comp1_sub = 1/vert_info$sigma_by_tau_p1
    comp2_sub = lapply(setNames(j_in, names(j_in)), disnet_comp2_sub)

    # Defining vert_list, which contains all information needed for FOI
    vert_list = list(vert_info = vert_info,
                     comp1_sub = comp1_sub,
                     comp2_sub = comp2_sub,
                     j_in = j_in,
                     j_out = j_out)

    # Saving pre-simulation parameters
    params = list(exit_latent_I = ((1/latent_period) * (1 - p_a)),
                  exit_latent_Ia =  ((1/latent_period) * p_a),
                  mu = 1/inf_period)


    # Initializing the Time Step list and data frame ***************************
    start_TS = disnet_start_TS(g)

    # Check if vaccination is to take place and proceed accordingly ************
    if(vacc){

        # VACCINATION CAMPAIGN IS ON *******************************************
        comb = expand.grid(vacc_eff, vacc_cov)
        vacc_prop = (comb$Var1 * comb$Var2)/10000

        # Transfer effectively vaccinated folks from Susceptible to Recovered cpt
        start_TS = lapply(vacc_prop, function(vacc_prop, start_TS, vacc_nd) {
            vacc_rows = which(start_TS$name %in% vacc_nd)
            old_S = start_TS$S[ vacc_rows ]
            new_R = round(old_S * vacc_prop)
            start_TS$S[ vacc_rows ] = old_S - new_R
            start_TS$R [ vacc_rows ] = start_TS$R [ vacc_rows ] + new_R
            start_TS
        }, start_TS, vacc_nd)
        
        names(start_TS) = as.character(vacc_prop)

        # Seeding node of interest
        start_TS = lapply(start_TS, function(start_TS, seed_nd, seed_no) {
            disnet_seed_nd(start_TS, seed_nd, seed_no)
        }, seed_nd, seed_no)

        # calculate starting foi
        start_TS = lapply(start_TS, function(start_TS, vert_list, j_out) {
            start_TS$foi = disnet_foi(start_TS, vert_list, j_out)
            start_TS
        }, vert_list, j_out)

        # Saving pre-simulation scaffold
        sim_input = lapply(start_TS, function(start_TS, ...) {
            list(start_TS = start_TS,
                 vert_list = vert_list,
                 j_out = j_out,
                 params = params)
        }, vert_list, j_out, params)

        names(sim_input) = names(start_TS)
        
    } else {

        # NO VACCINATION UNDERTAKEN ********************************************
        # Seeding node of interest
        start_TS = disnet_seed_nd(start_TS, seed_nd, seed_no)

        start_TS$foi = disnet_foi(start_TS, vert_list, j_out)

        # Saving pre-simulation scaffold
        sim_input = list(
                        start_TS = start_TS,
                        vert_list = vert_list,
                        j_out = j_out,
                        params = params)

        # if( !dir.exists(dir.save) ) dir.create(dir.save)

        # sim_input_name = sprintf("/intermed_seed-%s-in-%s.RDS",
        # seed_no,
        # paste0(seed_nd, collapse = "-"))
        # saveRDS(sim_input, paste0(dir.save, sim_input_name))
    }
    
    if(length(output_dir) > 0 && !is.na(output_dir)) {

        if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

        if(vacc) {
            vacc_prop = gsub("\\.", "p", names(sim_input))
            f = file.path(output_dir, sprintf("%d_sim-setup_seed-%s_vacc-%s-%s.RDS",
                                              seq_along(sim_input),
                                              paste0(seed_nd, collapse = "-"),
                                              paste0(vacc_nd, collapse = "-"),
                                              vacc_prop))
            
            lapply(seq_along(sim_input), function(n, sim_input, f, ...)
            {
                sim_input = sim_input[[n]]
                saveRDS(sim_input, f[[n]])
            }, sim_input, f)
        } else {
            f = file.path(output_dir,
                          sprintf("sim-setup_seed-%s-in-%s.RDS",
                                  seed_no,
                                  paste0(seed_nd, collapse = "-")))
            saveRDS(sim_input, f)
        }
        f
    } else {
        sim_input
    }
}




# ==============================================================================
# for code testing
# ==============================================================================
# VACCINATION: NO
if(FALSE){
    r0 = 1.44            # Pourbohloul
    latent_period = 2.62 # Tuite
    inf_period = 3.38    # Tuite
    tau = 3              # Return rate
    r_beta = 0.50        # Longini 2005
    p_a = 1/3            # Lonigni 2005
    # seed_nd = "885"
    seed_no = 1
    vacc = FALSE
    output_dir = NA
}


# VACCINATION: YES
if(FALSE){
    r0 = 1.44            # Pourbohloul
    latent_period = 2.62 # Tuite
    inf_period = 3.38    # Tuite
    tau = 3              # Return rate
    r_beta = 0.50        # Longini 2005
    p_a = 1/3            # Lonigni 2005
    # seed_nd = "885"
    seed_no = 1
    vacc = TRUE
    output_dir = getOption("disnetOutputDir", "disnet_output_dir")
    # vacc_eff = c(50, 60, 70, 80, 90)
    # vacc_cov = c(20, 40, 60, 80, 100)
    vacc_eff = c(80, 90)
    vacc_cov = c(80, 100)
    vacc_nd = "890"
}
