# ==============================================================================
# Simulations
# ==============================================================================

# This code reads in the prepared pre-sim file (set up for the simulation step)
# and runs the specified number of simulations (nsims) for the
# specified time steps (nsteps).
# 
# It saves the simulation results to a directory defined by the user. 


# * Libraries
# ==============================================================================
library(igraph)
library(dplyr)
library(future.apply)

# * source code
# ==============================================================================
invisible(lapply(list.files("../disnet/R", full = TRUE), source))

# Other simulation info---------------------------------------------------------
seed_inf_no = 1
seed_row = 890


# Set number of simulations and timesteps---------------------------------------
nsims = 500
nsteps = 1000


# ** set up future_lapply for parallelizing simulation runs
# ------------------------------------------------------------------------------
parallel = FALSE
future::availableCores()
plan(multiprocess, workers = 6) 



# * Read in saved sim-setup files and specify simulation parameters
# ==============================================================================
# previous  sim_input_name was "sim-input_2017-08-01_graph-commuting.RDS"
# previous as in from the dissertation code
for_sim = readRDS("data/pre-sim/890/sim-setup_seed-1-in-890.RDS")



# Set directory to save sim results in------------------------------------------
sim_output_dir = sprintf("data/simulations/890/%s-sims_seed-%s-in-nd%s/",
                         nsims, seed_inf_no, seed_row)



# * Run simulations
# ==============================================================================
disnet_simulate(sim_input = for_sim,
                sim_output_dir = sim_output_dir,
                nsims = nsims,
                nsteps = nsteps,
                parallel = parallel)


gc()



# ==============================================================================
# For rounded commuting sigmas
# ==============================================================================
round_comm = TRUE

if( round_comm ) {

    for_sim = readRDS("data/pre-sim_round/sim-setup_seed-1-in-890.RDS")

    sim_output_dir = sprintf("data/simulations_round/%s-sims_seed-%s-in-nd%s/",
                             nsims, seed_inf_no, seed_row)

    if( ! dir.exists(sim_output_dir) ) dir.create(sim_output_dir, recursive=TRUE)

    disnet_simulate(sim_input = for_sim,
                    sim_output_dir = sim_output_dir,
                    nsims = nsims,
                    nsteps = nsteps,
                    parallel = parallel)


    gc()

}



