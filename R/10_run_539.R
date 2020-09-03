# ==============================================================================
#
# Simulating different scenarios with outbreaks starting from 539
#
# ==============================================================================

# Libraries
# ------------------------------------------------------------------------------
library(igraph)
library(dplyr)
library(future.apply)


# * Source code
# ==============================================================================
invisible(lapply(list.files("R/disnet", full = TRUE), source))


# ** set up future_lapply for parallelizing simulation runs
# ------------------------------------------------------------------------------
future::availableCores()
plan(multiprocess, workers = 3) 
parallel = FALSE

# Set number of simulations and timesteps---------------------------------------
nsims = 500
nsteps = 1000


# * Read in saved sim-setup files, specify simulation parameters, and run sims
# ==============================================================================
in_dir = "data/pre-sim/539"
files = list.files(in_dir, full.names = TRUE, recursive = TRUE)


# Run the simulations over each vaccination scenario----------------------------
lapply(files, function(f, nsims, nsteps, in_dir, parallel) {

    # Read in for_sim file
    for_sim = readRDS(f)

    # Set directory to save sim results in
    fname = basename(f)
    fname = gsub("sim-setup_|.RDS", "", fname)
    base_in_dir = basename(in_dir)
    
    sim_output_dir = sprintf("data/simulations/%s/%s", base_in_dir, fname)

    if( !dir.exists(sim_output_dir )) dir.create(sim_output_dir, recursive=TRUE)

    # Run simulations
    disnet_simulate(sim_input = for_sim,
                    sim_output_dir = sim_output_dir,
                    nsims = nsims,
                    nsteps = nsteps,
                    parallel = parallel)

    # garbage clean to clear memory/space
    gc()
    
}, nsims, nsteps, in_dir, parallel)

