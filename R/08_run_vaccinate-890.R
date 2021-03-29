# ==============================================================================
#
# Simulating different vaccination scenarios
#
# Starting in node 890
#
# ***Running the simulations***
# 
# ==============================================================================

# Libraries
# ------------------------------------------------------------------------------
library(igraph)
library(dplyr)
library(future.apply)


# * Source code
# ==============================================================================
invisible(lapply(list.files("R/SEEDNet", full = TRUE), source))
# OR
# devtools::install_github("nistara/SEEDNet@v0.1")
# library(SEEDNet)

# ** set up future_lapply for parallelizing simulation runs
# ------------------------------------------------------------------------------
future::availableCores()
plan(multiprocess, workers = 7) 
parallel = FALSE

# Set number of simulations and timesteps---------------------------------------
nsims = 500
nsteps = 1000


# * Read in saved sim-setup files, specify simulation parameters, and run sims
# ==============================================================================
files = list.files("data/pre-sim/890/vaccinations_890", full.names=TRUE)


if(FALSE) {

    # Break vacc files into 2 groups
    bk = files
    files = bk[ 1:13 ]

    files = bk[ 14:25 ]

}


# Run the simulations over each vaccination scenario----------------------------
lapply(files, function(f, nsims, nsteps, parallel) {

    # Read in for_sim file
    for_sim = readRDS(f)

    # Set directory to save sim results in
    fname = basename(f)
    fname = gsub("sim-setup_|.RDS", "", fname)
    sim_output_dir = sprintf("data/simulations/890/vaccinations_890/%s", fname)

    if( !dir.exists(sim_output_dir )) dir.create(sim_output_dir, recursive=TRUE)

    # Run simulations
    disnet_simulate(sim_input = for_sim,
                    sim_output_dir = sim_output_dir,
                    nsims = nsims,
                    nsteps = nsteps,
                    parallel = parallel)

    # garbage clean to clear memory/space
    gc()
    
}, nsims, nsteps, parallel)






