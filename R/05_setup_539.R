# ==============================================================================
#
# Set up the pre-simulation network and data for outbreaks starting from 539
# Note: 539 is the node for Kibungo
#
# ==============================================================================


# Libraries
# -----------------------------------------------------------------------------
library(igraph)
library(dplyr)


# * Source code
# ==============================================================================
invisible(lapply(list.files("R/SEEDNet", full = TRUE), source))
# OR
# devtools::install_github("nistara/SEEDNet@v0.1")
# library(SEEDNet)


# * Disease parameters
# ==============================================================================
source("R/00_disease-parameters.R", echo = TRUE)

seed_nd = "539"

params = list(r0 = r0,
              latent_period = latent_period,
              inf_period = inf_period,
              tau = tau,
              r_beta = r_beta,
              p_a = p_a,
              vacc = FALSE,
              seed_nd = seed_nd,
              seed_no = 1)




# * Set up the pre-simulation network
# ==============================================================================

# ** Read in commuting data-----------------------------------------------------
g_comm = readRDS("data/commuting/rwa-commuting.RDS")

# ** Set output dir-------------------------------------------------------------
output_dir = paste0("data/pre-sim/", seed_nd)

# ** Set up the pre-sim file----------------------------------------------------
disnet_sim_setup(g_comm,
                 r0 = params$r0,
                 latent_period = params$latent_period,
                 inf_period = params$inf_period,
                 tau = params$tau,
                 r_beta = params$r_beta,
                 p_a = params$p_a,
                 vacc = params$vacc,
                 seed_nd = params$seed_nd,
                 seed_no = params$seed_no,
                 output_dir = output_dir)


    
# ==============================================================================
# * Set up vaccination scenarios
# ==============================================================================

vacc_eff = 80
vacc_cov = 60

vacc_nds = list(c("539", "890"), "539", "890")

lapply(seq_along(vacc_nds), function(n, params, vacc_nds, vacc_eff, vacc_cov, g_comm) {

    params$vacc = TRUE
    params$vacc_nd = vacc_nds[[ n ]]
    params$vacc_eff = vacc_eff
    params$vacc_cov = vacc_cov

    output_dir = paste0("data/pre-sim/", params$seed_nd)

    disnet_sim_setup(g_comm,
                     r0 = params$r0,
                     latent_period = params$latent_period,
                     inf_period = params$inf_period,
                     tau = params$tau,
                     r_beta = params$r_beta,
                     p_a = params$p_a,
                     seed_nd = params$seed_nd,
                     seed_no = params$seed_no,
                     vacc = params$vacc,
                     vacc_nd = params$vacc_nd,
                     vacc_eff = params$vacc_eff,
                     vacc_cov = params$vacc_cov,
                     output_dir = output_dir)

}, params, vacc_nds, vacc_eff, vacc_cov, g_comm)


