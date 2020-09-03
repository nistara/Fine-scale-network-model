################################################################################
## This code:
## 1. Reads in an igraph object (with commuting proportions previously
##      calculated)
## 
## 2. Calculates and attaches  "Effective Population" to graph
##
## 3. Seeds a node with specified number of infectious individuals (taking them
##      out from the Susceptibles and adding to Infectious compartment
##
## 4. Creates the FOI structure/scaffold used to quickly calculate FOI
##      (FOI = Force of Infection, acting on an individual in a node)
##
## 5. Simulates simplified SEIR models
################################################################################



# Libraries
# ==============================================================================
library(igraph)
library(dplyr)


# ==============================================================================
# * Source code
# ==============================================================================
invisible(lapply(list.files("R/disnet", full = TRUE), source))


# ==============================================================================
# * Read in commuting data
# ==============================================================================
g_comm = readRDS("data/commuting/rwa-commuting.RDS")


# ==============================================================================
# * Disease parameters
# ==============================================================================
source("R/00_disease-parameters.R", echo = TRUE)
       
seed_nd = "890"

params = list(r0 = r0,
              latent_period = latent_period,
              inf_period = inf_period,
              tau = tau,
              r_beta = r_beta,
              p_a = p_a,
              vacc = FALSE,
              seed_nd = seed_nd,
              seed_no = 1)



# ==============================================================================
# * Set up the pre-simulation network and data
# ==============================================================================

output_dir = "data/pre-sim/890"
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

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
vacc_eff = c(50, 60, 70, 80, 90)
vacc_cov = c(20, 40, 60, 80, 100)

if(FALSE) {
    comb = expand.grid(vacc_eff, vacc_cov)
    # Resultant file names correspond to the effectively vaccinated proportions
    vacc_prop = (comb$Var1 * comb$Var2)/10000
}

seed_nd = "890"
vacc_nd = "890"

params$vacc = TRUE
params$vacc_nd = vacc_nd
params$vacc_eff = vacc_eff
params$vacc_cov = vacc_cov


# ** Set output dir----------------------------------------------------------
out_dir = "data/pre-sim/890/vaccinations_890"
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ** Create sim setup files-----------------------------------------------------
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
                 output_dir = out_dir)
