# ==============================================================================
## This code calculates the commuting rates between nodes in the graph
##
## There are 1606 nodes, and 2577630 edges (after removing the self edges:
##      (1606 * 1606) - 1606 = 2577630
##      OR (1606 * 1605) = 2577630
##      NOTE: they go from 0-1605, not 1-1606
##            the matrix is arranged from smallest distance to largest one
# ==============================================================================
                        
library(igraph)
library(dplyr)


# source code
# ==============================================================================
invisible(lapply(list.files("../disnet/R", full = TRUE), source))


# Read in created Rwanda network graph
# ==============================================================================
g = readRDS("data/network/rwa-net.RDS")


# Calculate commuting rates over network
# ==============================================================================
comm_rate = 0.11
g_comm = disnet_commuting(g, N_c = comm_rate)


# Save new graph file
# ==============================================================================
out_dir = "data/commuting/"
if(!dir.exists(out_dir)) dir.create(out_dir)

saveRDS(g_comm, paste0(out_dir, "rwa-commuting.RDS"))
