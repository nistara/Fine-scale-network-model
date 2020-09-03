# ==============================================================================
# Place for miscellaneous code used in the project
# ==============================================================================


# * Create sample network
# ==============================================================================
out_dir = "data/sample_data/network"
if( !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

g = readRDS("data/network/rwa-net.RDS")
subset_sizes = c(3, 5, 10, 100, 200)

lapply(subset_sizes, function(n, g, out_dir) {
    g_subset = igraph::induced.subgraph(g, c("890", sample(1:1000, n - 1)))
    g_out = file.path(out_dir, paste0("g_", n, ".RDS"))
    saveRDS(g_subset, g_out)
}, g, out_dir)

# ** A graph subset without the most populous node, 890
g_no890 = igraph::induced.subgraph(g, c("91", sample(1:500, 4)))
g_out = file.path(out_dir, paste0("g_no890", ".RDS"))
saveRDS(g_no890, g_out)
