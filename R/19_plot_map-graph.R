# Ref
# ------------------------------------------------------------------------------
# http://curleylab.psych.columbia.edu/netviz/netviz1.html#/
# http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/
# https://briatte.github.io/ggnet/#edge-arrows

# Libraries
# -----------------------------------------------------------------------------
library(igraph)
library(dplyr)
library(gtools)
library(maps)
library(mapdata)
library(maptools)
library(raster)
library(ggnet)
library(Hmisc)

library(GGally)
library(intergraph)
library("RColorBrewer")


# Read graph with commuting info
g = readRDS("data/commuting/rwa-commuting.RDS")

edges = igraph::as_data_frame(g, "edges")
edges$comm_people = edges$pop_from * edges$commuting_prop
min1_comm = which(edges$comm_people >= 1)

g <- subgraph.edges(g, min1_comm)

verts = igraph::as_data_frame(g, "vertices")
lo <- as.matrix(verts[,c("lon", "lat")])


# Plot all-degree
# ------------------------------------------------------------------------------
g_all = strength(g, mode = "all")
v_all = cut(g_all,c(0, 20, 40, 60, 100, 1000, 2000), include.lowest = T)
levels(v_all) = c("0-20", "21-40", "41-60", "61-100", "101-1000", "1001-2000")


pdf("results/figs/rwa-net_all-deg.pdf")

ggnet2(g, mode  = lo, 
       size = v_all,
       size.palette = c("0-20" = 0.01,
                        "21-40" = 0.2,
                        "41-60" = 0.5,
                        "61-100" = 0.7,
                        "101-1000" = 1.2,
                        "1001-2000" = 2),
       node.color = "#313695",
       edge.alpha = 0.3,
       arrow.size = 0,
       node.alpha = 0.5,
       size.legend = "Node degree",
       legend.position = "top")

dev.off()
    


# Plot complete graph
# ------------------------------------------------------------------------------
if(FALSE){
    
    pdf("results/figs/rwa-net_all.pdf")

    ggnet2(g, mode  = lo, 
           size = 1,
           node.color = "darkgreen",
           edge.alpha = 0.3,
           arrow.size = 0,
           node.alpha = 0.5)

    dev.off()

}



# Plot in-degree
# ------------------------------------------------------------------------------
g_in = strength(g, mode = "in")
# quantile(g_in)
v_in = cut(g_in,c(0, 20, 40, 60, 100, 1000, 2000), include.lowest = T)
# sum(table(v_in))
# table(v_in)
levels(v_in) = c("0-20", "21-40", "41-60", "61-100", "101-1000", "1001-2000")

if(FALSE){

    pdf("results/figs/rwa-net_in-deg.pdf")
    
    ggnet2(g, mode  = lo, 
           size = v_in,
           size.palette = c("0-20" = 0.01,
                            "21-40" = 0.2,
                            "41-60" = 0.5,
                            "61-100" = 0.7,
                            "101-1000" = 1.2,
                            "1001-2000" = 2),
           node.color = "darkgreen",
           edge.alpha = 0.3,
           arrow.size = 0,
           node.alpha = 0.5,
           size.legend = "Node in-degree",
           legend.position = "top")

    dev.off()
    
}



# out degree plotting
# ------------------------------------------------------------------------------

g_out = strength(g, mode = "out")
summary(g_out)
v_out = cut(g_out, c(0, 20, 40, 60, 100, 1000, 2000), include.lowest = T)
sum(table(v_out))
table(v_out)
levels(v_out) = c("0-20", "21-40", "41-60", "61-100", "101-1000", "1001-2000")

if(FALSE){

    pdf("results/figs/rwa-net_out-deg.pdf")

    ggnet2(g, mode  = lo,
           size = v_out,
           size.palette = c("0-20" = 0.01,
                            "21-40" = 0.2,
                            "41-60" = 0.5,
                            "61-100" = 0.7,
                            "101-1000" = 1.2,
                                "1001-2000" = 2),
           node.color = "darkgreen",
           edge.alpha = 0.3,
           arrow.size = 0,
           node.alpha = 0.5,
           size.legend = "Node out-degree",
           legend.position = "top") 

    dev.off()

}


