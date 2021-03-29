# ==============================================================================
# Effective population
# ==============================================================================

# Based on the formula written for simplified model (on green page)
# -----------------------------------------------------------------

#'@export
disnet_eff_pop = function(g, tau = 3) {
    verts = igraph::as_data_frame(g, "vertices")
    edges = igraph::as_data_frame(g, "edges")
    # first component of effective pop (N_jj)
    verts$N_jj = verts$pop/(1 + (verts$sigma/tau))
    # second component of effective pop (sum N_ij)
    edges$sigma_from = dplyr::inner_join(edges, verts[ , c("name", "sigma")],
                                         by = c("from" = "name"))[["sigma"]]
    # split edges by receipient 
    to_edges = split(edges, as.numeric(edges$to))
    sum_N_ij = do.call(rbind, lapply(to_edges, disnet_eff_pop1, tau))
    # checks before merging
    # table(rownames(verts) == verts$name)
    # table(rownames(verts) == names(sum_N_ij))
    verts = dplyr::left_join(verts, sum_N_ij, by = "name")
    verts$sum_N_ij[ is.na(verts$sum_N_ij) ] = 0
    verts$eff_pop = verts$N_jj + verts$sum_N_ij
    # adding eff pop back to graph
    g = igraph::set_vertex_attr(g, "eff_pop", value = verts$eff_pop)
    return(g)
}


disnet_eff_pop1 = function(df, tau) {
    # browser()
    vert_name = df$to[1]
    N_ij = df$pop_from * ((df$commuting_prop/tau) / (1 + (df$sigma_from/tau)))
    data.frame(name = vert_name, sum_N_ij = sum(N_ij), stringsAsFactors = FALSE)
}


# ==============================================================================
# Add sigma and sigmaProp_by_tau
# ==============================================================================

#'@export
disnet_add_sigmas = function(g, tau){
    # calculate sigma_by_tau and sigmaProp_by_tau
    sigma_by_tau = igraph::vertex_attr(g, "sigma")/tau
    sigmaProp_by_tau = igraph::edge_attr(g, "commuting_prop")/tau

    # add the two as vertex and edge attribute respectively
    g = igraph::set_vertex_attr(g, "sigma_by_tau", value = sigma_by_tau)
    g = igraph::set_edge_attr(g, "sigmaProp_by_tau", value = sigmaProp_by_tau)
    return(g)
}


# ==============================================================================
# Initializing start_TS dataframe
# ==============================================================================

#'@export
disnet_start_TS = function(g){
    verts = igraph::as_data_frame(g, "vertices")
    S = round(igraph::vertex_attr(g, "pop"))
    start_TS = data.frame(name = verts$name,
                          S = S,
                          E = 0,
                          I = 0,
                          Ia = 0,
                          R = 0,
                          stringsAsFactors = FALSE)
    return(start_TS)
}


# ==============================================================================
# Seed node function
# ==============================================================================

#'@export
disnet_seed_nd = function(df, nd, inf){
    seed_row = which(df$name %in% nd)
    df[seed_row, c("S", "I")] = c(df$S[seed_row] -  inf, inf)
    return(df)
}



# ==============================================================================
# FOI Take II!
# ==============================================================================

#'@export
disnet_vert_info = function(g, beta){
    df = igraph::as_data_frame(g, "vertices")
    df = df[  , c("name", "eff_pop", "sigma_by_tau")]
    df$b_by_n = beta/df$eff_pop
    df$sigma_by_tau_p1 = df$sigma_by_tau + 1
    return(df)
}



# net_neighbors_fxn
# -----------------------------------------------------------------------------
# Gets neighbor info specific to whether the edge is incoming or outgoing
#      i.e.  2 neighbor modes: "in" or "out"
# 
# I. For incoming edges, it provides:
#    -------------------------------
#      1. name of neighbors commuting to node
#      2. sigma_by_tau_p1: sigma_by_tau + 1
#      3. sigmaProp_by_tau: proportion of neighbor pop commuting to node
#
# II. For outgoing edges, it provides:
#     -------------------------------
#      1. name of neighbors to which node is commuting to
#      2. sigmaProp_by_tau: proportion of node pop commuting to neighbor
#

#'@export
net_neighbors_fxn = function(vert, g, m, vert_info){
    neigh_name = names(igraph::neighbors(graph = g, v = vert, mode = m))
    if(length(neigh_name) != 0){
        if(m == "in"){
            from = neigh_name
            to = vert
            sigma_by_tau_p1 = vert_info$sigma_by_tau_p1[
                                  vert_info$name %in% neigh_name ]
            sigmaProp_by_tau = igraph::edge_attr(g, "sigmaProp_by_tau",
                                                 paste0(from, "|", to))
            df = data.frame(name = neigh_name,
                            sigma_by_tau_p1 = sigma_by_tau_p1,
                            sigmaProp_by_tau = sigmaProp_by_tau,
                            stringsAsFactors = FALSE)
        } else {
            from = vert
            to = neigh_name
            sigmaProp_by_tau = igraph::edge_attr(g, "sigmaProp_by_tau",
                                                 paste0(from, "|", to))
            df = data.frame(name = neigh_name,
                            sigmaProp_by_tau = sigmaProp_by_tau,
                            stringsAsFactors = FALSE)
        }   
    } else {
        df = data.frame(NULL)
    }
    return(df)
}



# j_in function (wraps up the net_neighbors_fxn for incoming edges)
# -----------------------------------------------------------------------------
#'@export
disnet_j_in = function(vert_info, g){
    j = vert_info$name
    j_in = lapply(setNames(j, j), net_neighbors_fxn, g, m = "in", vert_info)
    return(j_in)
}



# j_out function (wraps up the net_neighbors_fxn for outgoing edges)
# -----------------------------------------------------------------------------
#'@export
disnet_j_out = function(vert_info, g){
    j = vert_info$name
    j_out = lapply(setNames(j, j), net_neighbors_fxn, g, m = "out", vert_info)
    return(j_out)
}



# component 2 sub (minus I)
# -----------------------------------------------------------------------------
#'@export
disnet_comp2_sub = function(j_in){
    if(length(j_in) != 0){
        name = j_in$name
        comp2_sub = j_in$sigmaProp_by_tau/j_in$sigma_by_tau_p1
        df = data.frame(name = name,
                        comp2_sub = comp2_sub,
                        stringsAsFactors = FALSE)
    } else {
        df = data.frame(NULL)
    }
    
    return(df)
}


# component 2 with I
# -----------------------------------------------------------------------------
#'@export
# comp2_i_fxn = function(comp, vI, vIa)#, vname)
# {
#     df = (vI[comp$name] + vIa[comp$name]) * comp$comp2_sub
#     sum(df, na.rm = TRUE)
# }

# comp2_i_fxn = function(comp, vI, vIa,
#                        idx = if(length(comp) >= 3) comp[[3]] else comp$name) # , vname)
# {
#     df = (vI[idx] + vIa[idx]) * comp$comp2_sub
#     sum(df, na.rm = TRUE)
# }

#'@export
comp2_i_fxn = function(comp, vI, vIa)#, vname)
{
    # browser()
    if(nrow(comp) > 0) {
        df = (vI[comp$name] + vIa[comp$name]) * comp$comp2_sub
        sum(df, na.rm = TRUE)
    } else {
        return(0)
    }
}

# calculate l_ji part
# -----------------------------------------------------------------------------
#'@export
l_ji_fxn = function(j_out, l_in_node, idx = if(length(j_out) >= 3) j_out[[3]] else j_out$name)
{
    if(is.null(idx)) {
        0
        } else {
            local_foi = l_in_node[ idx ] # was j_out$idx  and before that was j_out$name
                 # $sigmaProp_by_tau
            df = j_out[[2]] * local_foi
            sum(df, na.rm = TRUE)
    }
}



# FOI
# ------------------------------------------------------------------------------
#'@export
disnet_foi = function(start_TS, vert_list, j_out, idx = NULL, acomp2_sub = NULL, groups = NULL, r_beta = 0.50, old = TRUE){

    # browser()

    # vert_list doesn't change across calls.
    vert_info = vert_list[[1]] # vert_info
    comp1_sub = vert_list[[2]] # comp1_sub
#Not needed if idx, acomp2_sub and groups provided.    
    comp2_sub = vert_list[[3]] # comp2_sub

if(!old) {    
    if(is.null(acomp2_sub)) 
        acomp2_sub = unlist(lapply(comp2_sub, `[[`, 2))
    if(is.null(idx)) {
        rowIds = unlist(lapply(comp2_sub, `[[`, 1))
        idx = match(rowIds, vert_info$name)    
    }
    if(is.null(groups)) {
        groups = rep(1:length(comp2_sub), sapply(comp2_sub, nrow))
    }
}
    
    #!! These change across calls.
    I = start_TS[[4]] # $I 
    Ia = start_TS[[5]] * r_beta # $Ia * r_beta 

    comp1_i = (I + Ia) * comp1_sub

    # comp2_sub doesn't change across. vert_info[[6]] and [[7]] do.
    #    doCompSum(comp2_sub, vert_info[[6]], vert_info[[7]])
    if(!old)   {
        tmp = (I[idx] + Ia[idx])*acomp2_sub
        #    comp2_i = sapply(split(tmp, groups), sum)
        comp2_i = tapply(tmp, groups, sum)    
    } else {
        comp2_i = sapply(comp2_sub, comp2_i_fxn,
                         structure(I, names = vert_info$name),  # $I
                         structure(Ia, names = vert_info$name)) #$Ia
    }
    

    # onwards to FOI
    # $b_by_n 
    l_in_node_val = (vert_info[[4]] * (comp1_i + comp2_i))/vert_info[[5]] # $sigma_by_tau_p1

    l_ji  = sapply(j_out, l_ji_fxn, l_in_node_val)

    # foi
    l_in_node_val + l_ji
}

# # Testing that the empty dfs are the same for both comp and j_in
# l = lfun(comp2_i)

# lfun = function(x) {
#     df = lapply(x, length)
#     df = do.call(rbind, df)
#     return(df)
# }

# l_j_in = lfun(j_in)
# which(l_j_in == 0) == which(l == 0)





# ==============================================================================
# Functions to transition between SEIR compartments
# ==============================================================================
S_to_E = function(S, p)
    rbinom(n = 1, size = S, prob = p)


E_to_I = function(E, p)
    rbinom(n = 1, size = E, prob = p)


I_to_R = function(I, p)
    rbinom(n = 1, size = I, prob = p)

# For transitioning to Inf or asymptomatic infectious, use rmultinom (to
# draw from a multinomial distribution
# e.g
# mapply(rmultinom,size = c(3, 7, 3, 5, 6, 1, 2),
# MoreArgs = list(n = 1, prob = c(0.2, 0.8)))

# If I don't use MoreArgs like above, I'll get an error. E.g.:
# > mapply(rmultinom,size = c(3, 7, 3, 5, 6, 1, 2), n = 1, prob = c(0.2, 0.8))
# [1] 3 7 3 5 6 1 2
# Warning message:
# In mapply(rmultinom, size = c(3, 7, 3, 5, 6, 1, 2), n = 1, prob = c(0.2,  :
# longer argument not a multiple of length of shorter


addIndices =
function(comps, names)
{
  lapply(comps, function(x) { x$idx = match(x$name, names) ; x})
}

# ==============================================================================
# Simulations ahoy
# ==============================================================================
#'@export
disnet_sim_lapply = function(sim, nsteps, start_TS, vert_list, j_out, params, sim_dir, idx = NULL, acomp2_sub = NULL, groups = NULL, ...){
    # browser()

    TS = vector("list", nsteps)
    TS_sum = matrix(NA, nrow = nsteps, ncol = 5)
    colnames(TS_sum) = c("S", "E", "I", "Ia", "R")
    prev_TS = start_TS
    n = nrow(prev_TS)    

    cat("-------------------------------------------------------\n")
    cat("******** Simulation ", sim, "\n")

    old = TRUE
    # Lift this computation out to disnet_simulate() and then pass in these as parameters/arguments. No need for rowIds or comp2_sub
    if(!old && is.null(idx)) {      # missing(idx)
        # browser()
        comp2_sub = vert_list[[3]]
        rowIds = unlist(lapply(comp2_sub, `[[`, 1))   # doesn't get used after this computation.
        groups = rep(1:length(comp2_sub), sapply(comp2_sub, nrow))
        acomp2_sub = unlist(lapply(comp2_sub, `[[`, 2))
        idx = match(rowIds, vert_list[[1]]$name)    
    }

    if(!old) {
        ##XX for each of the data frames in vert_list[[3]] and also j_out, take their names
        # and match them to vert_list[[1]]$name and store in $idx in each data frame
        #?? Assuming these indices don't change over the simulation.
        vert_list[[3]] = addIndices(vert_list[[3]], vert_list[[1]]$name)
        j_out = addIndices(j_out, vert_list[[1]]$name)
    }
    
    
    for(i in 1:nsteps){

        # browser()

        cat("\r\t\t\tTimestep: ", i, "/", nsteps, sep = "")
        
        E = rbinom(n, prev_TS$S, prev_TS$foi)
        exit_E = mapply(rmultinom, size = prev_TS$E,
                        MoreArgs = list(n = 1,
                                        prob = c(params$exit_latent_I,
                                                 params$exit_latent_Ia)))
        I = exit_E[1, ]
        Ia = exit_E[2, ]
        R_I = rbinom(n, prev_TS$I, params$mu)
        R_Ia = rbinom(n, prev_TS$Ia, params$mu)        

        new_S = prev_TS$S - E
        new_E = prev_TS$E + E - (I + Ia)
        new_I = prev_TS$I + I - R_I
        new_Ia = prev_TS$Ia + Ia - R_Ia
        new_R = prev_TS$R + R_I + R_Ia

        new_TS = data.frame(name = prev_TS$name,
                            S = new_S,
                            E = new_E,
                            I = new_I,
                            Ia = new_Ia,
                            R = new_R,
                            stringsAsFactors = FALSE)


        if((sum(new_E) + sum(new_I) + sum(new_Ia)) == 0){
            TS[[i]] = new_TS
            TS[(i + 1):nsteps] = NULL
            TS_sum[i, ] = colSums(new_TS[ , -c(1)])
            TS_sum = TS_sum[ -((i + 1):nsteps), ]
            break
        }

        if(!old) {       
            new_TS$foi = disnet_foi(new_TS, vert_list, j_out, idx, acomp2_sub, groups, old = FALSE)
        } else {       
            new_TS$foi = disnet_foi(new_TS, vert_list, j_out, old = TRUE)
        }
        
        
        TS[[i]] = new_TS
        TS_sum[i, ] = colSums(prev_TS[ , -c(1, 7)])
        prev_TS = new_TS
        
    }
    
    
    if(length(sim_dir) > 0 && !is.na(sim_dir)) {
        
        f = file.path(sim_dir, sprintf("%d%s.RDS", sim, c("", "_info")))
        saveRDS(TS, f[1])
        saveRDS(TS_sum, f[2])
        f
    } else {
        list(timeStep = TS, info = TS_sum)
    }
    
}















