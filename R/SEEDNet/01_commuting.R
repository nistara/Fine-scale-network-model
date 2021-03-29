# ==============================================================================
# This code calculates the commuting rates between nodes in the graph
#
# There are 1606 nodes, and 2577630 edges (after removing the self edges:
#      (1606 * 1606) - 1606 = 2577630
#      NOTE: they go from 0-1605, not 1-1606
#            the matrix is arranged from smallest distance to largest one
# ==============================================================================


# Functions for commuting rate (removing nodes with < 10 people)
# ==============================================================================

#' Commuting function
#'
#' `disnet_commuting` gets the `from` nodes, and sends them as the parameter for
#' the disnet_comm2 along with the entire dataframe `nd_edges`.
#' It incoporates both the individual and all nodes' functions.
#' Note: it removes nodes with less than 10 people in them. 
#'
#' @param g The `graphml` (network) object for which to calculate commuting rates
#' @param N_c The commuting proportion of the entire population. Default = 0.11 from
#' Simini
#'
#' @examples
#' f = system.file("sample_data", "g.rds", package = "SEEDNet")
#' g = readRDS(f)
#' disnet_commuting(g)
#' @export

disnet_commuting = function(g, N_c = 0.11, round_sigma = FALSE, ...)
{
    comm_info = disnet_comm3(g)
    g_edges = comm_info$g_edges
    i = unique(g_edges$from)
    cr = lapply(i, disnet_comm2, g_edges, N_c)
    g_edges$commuting_prop = do.call(rbind, cr)

    verts_info = comm_info$g_verts

    # Calculating Sigma (total commuting prop for each node
    verts_info$sigma = aggregate(commuting_prop ~ as.numeric(g_edges$from),
                                 data = g_edges, FUN = sum)[["V1"]]

    if( round_sigma ) {
        # rounding commuting proportion to 2 decimal places
        verts_info$sigma = round(verts_info$sigma, 2)
    }

    ## Create graph file
    g_comm = igraph::graph_from_data_frame(g_edges,
                                           directed = TRUE,
                                           vertices = verts_info)

    if(FALSE){
        # Check that the info is correct in the created graph:
        g_edges2 = igraph::as_data_frame(g_comm, what = "both")

        # Checking Kigali, the largest node, named "890". Its pop match Kigali's
        g_edges2$vertices[ g_edges2$vertices$name == "890", ]

        # head and tail, to make sure the names corroborate with the graph vertices
        head(g_edges2$vertices, 10)
        tail(g_edges2$vertices, 10)
        head(g_edges2$edges)
        head(g_edges)

        # Save the graph
        write.graph(g_comm, paste0("data/graphml/", Sys.Date(), "_graph-commuting.graphml"),
                    format = "graphml")

    }

    g_comm
}

# ==============================================================================

#' disnet_comm1
#'
#' gets commuting rate for each individual node (hence it's for individual nodes)
#' Considers a commuting proportion of 11% for the entire population
#' @param i the node for which commuting proportion is being calculated
#' @param edges_subset subset of `nd_edges` with all outgoing edges of `i`
#' @param j all the nodes `i` is connected to
#' @param N_c The commuting proportion of the entire population. Default = 0.11 from
#' Simini
#' 
#' @export

disnet_comm1 = function(j, edges_subset, i, N_c) {
    radius = edges_subset$Total_Length[ edges_subset$to %in% j]
    df_radius = edges_subset[ edges_subset$Total_Length <= radius, ]
    m_i = df_radius$pop_from[1]
    n_j = df_radius$pop_to[ df_radius$to %in% j]
    s_ij = sum(df_radius$pop_to[ df_radius$to != j])
    # N_c = 0.11 # using 0.11 as commuting proportion from Simini paper
    N = 1 # to get 0.11 as proportion
    T_i = (N_c/N) # want only rate, not num people. original:  m_i * (N_c/N)
    T_ij = T_i * ( (m_i * n_j) / ((m_i + s_ij) * (m_i + n_j + s_ij)) )
    T_ij
}

# ==============================================================================

#' disnet_comm2
#'
#' Gets commuting rate for all nodes (includes individual node function)
#'
#' @param i id of the `from` node, for which we want to calculate commuting
#' proportion
#' @param test_edges the dataframe of the edges which contains the distances
#' between the nodes and the population of each edges's
#                   `from` and `to nodes.
#' @param N_c The commuting proportion of the entire population. Default = 0.11 from
#' Simini
#' 
#' @export

disnet_comm2 = function(i, test_edges, N_c) {
    edges_subset = test_edges[ test_edges$from %in% i, ]
    all_j = edges_subset$to
    print(paste0("working on node: ", i))

    comm_rate = lapply(all_j, disnet_comm1, edges_subset, i, N_c)
    comm_rate = do.call(rbind, comm_rate)
    return(comm_rate)
}

# ==============================================================================

#' disnet_comm3
#'
#' Preps the incoming graph for commuting rate calculation
#'
#' @param g graph whose commuting rate needs to be calculated
#' 
#' @export

disnet_comm3 = function(g){
    
    # Convert graphml object to dataframe for further manipulation
    df = igraph::as_data_frame(g, what = "both")

    # Node information
    g_verts = df$vertices

    # Remove nodes with less than 10 people in them
    # sum(g_verts$pop < 10)
    g_verts = g_verts[ !g_verts$pop < 10, ]
    nodes = unique(g_verts$name)           #unique nodes

    # Edges
    g_edges = df$edges[ df$edges$from %in% nodes & df$edges$to %in% nodes, ] 

    # Adding population data to edges
    # -----------------------------------------------------------------------------
    pop_from = dplyr::inner_join(g_edges, g_verts, by = c("from" = "name"))["pop"]
    g_edges$pop_from = pop_from$pop

    pop_to = dplyr::inner_join(g_edges, g_verts, by = c("to" = "name"))["pop"]
    g_edges$pop_to = pop_to$pop
    list(g_verts = g_verts, g_edges = g_edges)
}



# *** Take two
# THIS DOESN'T GIVE ACCURATE ANSWERS!!! DO NOT USE IT!!!
# Till it's fixed
# doubled checked its results with the test code in flu-net, and it had some right
# but not others. hence it's unreliable
disnet_commuting2 = function(g, N_c = 0.11)
{
    comm_info = disnet_comm3(g)
    g_edges = comm_info$g_edges
    verts_info = comm_info$g_verts
    from = factor(g_edges$from, levels = unique(g_edges$from))

    edges_from = split(g_edges, from)

    edges_from_to = lapply(edges_from, function(edges_from)
    {
        radii = edges_from$Total_Length
        lapply(seq_along(radii), function(i, df, radii)
        {
            df = df[ df$Total_Length <= radii[i], ]
        }, edges_from, radii)
    })

    cr = lapply(seq_along(edges_from_to), function(i, edges_from_to, edges_from)
    {
        df_radius = edges_from_to[[i]]
        m_i = edges_from[[i]]$pop_from
        n_j = edges_from[[i]]$pop_to
        # N_c = 0.11 # using 0.11 as commuting proportion from Simini paper
        N = 1 # to get 0.11 as proportion
        T_i = (N_c/N) # want only rate, not num people. original:  m_i * (N_c/N)
        
        s_ij = do.call(rbind, lapply(df_radius, function(df_radius)
        {
            sum(df_radius$pop_to) - df_radius$pop_to[1]
        }))

        T_ij = T_i * ( (m_i * n_j) / ((m_i + s_ij) * (m_i + n_j + s_ij)) )
        T_ij
    }, edges_from_to, edges_from)
    

    g_edges$commuting_prop = do.call(rbind, cr)



    # Calculating Sigma (total commuting prop for each node
    verts_info$sigma = aggregate(commuting_prop ~ as.numeric(g_edges$from),
                                 data = g_edges, FUN = sum)[["V1"]]

    # rounding commuting proportion to 2 decimal places
    # verts_info$sigma = round(verts_info$sigma, 2)

    ## Create graph file
    g_comm = igraph::graph_from_data_frame(g_edges,
                                           directed = TRUE,
                                           vertices = verts_info)

    if(FALSE){
        # Check that the info is correct in the created graph:
        g_edges2 = igraph::as_data_frame(g_comm, what = "both")

        # Checking Kigali, the largest node, named "890". Its pop match Kigali's
        g_edges2$vertices[ g_edges2$vertices$name == "890", ]

        # head and tail, to make sure the names corroborate with the graph vertices
        head(g_edges2$vertices, 10)
        tail(g_edges2$vertices, 10)
        head(g_edges2$edges)
        head(g_edges)

        # Save the graph
        write.graph(g_comm, paste0("data/graphml/", Sys.Date(), "_graph-commuting.graphml"),
                    format = "graphml")

    }

    g_comm
}



    

    

