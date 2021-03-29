#' Function to map the network
#'
#' This function creates a leaflet map of the network, and highlights nodes
#' of interest specified by the user.
#'
#' @param g The network to be mapped.
#' @param nodes The nodes to be highlighted (in red vs. the default blue).
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' f = system.file("sample_data", "g.rds", package = "SEEDNet")
#' g = readRDS(f)
#' map = map_g(g)

#' @export

# ------------------------------------------------------------------------------
# *** Map network vertices, and highlight user-defined nodes of interest
# ------------------------------------------------------------------------------
map_g = function(g, nodes = NULL) {
    # browser()
    net_verts = igraph::as_data_frame(g, "vertices")
    map = leaflet::leaflet() %>%
        leaflet::addProviderTiles("Esri.NatGeoWorldMap", group = "Esri.NatGeoWorldMap") %>%
        leaflet::addCircleMarkers(data = net_verts, ~lon, ~lat,
                         radius = 5,
                         stroke = FALSE,
                         fillOpacity = 0.5,
                         popup = paste("name:", net_verts$name,
                                       "<br> pop:", net_verts$pop,
                                       "<br> lat:", net_verts$lat,
                                       "<br> lon:", net_verts$lon))
    if(!is.null(nodes)) {
        df = net_verts[ net_verts$name %in% nodes, ]
        map = map %>%   
        leaflet::addCircleMarkers(data = df, ~lon, ~lat,
                         radius = 5,
                         stroke = FALSE,
                         color= "red",
                         fillOpacity = 0.7,
                         popup = paste("name:", df$name,
                                       "<br> pop:", df$pop,
                                       "<br> lat:", df$lat,
                                       "<br> lon:", df$lon))
    }
    map
}

        


