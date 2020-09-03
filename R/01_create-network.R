# ==============================================================================
# CONVERT THE ARCGIS NETWORK DATA TO A GRAPHML (NETWORK) OBJECT
# ==============================================================================


# Libraries
# ==============================================================================
library(igraph)
library(intergraph)
library(leaflet)
library(htmlwidgets)


# * Load data
# ==============================================================================

matrix_files = list.files("data/arcgis-export/2016-07-14_Network/",
                          pattern = "matrix_", full.names = TRUE)

net_matrix = do.call(rbind,
                     lapply(matrix_files, read.csv, stringsAsFactors = FALSE))

head(net_matrix)

net_points = read.table("data/arcgis-export/2016-07-14_Network/points.txt",
                        header = TRUE, sep=',', stringsAsFactors = FALSE)

head(net_points)
names(net_points)

net_points = net_points[ , c("FID", "NEAR_X", "NEAR_Y", "SUM")]
head(net_points)

# create backup in case its needed in this code session
mat = net_matrix
pnts = net_points

net_matrix = mat


# * Edit network matrix
# ==============================================================================
head(net_matrix)

# Choose OriginID, DestinationID and Total_Length (distance between points)
net_matrix = net_matrix[ , c("OriginID", "DestinationID", "Total_Length")]

# Subtract 1 from the Origin and Destination, so that they match the FID (original IDs
# for the intersection points
net_matrix[ , c("OriginID", "DestinationID")] = net_matrix[ , c("OriginID", "DestinationID")] - 1

# Remove same location edes, e.g. Origin = 1 and Destination = 1
net_matrix = net_matrix[ !(net_matrix$OriginID == net_matrix$DestinationID), ] 

# Converting the Origin and Destination IDs to character, for igraph
# Ref: https://sites.google.com/site/daishizuka/toolkits/sna/weighted-edgelists
net_matrix[ , "OriginID"] = as.character(net_matrix[ , "OriginID"])
net_matrix[ , "DestinationID"] = as.character(net_matrix[ , "DestinationID"])
str(net_matrix)



# Attribute dataframe -----------------------------------------------------
# Create attribute dataframe to merge with network object
attrs = data.frame(name = as.character(net_points$FID), pop = net_points$SUM,
                   lat = net_points$NEAR_Y, lon = net_points$NEAR_X,
                   stringsAsFactors = FALSE)



# Create network object ---------------------------------------------------
net = graph.data.frame(net_matrix, directed=TRUE) 
net



# Add vertex attributes ---------------------------------------------------
# Get vertex names from created graph
verts = data.frame(name = vertex_attr(net)[[1]], stringsAsFactors = FALSE)
dim(verts)


# Merge with attribute dataframe and order
verts = merge(verts, attrs, by = "name")
verts = verts[ order(as.numeric(verts$name)), ]


# Add attributes to graph
vertex_attr(net) = list(name = verts$name, pop = verts$pop, lat = verts$lat, lon = verts$lon)


# Check added attributes
head(vertex_attr(net)[[1]])
head(vertex_attr(net)[[2]])
head(vertex_attr(net)[[3]])
head(vertex_attr(net)[[4]])


# IMP Checking edges of particular node -----------------------------------
length(E(net)[ from("67") ])


# * Export created network
# ==============================================================================
out_dir = "data/network/"
if( !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

saveRDS(net, paste0(out_dir, "rwa-net.RDS"))


# * Create leaflet map
# ==============================================================================

create_map = FALSE

if(create_map) {
    
    # Create dataframe for leaflet map
    # ------------------------------------------------------------------------------
    chk_net = data.frame(name = vertex_attr(net)[[1]], pop = vertex_attr(net)[[2]],
                         lat = vertex_attr(net)[[3]], lon = vertex_attr(net)[[4]],
                         stringsAsFactors = FALSE)

    chk_net = chk_net[order(chk_net$name), ]
    head(chk_net)

    map_full = leaflet() %>%
        addTiles() %>%
        addCircleMarkers(data = chk_net, 
                         radius = log(chk_net$pop)/2, 
                         popup = paste("name:", chk_net$name,
                                       "<br> pop:", chk_net$pop,
                                       "<br> lat:", chk_net$lat,
                                       "<br> lon:", chk_net$lon))

    map_full

    # OR

    # Read in graph file created above and create leaflet map from it
    # --------------------------------------------------------------------------

    net = readRDS("data/network/rwa-net.RDS")

    dfv = igraph::as_data_frame(net, "vertices")

    map = leaflet() %>%
        addTiles() %>%
        addCircleMarkers(data = dfv,
                         radius = log(dfv$pop)/2,
                         popup = paste("name:", dfv$name,
                                       "<br> pop:", dfv$pop,
                                       "<br> lat:", dfv$lat,
                                       "<br> lon:", dfv$lon))

    df_sim = dfv[ dfv$name %in% c("890", "1239", "539"), ]
    
    map = map %>% 
                addCircleMarkers(data = df_sim,
                                 radius = log(df_sim$pop)/2,
                                 popup = paste("name:", df_sim$name,
                                               "<br> pop:", df_sim$pop,
                                               "<br> lat:", df_sim$lat,
                                               "<br> lon:", df_sim$lon),
                                 color = "red")
    map

    
    # Save map
    # ------------------------------------------------------------------------------
    # Ref: https://github.com/ramnathv/htmlwidgets/issues/299#issuecomment-375058928
    
    saveWidgetFix = function (widget,file,...) {
        ## A wrapper to saveWidget which compensates for arguable BUG in
        ## saveWidget which requires `file` to be in current working
        ## directory.
        wd = getwd()
        on.exit(setwd(wd))
        outDir = dirname(file)
        file = basename(file)
        setwd(outDir);
        saveWidget(widget,file = file,...)
    }


    map_out_dir = "results/leaflet-maps/"
    if( !dir.exists(map_out_dir)) dir.create(map_out_dir, recursive = TRUE)

    saveWidgetFix(map, file.path(map_out_dir, "rwa-net.html"))

}

    
    
    
    
