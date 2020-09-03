
# References
# ------------------------------------------------------------------------------
# For jittering map data: https://stackoverflow.com/a/27623689/5443003


# Workspace
# ------------------------------------------------------------------------------
library(gtools)
library(doBy)
library(dplyr)
library(sf)
library(raster)
library(igraph)
library(RColorBrewer)


# Import data
# ------------------------------------------------------------------------------
df = readRDS("results/output/890/500-sims_seed-1-in-nd890/nd-inf-info_500-sims_seed-1-in-nd890.RDS")

g = readRDS("data/network/rwa-net.RDS")

# Get vert info
verts = igraph::as_data_frame(g, "vertices")


# Add vert info to infection df
df = inner_join(df, verts[ , c("name", "lat", "lon", "pop")], by = "name")


# Get Rwanda polygon map
# ------------------------------------------------------------------------------
tmpdir = tempdir()
url = "http://www.maplibrary.org/library/stacks/Africa/Rwanda/RWA_outline_SHP.zip"
file = file.path(tmpdir, basename(url))
download.file(url, file)
unzip(file, exdir = tmpdir)

map = st_read(paste0(tmpdir, "/RWA_outline.shp"), crs = 4326)

m = map$geometry %>%
    st_cast("MULTILINESTRING") %>%
    st_cast("LINESTRING")
    




# ------------------------------------------------------------------------------
# Jitter nodes by av_inf
# ------------------------------------------------------------------------------
p = st_as_sf(verts[ , c("name", "lon", "lat")],
             coords = c("lon", "lat"), crs=4326)

t = as.numeric(st_distance(p, m))/1000
t13 = t >= 13

df_in = verts$name[ t13 ]
df_out = verts$name[ !t13 ]


inf_raster_map = function(df, m, df_in, df_out) {
    
    data1 = splitstackshape::expandRows(df[ df$name %in% df_in, ],
                                        "av_inf", drop = FALSE)
    data1$lon = rnorm(nrow(data1), data1$lon, (log(data1$pop)/500)/1.96)
    data1$lat = rnorm(nrow(data1), data1$lat, (log(data1$pop)/500)/1.96)

    data2 = splitstackshape::expandRows(df[ df$name %in% df_out, ],
                                        "av_inf", drop = FALSE)
    data2j = data2
    data2j$lon = rnorm(nrow(data2), data2$lon, (log(data2$pop)/500)/1.96)
    data2j$lat = rnorm(nrow(data2), data2$lat, (log(data2$pop)/500)/1.96)

    p = st_as_sf(data2j[ , c("lon", "lat")], coords = c("lon", "lat"), crs=4326) 
    tt = st_join(p, map)

    idx = is.na(tt$OBJECTID)

    while(sum(idx)>0){ # redo for points outside polygon
        data2j$lon[idx] = rnorm(sum(idx),
                                 data2$lon[idx],
                                 (log(data2$pop[idx])/500)/1.96)
        data2j$lat[idx] = rnorm(sum(idx),
                                 data2$lat[idx],
                                 (log(data2$pop[idx])/500)/1.96)
        p = st_as_sf(data2j[ , c("lon", "lat")], coords = c("lon", "lat"), crs=4326) 
        tt = st_join(p, map)
        idx = is.na(tt$OBJECTID)
        print(table(idx))
    }

    print("rbinding")
    dmap = rbind(data1, data2j)
    rownames(dmap) = NULL

    dmap
}


ldmap = inf_raster_map(df, m, df_in, df_out)

dpwgs = st_as_sf(ldmap[ , c("name", "av_inf", "lon", "lat")],
                 coords = c("lon", "lat"), crs = 4326)


dp = st_transform(dpwgs, 32735)

raster_template = raster(extent(dp), resolution = 250,
                         crs = crs(dp))


r2 = rasterize(dp, raster_template, field = 1, fun = "count")

r3 = projectRaster(r2, crs="+proj=longlat +datum=WGS84 +no_defs")

r3@data@values[is.na(r3@data@values)] = 0
r3v = getValues(r3)

rbin = cut(r3v, breaks = c(-Inf,
                           0, 
                           10,     
                           100,    
                           500,    
                           1000,   
                           5000),
           labels = 1:6)

r3@data@values = as.numeric(as.character(rbin))

r4 = raster::crop(r3, map)
r5 = raster::mask(r4, map)

col_no = unique(r5@data@values)

if(FALSE) {
    table(r5@data@values, useNA="ifany")
}


# ------------------------------------------------------------------------------
# Plot heatmap
# ------------------------------------------------------------------------------

# Assign color palette
color = c("#313695", # dark blue
          "#2c7bb6",
          "#abd9e9",
          "#ffffbf",
          "#fdae61",
          "#d7191c")

# Plot
pdf("results/figs/R_inf-plot-1.pdf")
plot(map$geometry, col = "#313695", border = "#313695")
plot(r5, col = color, axes = FALSE, box = FALSE, add = TRUE, legend = FALSE)
dev.off()



# Other plots
if(FALSE) {
    par(mfrow = c(1,2))
    plot(seq_along(color), bg = color, pch = 21)
    plot(rw_poly_0, col = "#313695", border = "#313695")
    plot(r5, col=color, axes=FALSE, box=FALSE, add = TRUE, legend = FALSE)
    dev.off()


    par(mfrow = c(2,4), mar = rep(1, 4))
    for(x in seq_along(col_no))
    {
        print(col_no[x])
        rr = r5
        rr@data@values = ifelse(rr@data@values %in% col_no[x], 1, NA)
        plot(rr, col = "blue", main = col_no[x], legend=FALSE, frame.plot = FALSE,
             # bty = "n",
             box = FALSE,
             axes = 0)
    }
    dev.off()
}


