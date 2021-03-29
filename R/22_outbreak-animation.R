# ==============================================================================
# * Workspace
# ==============================================================================
library(RColorBrewer)
library(doBy)
library(dplyr)
library(gtools)
library(raster)
library(sf)
library(ggmap)
library(tmap)
library(data.table)
library(tidyr)
library(hrbrthemes)
library(cowplot)

extrafont::loadfonts()

invisible(lapply(list.files("R/SEEDNet", full = TRUE), source))
# OR
# devtools::install_github("nistara/SEEDNet@v0.1")
# library(SEEDNet)


# * Read and choose the folders for evaluation
# ==============================================================================
# sim_dirs = "data/simulations/890/500-sims_seed-1-in-nd890"
sim_dirs = list.dirs("data/simulations/890")
sim_dir = sim_dirs[ grepl("seed-1-in-nd890", sim_dirs) ]

results_dir = paste0("results/animations")
if(!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

nsims = NA
nd = 890

inf_info = readRDS("results/output/890/500-sims_seed-1-in-nd890/inf-info_500-sims_seed-1-in-nd890.RDS")

sim_info = get_sim_info(sim_dir, nsims = nsims)
outbrks_info = get_outbrks_info(sim_info, inf_info)
outbrks_l = get_sim_l(outbrks_info)
mean_outbrk_l = mean(outbrks_l)

set.seed(0)
sim_n = sample(which(sim_l > (mean_outbrk_l - 10) &
                     sim_l < (mean_outbrk_l + 10)), 1)
sim_l[ sim_n ]

sim_file = file.path(sim_dir, paste0(sim_n, ".RDS"))
sim = readRDS(sim_file)



# Get Rwanda polygon map
# ------------------------------------------------------------------------------
tmpdir = tempdir()
url = "http://www.maplibrary.org/library/stacks/Africa/Rwanda/RWA_outline_SHP.zip"
file = file.path(tmpdir, basename(url))
download.file(url, file)
unzip(file, exdir = tmpdir)

rwa = st_read(paste0(tmpdir, "/RWA_outline.shp"), crs = 4326)

# Get vert info
verts = igraph::as_data_frame(g, "vertices")



# ==============================================================================
# ==============================================================================
# ==============================================================================

# ** All plots

sims = mapply(cbind, sim, "day" = seq_along(sim), SIMPLIFY = F) %>%
    rbindlist(fill = TRUE)

sims = dplyr::left_join(sims, verts[ , c("name", "lat", "lon")], by = "name")


max(sims$day)
sum(sims$R[ sims$day %in% max(sims$day) ])



sim_df = sims %>%
    mutate_at(vars("lat", "lon"), as.numeric) %>%
    mutate(total_I = I + Ia,
           pt_size = sqrt(total_I/5000))

sim_sf = sim_df %>%
    st_as_sf(coords = c('lon', 'lat'), crs = 4326)

out_dir = "results/animations"
if( !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

library(patchwork)


for(i in seq_along(sim)) {

    df = sim_sf %>%
        filter(day %in% i & total_I > 0)

    map =

    ggplot() +
        geom_sf(data = rwa, fill = "#D3B3A2", color = NA ) +
        geom_sf(data = df, size = df$pt_size * 2, col = "#992828") +
    theme_void()

    curves =

    sim_df %>%
        filter(day <= i) %>%
        dplyr::select(S, total_I, R, day) %>%
        group_by(day) %>%
        summarize_all(list(sum)) %>%
        pivot_longer(cols = S:R) %>%
        ggplot() +
        geom_line(aes(x = day, y = value, color = name)) +
    xlab("Days") +
    ylab("No. of Individuals") +
    scale_color_manual(values = c("#275599", "#279955", "#C13165"),
                       labels = c("Recovered", "Susceptible", "Infectious")) +
    theme_ipsum(base_size = 12, axis_title_size = 12, axis_text_size = 10) +
    # theme(panel.grid.minor.x = element_blank()) +
    labs(color = "Infection status") +
    coord_cartesian(xlim=c(0, 290))

    p = map + curves
    # p = plot_grid(map, curves, align = "h", axis = "bt", rel_widths = c(1, 1))
    out_p = file.path(out_dir, paste0(i, ".png"))
    cowplot::save_plot(out_p, p,
                       base_height = 4,
                       base_width = 10)

}



    


    
    

    df_day = sim_df %>%
        dplyr::select(S, total_I, R, day) %>%
        group_by(day) %>%
        summarize_all(list(sum)) %>%
        as.data.frame()


    pivot_longer(cols = S:R) %>%
        as.data.frame() %>%
        ggplot() +
        geom_line(aes(x = day, y = value, color = name))

