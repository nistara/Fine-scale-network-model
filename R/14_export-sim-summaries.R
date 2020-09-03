# ==============================================================================
# 
# With the read-sim results
# Read in summarized simulation info and output to a csv
# 
# ==============================================================================


# Function to write above data to csv (from sheetr pkg)
# ==============================================================================
# ref: https://github.com/d-notebook/sheetr

write_list_to_csv = function(df_list, file){
    # Properties
    headings = names(df_list)
    seperator = paste(replicate(50, "="), collapse = "")
    # Create sink path
    sink(file)
    # Loop over each dataframe, and
    for(df.i in seq_along(df_list)){
        # Write heading
        if(headings[df.i] != ""){
            cat(headings[df.i])
            cat("\n")
        }
        # Write CSV
        write.csv(df_list[[df.i]])
        # Write seperator
        cat(seperator)
        cat("\n\n")
    }
    # Close sink
    sink()
    return(file)
}


# ==============================================================================
# Set input/ouput info
# ==============================================================================
# in_dir = "results/output/890/500-sims_seed-1-in-nd890"
in_dirs = list.dirs("results/output", recursive = FALSE)
in_sim_dirs = do.call(c, lapply(in_dirs, list.dirs, recursive = FALSE))
in_sim_dirs = gtools::mixedsort(in_sim_dirs)

# ==============================================================================
# Get the read-sim results
# ==============================================================================
lapply(in_sim_dirs, function(in_dir) {

    dir_split = strsplit(in_dir, "/")[[1]]
    out_dir = file.path("results", "csv", dir_split[3], dir_split[4])
    if( !dir.exists(out_dir) ) dir.create(out_dir, recursive = TRUE)

    fname = basename(in_dir)


    summ = readRDS(list.files(in_dir, pattern = "sim-summ", full.name = TRUE))


    inf = readRDS(list.files(in_dir, pattern = "nd-inf-times", full.name = TRUE))
    col_keep = c("city", "obs_order", "start_lowCI", "start_mean", "start_uppCI",
                 "start_median", "start_mode")

    inf_city = lapply(inf, function(df, col_keep) {
        df = df[ !is.na(df$city), ]
        df[ , col_keep]
    }, col_keep)

    
    write.csv(summ, file.path(out_dir, paste0("sim-summ-info_", fname, ".csv")),
              row.names = FALSE)

    write_list_to_csv(inf_city, file.path(out_dir,
                                          paste0("inf-max-city_", fname, ".csv")))
})


# ==============================================================================
# Get inf summary results for paper tables
# ==============================================================================
sim_names = basename(in_sim_dirs)

sim_summaries = lapply(in_sim_dirs, function(in_dir) {
    readRDS(list.files(in_dir, pattern = "sim-summ", full.name = TRUE))
})
names(sim_summaries) = sim_names

outbreak_summaries = lapply(in_sim_dirs, function(in_dir) {
    summ = readRDS(list.files(in_dir, pattern = "sim-summ", full.name = TRUE))
    summ[ summ$type %in% 3, ]
})

outbreak_summaries = do.call(rbind, outbreak_summaries)
col_names = names(outbreak_summaries)
outbreak_summaries$sim = sim_names
outbreak_summaries$prob_n = outbreak_summaries$n/500
outbreak_summaries = outbreak_summaries[ , c("sim", "prob_n", col_names)]

summ_out_dir = "results/csv/combined_summaries"
if(!dir.exists(summ_out_dir)) dir.create(summ_out_dir, recursive = TRUE)

write_list_to_csv(sim_summaries, file.path(summ_out_dir, "sim_summaries.csv"))
write.csv(outbreak_summaries, file.path(summ_out_dir, "outbreak_summaries.csv"),
          row.names = FALSE)

