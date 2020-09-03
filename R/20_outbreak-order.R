library(dplyr)

nd_l = readRDS("results/output/890/500-sims_seed-1-in-nd890/nd-inf-times_500-sims_seed-1-in-nd890.RDS")


nd_l = lapply(nd_l, function(df) df[ !is.na(df$city), ])

col_keep = c("start_mean", "start_sd", "start_lowCI", "start_uppCI",
             "city", "obs_order")

nd_l$first_times[, col_keep]

ft = nd_l$first_times %>%
    dplyr::select(col_keep) %>%
    mutate_at(vars(start_mean, start_sd, start_lowCI, start_uppCI),
              round, 1)

ft$m_sd = paste0(ft$start_mean, "±", ft$start_sd)
ft$ci = paste0(ft$start_lowCI, "-", ft$start_uppCI)
