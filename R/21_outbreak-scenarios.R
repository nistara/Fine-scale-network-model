library(forestplot)
library(dplyr)
library(gridGraphics)
library(gridExtra)

df = read.csv("results/csv/combined_summaries/outbreak_summaries.csv",
              stringsAsFactors = FALSE)

scenarios = c("500-sims_seed-1-in-nd890", 
              "14_seed-890_vacc-890-0p48", 
              "seed-1-in-1239", 
              "1_seed-1239_vacc-890-0p48", 
              "1_seed-1239_vacc-1239-0p48", 
              "1_seed-1239_vacc-1239-890-0p48", 
              "seed-1-in-539", 
              "1_seed-539_vacc-890-0p48", 
              "1_seed-539_vacc-539-0p48", 
              "1_seed-539_vacc-539-890-0p48")

df = df[df$sim %in% scenarios, ]
df = df[ match(scenarios, df$sim), ]

df[ , c("n", "type")] = NULL

df[ , grep("inf", names(df))] = round(df[ , grep("inf", names(df))]/10000, 1)
df[ , grep("nds", names(df))] = round(df[ , grep("nds", names(df))]/100, 1)
df$prob_n = round(df$prob_n, 2)


# df$inf = paste0(df$inf_mean, "±", df$inf_sd)
# df$areas = paste0(df$nds_mean, "±", df$nds_sd)

# df$inf = paste0(df$inf_mean, "(", df$inf_min, ", ", df$inf_max, ")")
# df$areas = paste0(df$nds_mean, "(", df$nds_min, ", ", df$nds_max, ")")


df

# df_out = df[ , c("sim", "prob_n", "inf", "areas")]
df_out = df[ , c("sim", "prob_n", "inf_mean", "nds_mean", "days_mean")]
write.csv(df_out, "results/csv/combined_summaries/outbreak_summaries_trim.csv",
          row.names = FALSE)


# ==============================================================================
# Make forest plots
# ==============================================================================

df_text = data.frame(Seed = c("Kigali", NA,
                              "Rubavu", NA, NA, NA,
                              "Kibungo", NA, NA, NA),
                     Vaccination = c("None", "Kigali",
                                     "None", "Kigali", "Rubavu", "Rubavi & Kigali",
                                     "None", "Kigali", "Kibungo", "Kibungo & Kigali"),
                     stringsAsFactors = FALSE)

df_text$Seed = NA
df_text$Vaccination = NA

df_prob = df %>%
    select("prob_n") %>%
    mutate(lower = 0,
           upper = 1)

df_inf = df %>%
    select("inf_mean") %>%
    mutate(lower = min(inf_mean),
           upper = max(inf_mean))

df_days = df %>%
    select("days_mean") %>%
    mutate(lower = min(days_mean),
           upper = max(days_mean))


df_nds = df %>%
    select("nds_mean") %>%
    mutate(lower = min(nds_mean),
           upper = max(nds_mean))

forestplot(df_text, df_prob,
           xticks = c(0, 0.5, 1),
           boxsize = 0.15,
           clip = c(0, 1),
           zero = NA,
           lineheight = unit(1, "cm"),
           col=fpColors(box="#d95f02",line="#d95f02"))

p_prob = grid.grab()

forestplot(df_text, df_inf,
           xticks = c(10, 35, 60),
           boxsize = 0.15,
           clip = c(15, 55),
           zero = NA,
           lineheight = unit(1, "cm"),
           col=fpColors(box="#0072B2",line="#0072B2"))

p_inf = grid.grab()

forestplot(df_text, df_days,
           xticks = c(280, 370,  470),
           boxsize = 0.15,
           clip = c(280, 465),
           zero = NA,
           lineheight = unit(1, "cm"),
           col=fpColors(box="#009E73",line="#009E73"))

p_days = grid.grab()

forestplot(df_text, df_nds,
           xticks = c(9, 10.5, 12),
           boxsize = 0.15,
           clip = c(9, 12),
           zero = NA,
           lineheight = unit(1, "cm"),
           col=fpColors(box="#AA3377",line="#AA3377"))

p_nds = grid.grab()


pdf("results/figs/forest-plots.pdf")
grid.arrange(p_prob, p_inf, p_days, p_nds, ncol=4, vp=viewport(width=0.7, height=0.7))
dev.off()


