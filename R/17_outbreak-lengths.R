library(finalfit)
library(dplyr)

df = readRDS("results/output/890/500-sims_seed-1-in-nd890/inf-info_500-sims_seed-1-in-nd890.RDS")

head(df)

explanatory = "type"

df %>%
    summary_factorlist("days", explanatory)

df %>%
  summary_factorlist("days", explanatory, cont="median")


df %>%
  summary_factorlist("nds", explanatory)

df %>%
  summary_factorlist("nds", explanatory, cont="median")

agg_fun <- function(v, na.rm = TRUE) {
    c(n = length(v), 
      round(c(mean = mean(v),
              median = median(v),
              min = min(v),
              max = max(v)), 2))
}


# Number of outbreaks
table(df$type)
table(df$type)/500 * 100

# Spread for type 1 and 2 outbreaks
df12 = df[ df$type %in% c(1, 2), ]
df12$type[ df12$days %in% 1 & df12$type %in% 1 ] = 0
table(df12$days)
table(df12$nds)

df12 %>%
    select(days, nds) %>%
    group_by(nds, days) %>%
    summarize(n = n()) %>%
    as.data.frame()

aggregate(days ~ nds + type, data = df12, FUN = agg_fun, na.rm = TRUE)

# Days (simulation lengths) by type
aggregate(days ~ type, data = df, FUN = agg_fun, na.rm = TRUE)

# Spread( nodes infected) by type
aggregate(nds ~ type, data = df, FUN = agg_fun, na.rm = TRUE)





