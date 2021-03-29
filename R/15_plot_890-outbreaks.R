# ==============================================================================
# * Workspace
# ==============================================================================
library(dplyr)
library(ggplot2)
library(hrbrthemes)
require(ggthemes) 

invisible(lapply(list.files("R/SEEDNet", full = TRUE), source))
# OR
# devtools::install_github("nistara/SEEDNet@v0.1")
# library(SEEDNet)


# ==============================================================================
# Load data
# ==============================================================================
sim_dir = "data/simulations/890/500-sims_seed-1-in-nd890"
sim_name = strsplit(sim_dir, "/")[[1]][4]

# Read in the info files for each simulation--------------------------------
sim_info = get_sim_info(sim_dir, nsims = NA)

# Get the duration of each simulation---------------------------------------
sim_l = get_sim_l(sim_info)

# Get outbreaks info------------------------------------------------------------
outbrks_info = readRDS("results/output/890/500-sims_seed-1-in-nd890/outbrks-info_500-sims_seed-1-in-nd890.RDS")


# ==============================================================================
# Plot epidemic sizes
# ==============================================================================
out_dir = "results/figs"
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

fname = paste0("results/figs/epi-size_", sim_name, ".pdf")

pdf(fname)

p = hist(sim_l, breaks = 30,
         # labels = TRUE,
         ylim = c(0, 275),
         xlim = c(0, 400),
         xlab = "Epidemic size (days)",
         ylab = "Frequency",
         main = paste0("Histogram of epidemic sizes \n", sim_name))

# sim_ldf = data.frame(days = sim_l, n = 1) %>%
#     arrange(days) %>%
#     group_by(days) %>%
#     summarize(value = n()) %>%
#     as.data.frame()
# 
# sim_ldf1 = sim_ldf[ sim_ldf$days < 200, ]
# sim_ldf2 = sim_ldf[ sim_ldf$days >= 200, ]


# ggplot() +
#     geom_segment(data = sim_ldf1,
#                  aes(x = days, xend = days, y = 0, yend = value),
#                  color="grey") +
#     geom_point(data = sim_ldf1,
#                aes(x = days, y = value),
#                size=1, color="steelblue") +
#     geom_segment(data = sim_ldf2,
#                  aes(x = days, xend = days, y = 0, yend = value),
#                  color="grey") +
#     geom_point(data = sim_ldf2,
#                aes(x = days, y = value),
#                size=1, color= "red") +
#     theme_classic(base_size = 12)+
#     coord_fixed(ratio = 3) +
#     scale_x_continuous(breaks=seq(0,400,50), expand = c(0.01, 0.01))+
#     scale_y_continuous(breaks=seq(0,100,25), expand = c(0.01, 0.01))+
#     geom_segment(aes(x=0,xend=400,y=-Inf,yend=-Inf))+
#     geom_segment(aes(y=0,yend=100,x=-Inf,xend=-Inf))+
#     theme(axis.line=element_blank()) +
#     xlab("Epidemic size (days)") +
#     ylab("")

dev.off()

# Plot epidemic curve (mean + median + individual sims)-----------------

n = max(sim_l)

# getting infection info
inf = lapply(outbrks_info, function(x, n) {
    inf_sum = x[ , "I"] + x[ , "Ia"]
    length(inf_sum) = n
    inf_sum
}, n)


inf_df = do.call(cbind, inf)
inf_mean = rowMeans(inf_df, na.rm = TRUE)
inf_median = apply(inf_df, 1, median, na.rm = TRUE)

plot_ylim = max(unlist(inf), na.rm = TRUE) # 39001


pdf(paste0("results/figs/sim-lines_", sim_name, ".pdf"))

# First create an empty plot.
plot(1, type = 'n',
     xlim = c(0, 400),
     ylim = c(0, plot_ylim),
     xlab = "Days",
     ylab = "No. of infected individuals")


for(i in 1:length(inf)) {
    lines(inf[[i]], col = "lightgrey")
}

lines(inf_mean, col = "darkgreen", lwd = 1.5)
lines(inf_median, col = "purple", lwd = 1.5)

legend(225, 30000,
       legend = c("Individual outbreak",
                "Mean across all outbreaks",
                "Median across all outbreaks"),
       col = c("lightgrey", "darkgreen", "purple"),
       lty = 1, cex = 0.9, lwd = 1.5,
       box.lty = 0, title = "Influenza cases")

axis(side = 1, at = 242, col="red", col.axis="red", col.lab="blue", 
     font.axis = 3, font = 1)

axis(side = 1, at = 288, col = "red", col.axis = "red", col.lab = "blue", 
     font.axis = 3, font = 1)

dev.off()

}


# ==============================================================================
# ==============================================================================
# ==============================================================================

# Libraries
library(tidyverse)
library(hrbrthemes)
library(plotly)
library(patchwork)
library(babynames)
library(viridis)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/3_TwoNumOrdered.csv", header=T)
data$date <- as.Date(data$date)

# plot
data %>%
  ggplot( aes(x=date, y=value)) +
    geom_area(fill="#69b3a2", alpha=0.5) +
    geom_line(color="#69b3a2") +
    ggtitle("Evolution of Bitcoin price") +
    ylab("bitcoin price ($)") +
    theme_ipsum()
