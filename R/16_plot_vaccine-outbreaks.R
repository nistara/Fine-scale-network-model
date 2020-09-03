# ==============================================================================
# Generate outbreak probability plot for different vaccine scenarios
# ==============================================================================

# * Workspace
# ==============================================================================
library(gtools)
library(RColorBrewer)
library(stringr)
library(tidyr)
library(dplyr)

# * Set input/ouput info
# ==============================================================================
in_dir = "results/output/890"
out_dir = "results/figs"
if( !dir.exists(out_dir) ) dir.create(out_dir, recursive = TRUE)

pdf_out = file.path(out_dir, "vacc_prop.pdf")

no_vacc_file = "results/output/890/500-sims_seed-1-in-nd890/sim-summ-inf_500-sims_seed-1-in-nd890.RDS"

# * Read in data
# ==============================================================================
inf_info_files = mixedsort(
                     list.files(in_dir, pattern = "inf-info",
                                recursive = TRUE,
                                full.names = TRUE))
inf_info_files = inf_info_files[ !grepl("nd-inf-info|seed-1-in", inf_info_files)] 


sim_info_files = mixedsort(
                     list.files(in_dir, pattern = "sim-summ-inf",
                                recursive = TRUE,
                                full.names = TRUE))
sim_info_files = sim_info_files[ !grepl("seed-1-in", sim_info_files)] 


inf_info = lapply(inf_info_files, readRDS)
sim_info = lapply(sim_info_files, readRDS)


# * Get outbreak probabilities
# ==============================================================================
outbrk_prop = sapply(sim_info, function(df) {
    prop = df$n[3]/sum(df$n)
})

if(FALSE) {

    outbrk_prop_chk = sapply(inf_info, function(df) {
        prop = sum(df$type == 3)/nrow(df)
    })

    all.equal(outbrk_prop, outbrk_prop_chk)

}

names(outbrk_prop) = seq_along(outbrk_prop)

vacc_eff = c(50, 60, 70, 80, 90)
vacc_cov = c(20, 40, 60, 80, 100)
vacc = expand.grid(vacc_eff, vacc_cov)
names(vacc) = c("eff", "cov")

vacc$vacc_prop = (vacc$eff * vacc$cov)/10000
vacc$i = rep(1:5, 5)
vacc$outbrk_prop = outbrk_prop

if(FALSE) {

    cov_fnames = sapply(inf_info_files, function(fname) strsplit(fname,  "/|-")[[1]][7])
    names(cov_fnames) = NULL
    cov_fnames = gsub("p", ".", cov_fnames)
    cov_fnames = as.numeric(cov_fnames)

    all.equal(cov_fnames, vacc$vacc_prop)

}

no_vacc_info = readRDS(no_vacc_file)
no_vacc_outbrk_prop = no_vacc_info$n[3]/sum(no_vacc_info$n)

outbrks = do.call(rbind,
                  lapply(1:5, function(i, df) {df$outbrk_prop[ df$i == i ]}, vacc))
outbrks = cbind(no_vacc_outbrk_prop, outbrks)
rownames(outbrks) = vacc_eff
colnames(outbrks) = c("0", vacc_cov)

for(i in 1:5) {
    prop = c(no_vacc_outbrk_prop, vacc$outbrk_prop[ vacc$i == i])
    print(prop)
}

    
# * Plot prob of outbreak for varying vaccinations
# ==============================================================================

cl = brewer.pal(5, "Dark2")
l_width = c(1, 1, 1, 2, 1) # set line width
# cl <- rainbow(5)

pdf(pdf_out)

vacc_cov_plot = c(0, vacc_cov)
par(mar = c(6,10,4,4))
plot(vacc_cov_plot, seq(0, 1, .2),
     ylim = c(0, 0.4),
     xlab = "Vaccine coverage (%)",
     ylab = "",
     type = "n",
     bty="n")
mtext("Outbreak \n probability ", side=2,las=1,line=2)

for(i in 1:5) {
    # prop = c(no_vacc_outbrk_prop, vacc$outbrk_prop[ vacc$i == i])
    prop = outbrks[i, ]
    points(vacc_cov_plot, prop, type = "p", cex = .4, col = cl[i])
    lines(vacc_cov_plot, prop, type="l", col = cl[i], lwd = l_width[i])
}

legend("topright",
       title = "Vaccine efficacy (%)",
       legend = vacc_eff, 
       col = cl,
       lty = 1,
       lwd = l_width,
       # pch = 19, 
       bty = "n", 
       pt.cex = 1, 
       cex = 1, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))
title(main="Probability of outbreaks for varying vaccine coverage and efficacies", cex.main=1)

dev.off()


# * Main vaccination info
# ==============================================================================
vacc_eff80 = vacc[ vacc$eff %in% 80, ]
vacc_eff80$outbrk_percent = vacc_eff80$outbrk_prop * 100
