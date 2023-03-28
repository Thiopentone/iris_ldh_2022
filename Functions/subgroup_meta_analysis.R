pacman::p_load(metafor, meta)

res_rr<- results %>%
  filter(cat=="rr")

source("Functions/ci_estimate_function.R")

res_rr<-ci_estimate(res_rr)

logRR <- log(res_rr$value)
loglower <- log(res_rr$lower)
logupper <- log(res_rr$new)

graph1<- metagen(logRR, 
                 lower=loglower,
                 upper = logupper,
                 studlab = hemisphere,
                 subgroup = org,
                 method.tau = "REML",
                 sm = "RR",
                 text.fixed = "Fixed effects model",
                 data = res_rr)
pdf("forest_plot_pooled_results.pdf", width = 10)
step<- forest(graph1,
              leftcols = c("studlab", "N"),
              just = "left", just.addcols = "left", just.studlab = "left",
              fontsize = 12,
              fs.hetstat = 10,
              plotwidth = "8cm",
              col.square = "#9EC1CF",
              col.diamond = "#CC99C9",
              col.diamond.lines = "black",
              col.square.lines = "black",
              col.fixed = "transparent",
              col.random = "transparent",
              col.predict = "black",
              xlim = c(0.1,5),
              col.study = "black",
              squaresize = 1.0,
              hetlab = "Heterogeneity: ",
              spacing = 1,
              addrows.below.overall = 2,
              col.by = "black",
              text.common.w = "Fixed effects model",
              leftlabs = c("Hemisphere", "N"))
dev.off()
