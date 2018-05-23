rm(list=ls())
setwd("~/GitHub/projectX/CODE/after Dhaka visit")
source("useful asreml functions.R")
library(data.table)
library(asreml)
library(nadiv)
library(gridExtra)
list.data <- readRDS("list_data.rds")
s.d <- "aman"
dat <- list.data[[s.d]]$`GxE means`

# multiple genetic trends #
###########################

# arithmetic
ari <- asreml(fixed   = ari.mean ~ 0 + Group + Group:r.i + t.j,
              random  = ~ G + L + Y + Y:L + L:G + Y:G, 
              data    = dat, ran.order = "user")

# adjusted
adj <- asreml(fixed   = adj.mean ~ 0 + Group + Group:r.i + t.j,
              random  = ~ G + L + Y + Y:L + L:G + Y:G + Y:L:G, 
              weights = w,
              family  = asreml.gaussian(dispersion=1.0),
              data    = dat, ran.order = "user")

info <- data.frame(means=c("arithmetic", "adjusted"),
           conv=c(ari$converge, adj$converge),
           n.par=c(getAIC(ari)$n.par, getAIC(adj)$n.par),
           AIC=c(getAIC(ari)$AIC, getAIC(adj)$AIC))

p.ari <- plotVC(getVC(ari)) 
p.adj <- plotVC(getVC(adj))


pdf(paste0(s.d," basic vc.pdf"), height=5, width=10)
  info
  grid.arrange(p.ari, p.adj, ncol=2, nrow=1)
dev.off()

system(paste0('open "', getwd(),'/',s.d, ' basic vc.pdf"'))

