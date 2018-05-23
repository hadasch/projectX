rm(list=ls())
setwd("~/GitHub/projectX/CODE/after Dhaka visit")
source("useful asreml functions.R")
library(data.table)
library(asreml)
library(nadiv)
library(gridExtra)
list.data <- readRDS("list_data.rds")
s.d <- "boro"
dat <- list.data[[s.d]]$`GxE means`

n.fa <- 3

# arithmetic means - unstructured
ari <- asreml(fixed   = ari.mean ~ 0 + Group + Group:r.i + t.j,
              random  = ~ L + Y + Y:L + us(L):G + Y:G, 
              data    = dat, ran.order = "user")

# adjusted means - unstructured
adj <- asreml(fixed   = adj.mean ~ 0 + Group + Group:r.i + t.j,
              random  = ~ L + Y + Y:L + us(L):G + Y:G + Y:L:G, 
              weights = w,
              family  = asreml.gaussian(dispersion=1.0),
              data    = dat, ran.order = "user")

