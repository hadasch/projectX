rm(list=ls())
setwd("C:/Users/Paul/Desktop/Bangladesh")

# load required packages
library(data.table)

list.data <- readRDS("list_data.rds")

### Investigating Group - Genotype Combinations ###
###################################################
 # Boro
   dat <- list.data[["boro"]][["plot data formatted"]]
   table(dat$G, dat$Group) # seems fine
   
 # Aman
   dat <- list.data[["aman"]][["plot data formatted"]]
   table(dat$G, dat$Group) # Overlap for dhan53, dhan 57, dhan 66
   
   dat <- data.table(dat)
   pdat <- droplevels(dat[G %in% c("BRRI dhan53", "BRRI dhan57", "BRRI dhan66")])
   
   table(pdat$Group, pdat$G)
   table(pdat$Env, pdat$G, pdat$Group)
   