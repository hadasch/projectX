rm(list=ls())
setwd("C:/Users/Paul/Desktop/Bangladesh")

# load required packages
library(data.table)
library(emmeans)
library(plyr)

list.data <- readRDS("list_data.rds")

### loop through aman and boro ##########
# Obtain genotype means per environment #
# 1: arithmetic means                   #
# 2: adjusted means                     #

for (s.d in names(list.data)) {
  
  dat <- as.data.table(list.data[[s.d]][["plot data formatted"]])
  
  ### arithmetic means
  dat[, G.ari.mean  := mean(Yield), by=c("Env","G")]  # calc. mean yield per Env-G  combination
  dat[, GG.ari.mean := mean(Yield), by=c("Env","GG")] # calc. mean yield per Env-GG combination
  e.mean.ari <- unique(dat[, -c("Rep","Yield")])  # reduce dataset - eliminate duplicate rows
  
  ### adjusted means
  
  # prepare list
  n.e        <- length(levels(dat$Env))
  G.mean.adj  <- matrix(list(), n.e, 1)    
  GG.mean.adj <- matrix(list(), n.e, 1)   
  rownames(G.mean.adj) <- unique(dat$Env)
  rownames(GG.mean.adj) <- unique(dat$Env)

### GG ###  
  # loop through and analyze each environment
  for (sel.env in c(1:n.e)){
    mod <- lm(formula = Yield ~ GG + Rep,
              data    = subset(dat, Env==dat$Env[sel.env]))
    m       <- emmeans(mod, "GG")
    means   <- as.data.frame(m)[,c("GG", "emmean")]
    means$w <- diag(solve(vcov(m)))
    names(means) <- c("GG", "GG.adj.mean", "w.GG")
    
    GG.mean.adj[[sel.env]] <- means
  }
  # bring together all adj. means from different environments
  GG.mean.adj <- data.table(plyr::ldply(GG.mean.adj[,1], data.frame, .id="Env"))
  # combine arithmetic and adjusted means
  e.means <- data.table(merge(e.mean.ari, GG.mean.adj, by=c("Env","GG")))

### G ###
  # loop through and analyze each environment
  for (sel.env in c(1:n.e)){
    mod <- lm(formula = Yield ~ G + Rep,
              data    = subset(dat, Env==dat$Env[sel.env]))
    m       <- emmeans(mod, "G")
    means   <- as.data.frame(m)[,c("G", "emmean")]
    means$w <- diag(solve(vcov(m)))
    names(means) <- c("G", "G.adj.mean", "w.G")
    
    G.mean.adj[[sel.env]] <- means
  }
  # bring together all adj. means from different environments
  G.mean.adj <- data.table(plyr::ldply(G.mean.adj[,1], data.frame, .id="Env"))
  # combine arithmetic and adjusted means
  e.means <- data.table(merge(e.means, G.mean.adj, by=c("Env","G")))
  
  
  list.data[[s.d]][["GxE means"]] <- e.means[, c("Y", "L", "Env", "t.k", "r.i", "Group", 
                                                     "G", "G.ari.mean", "G.adj.mean", "w.G",
                                                     "GG", "GG.ari.mean", "GG.adj.mean", "w.GG")]
  
  # additionally export GxE means as text file
  write.table(x = list.data[[s.d]][["GxE means"]], sep="\t",
              file = paste0(s.d," GxE means.txt"))
  
}

### Check for data discrepancies
list.data[["aman"]][["GxE means"]][ G.ari.mean - G.adj.mean > 0.000001] # two means
list.data[["boro"]][["GxE means"]][ G.ari.mean - G.adj.mean > 0.000001] # nothing

### Save output
saveRDS(list.data, file = "list_data.rds")
