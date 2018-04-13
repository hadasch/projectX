rm(list=ls())
library(data.table)
library(emmeans)
library(plyr)

list.data <- readRDS("list_data.rds")

############## First Stage ############## 
# Obtain genotype means per environment #
# A: arithmetic means                   #
# B: adjusted means                     #
# check whether A == B                  #


for (sel.dat in names(list.data)) {
  
  dat <- as.data.table(list.data[[sel.dat]][["plot data formatted",1]])
  
  # arithmetic means
  dat[, ari.mean := mean(Yield), by=c("Env","GG")] # calc. mean yield per Env-GG combination
  e.mean.ari <- unique(dat[, -c("Rep","Yield")])   # reduce dataset - eliminate duplicate rows
             
  # adjusted means
  n.e        <- length(unique(dat$Env))
  e.mean.adj <- matrix(list(), n.e, 1)
  rownames(e.mean.adj) <- unique(dat$Env)
  
  for (sel.env in c(1:n.e)){
    mod <- lm(formula = Yield ~ GG + Rep,
              data    = subset(dat, Env==dat$Env[sel.env]))
    m       <- emmeans(mod, "GG")
    means   <- as.data.frame(m)[,c("GG", "emmean")]
    names(means) <- c("GG", "adj.mean")
    means$w <- diag(solve(vcov(m)))
    
    e.mean.adj[[sel.env]] <- means
  }
  e.mean.adj <- data.table(plyr::ldply(e.mean.adj[,1], data.frame, .id="Env"))
  
  # combine arithmetic and adjusted means
  e.means <- data.table(merge(e.mean.ari, e.mean.adj, by=c("Env","GG")))
  
  list.data[[sel.dat]][["GxE means",1]] <- e.means[, c("Y", "L", "Env", "t.j", 
                                                       "r.i", "Group", "G", "GG", 
                                                       "ari.mean", "adj.mean", "w")]
  
  # export GxE means as text file
  write.table(x = list.data[[sel.dat]][["GxE means",1]], sep="\t",
              file = paste0(sel.dat," GxE means.txt"))

}

# Check for data discrepancies
list.data[["aman"]][[4,1]][ ari.mean - adj.mean > 0.000001]
list.data[["boro"]][[4,1]][ ari.mean - adj.mean > 0.000001]

saveRDS(list.data, file = "list_data.rds")
