rm(list=ls())
setwd("~/GitHub/projectX/CODE/after Dhaka visit")
source("useful asreml functions.R")
library(data.table)
library(asreml)
library(nadiv)
library(gridExtra)
list.data <- readRDS("list_data.rds")
s.d <- "aman"

results <- matrix(list(), 3, 2)
  rownames(results) <- c("vc", "vcplot", "gcorr")
  colnames(results) <- names(list.data)

for (s.d in names(list.data)){
  
dat <- list.data[[s.d]]$`GxE means`

ari <- asreml(fixed   = ari.mean ~ 0 + Group + Group:r.i + t.j,
              random  = ~ G + L + Y + Y:L + L:G + Y:G, 
              data    = dat, ran.order = "user")

vc <- getVC(ari); 
vc$CovParm <- substr(vc$CovParm,1,regexpr("!", vc$CovParm)-1)
results[["vc", s.d]] <- vc
write.table(vc[,c("CovParm", "estimate", "std.error")], 
            paste0(s.d, " vc.txt"), 
            row.names = F, sep="\t", dec = ",") 

g.var  <- vc[CovParm=="G",   estimate]
gl.var <- vc[CovParm=="L:G", estimate]
g.corr <-  g.var / (g.var + gl.var)
results[["gcorr", s.d]] <- g.corr

plot <- plotVC(vc) + 
        ggtitle(paste0(s.d, "  (gen. corr. = ", round(g.corr,3),")"))
results[["vcplot", s.d]] <- plot
                             
}
  
grid.arrange(results[["vcplot", "aman"]], 
             results[["vcplot", "boro"]], ncol=2, nrow=1)
  

  