rm(list=ls())
setwd("C:/Users/Paul/Documents/GitHub/projectX/CODE/after Dhaka visit")
library(lme4)
library(lmerTest)
library(plyr)
library(stringr)

# Import
raw <- read.delim("Boro dataset.txt")

raw$t.k   <- as.numeric(substr(raw$Year, 1, 4))
raw$Env   <- as.factor(paste(raw$Year, raw$Location, sep="-"))
raw$r.i   <- as.numeric(raw$Year.of.release)
raw$G     <- as.factor(raw$Variety)
raw$L     <- as.factor(raw$Location)
raw$Y     <- as.factor(raw$t.k)
raw$Rep   <- as.factor(raw$Rep)
raw$Group <- as.factor(raw$Group)

plotdata <- raw[ ,c("Y", "t.k", "r.i", "L", "Env", "G", "Rep", "Group", "Yield")]
plotdata <- as.data.table(plotdata)

# Prepare list for all location pairs
Locs <- levels(plotdata$L)
Locpairs <- combn(Locs,2)

g.corrs <- data.frame(matrix(NA,10,10))
rownames(g.corrs) <- Locs; colnames(g.corrs) <- Locs

for (i in 1:dim(Locpairs)[2]){
  
  pair      <- Locpairs[,i]
  pair.data <- droplevels(plotdata[L %in% pair])

  # model fitting
  lme <- lmer(formula = Yield ~ (1|Y) + (1|L) + (1|Y:L) + 
                                (1|Y:G) + (L-1|G) + (1|Y:L:Rep),
              data    = pair.data)
  
  # vc formatting
  vc <- data.table(data.frame(VarCorr(lme)))
  vc[!is.na(var2), c("vcov", "var1") := .(sdcor, "L.corr")]
  vc[var1=="(Intercept)", var1 := NA]
  vc <- data.table(vc.1=vc$grp, vc.2=vc$var1, 
                      estimate=round(vc$vcov, 3))
  vc <- vc[order(vc.1, vc.2)]
    
  # result output
  g.corrs[pair[1], pair[2]] <- vc[vc.2=="L.corr", "estimate"]
    
  rm(pair, pair.data, vc)
}

g.corrs
