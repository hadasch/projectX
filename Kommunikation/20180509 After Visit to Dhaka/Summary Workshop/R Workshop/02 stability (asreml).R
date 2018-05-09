##########################################################
### Step 1: Import meandata (created in previous R script)
##########################################################
rm(list=ls())
setwd("...change.this.to.your.folder...")  
meandata <- readRDS("GxEmeans.rds") # import rds file from first script

##############################################
### Step 2: Refit basic model, but with ASReml
##############################################
  # Modelling stability requires more complex variance structures of random effects.
  # Fitting such variance structures is not possible with open-source packages like
  # lme4 or nlme. Therefore, we need to switch to the ASReml-R package. This package
  # is not open source - it requires a license. 
  # See: https://www.vsni.co.uk/software/asreml-r/

library(asreml) # the asreml package will only work if you have a (trial) licence

# Same model as Step 4 in first R script
mod <- asreml(fixed  = mean.yield ~ r.i + t.k,
              random = ~ G + L + Y + Y:L + L:G + Y:G, 
              data   = meandata)

# Same model as Step 4b in first R script
mod.group <- asreml(fixed  = mean.yield ~ 0 + Group + Group:r.i + t.k,
                    random = ~ G + L + Y + Y:L + L:G + Y:G, 
                    data   = meandata)

# (Shukla's) stability variance per genotype (slide 9 in in "Piepho - stability.pdf")
mod.stabvar <- asreml(fixed  = mean.yield ~ 0 + Group + Group:r.i + t.k,
                      random = ~ G + L + Y + Y:L + L:G + Y:G,
                      rcov   = ~ at(G):units,
                      data   = meandata)

summary(mod.stabvar)$varcomp[,c("component", "std.error")]
  # Show estimated variance components (including stability variance per genotype)
