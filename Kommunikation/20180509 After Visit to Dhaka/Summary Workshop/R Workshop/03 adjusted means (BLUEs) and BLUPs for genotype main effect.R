rm(list=ls())
setwd("...change.this.to.your.folder...") 
meandata <- readRDS("GxEmeans.rds")

library(asreml)
library(lme4)
library(lmerTest)
library(emmeans)
  # Note that in order to obtain adjusted means (also called
  # lsmeans or BLUEs) for an lme4 object, we need the function
  # lsmeans() of yet another package called emmeans.

  # Note also that irrespective of whether BLUPs or BLUEs are
  # to be estimated for the genotype main effect, the respective
  # model for this purpose should not include genetic trend effect(s).

### Obtain BLUPs for genotypes
##############################
  # In order to obtain genotypic BLUPs, there must be a random genotype main effect.

# lme4
ran.G.lme <- lmer(formula = mean.yield ~ t.k +
                          (1|G) + (1|L) + (1|Y) + 
                          (1|Y:L) + (1|Y:G) + (1|L:G),
                data    = meandata)

G.BLUPs.lme <- ranef(ran.G.lme)$G
G.BLUPs.lme

# asreml
ran.G.asr <- asreml(fixed  = mean.yield ~ t.k,
                    random = ~ G + L + Y + Y:L + L:G + Y:G, 
                    data   = meandata)

G.BLUPs.asr <- predict(ran.G.asr, classify="G", only="G")$predictions$pvals[,1:3]
G.BLUPs.asr


### Obtain adjusted means (BLUEs) for genotypes
###############################################
  # In order to obtain genotypic BLUEs, there must be a fixed genotype main effect.
  # Note that in this model, the genotypic BLUEs depend on t.k: Since t.k is estimated
  # to be a positive trend for trial years, the genotypic BLUEs would be higher in 2015
  # than in 2001. When genotypic BLUEs are requested for no specific trial year, the 
  # lsmeans() and predict() functions will by default estimate them for the overall 
  # mean of trial year (t.k), which in this dataset is:
  mean(meandata$t.k) # 2008.38

# lme4
fix.G.lme <- lmer(formula = mean.yield ~ G + t.k +
                          (1|L) + (1|Y) + 
                          (1|Y:L) + (1|Y:G) + (1|L:G),
                data    = meandata)

G.BLUEs.lme <- lsmeans(fix.G.lme, "G")
G.BLUEs.lme

# asreml
fix.G.asr <- asreml(fixed  = mean.yield ~ G + t.k,
                    random = ~ L + Y + Y:L + L:G + Y:G, 
                    data   = meandata)

G.BLUEs.asr <- predict(fix.G.asr, classify="G")$predictions$pvals[,1:3]
G.BLUEs.asr
