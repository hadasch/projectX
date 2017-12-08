require(asreml)
wd <- paste("D:/User/pschmidt/bwSyncAndShare/Yield Stability/examplecode")
setwd(wd)
GxEmeans <- read.delim("GxE_means.txt")

########################################
### Eberhart-Russell factor analytic ### 
########################################

ER_fa_mod <- asreml(fixed   = GxE_mean ~ G,
                    random  = ~ fa(G,k=1):E,
                    family  = asreml.gaussian(dispersion=10e-5),
                    control = asreml.control(pworkspace=8e8 ,
                                             workspace=8e8, 
                                             maxiter=100),
                    data    = GxEmeans)

ER_fa_mod_vc  <- summary(ER_fa_mod)$varcomp[2:3]
ER_fa_mod_vc$temp <- rownames(ER_fa_mod_vc)[2:dim(ER_fa_mod_vc)[1]]
ER_fa_mod_vc$test <- gregexpr("G.",ER_fa_mod_vc$temp)
