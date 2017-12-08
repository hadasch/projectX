require(asreml)

GxEmeans <- read.delim("GxE_means.txt")

########################################
### Eberhart-Russell factor analytic ### 
########################################

ER_fa_mod <- asreml(fixed   = GxE_mean ~ G,
                    random  = ~ fa(G,k=1):E,
                    family  = asreml.gaussian(dispersion=10e-5),
                    control = asreml.control(pworkspace=8e8,
                                             workspace=8e8, 
                                             maxiter=100),
                    data    = GxEmeans)

# format variance component output
VC  <- summary(ER_fa_mod)$varcomp[2:3]
VC$name <- gsub("E!G.","",substr(rownames(VC),regexpr("!",rownames(VC))-1,nchar(rownames(VC))))
row.names(VC) <- NULL

VC_t <- data.frame(G     = unique(substr(VC$name,1,nchar(VC$name)-4)[1:(dim(VC)[1]-1)/2]),
                   Var   = VC[which(substr(VC$name,nchar(VC$name)-2,nchar(VC$name))=="var"),"component"],
                   Lamda = VC[which(substr(VC$name,nchar(VC$name)-2,nchar(VC$name))=="fa1"),"component"])

# BLUEs
BLUEs <- predict(ER_fa_mod, classify="G")$pred$pvals[,c('G','predicted.value')]
colnames(BLUEs) <- c("G","BLUE")

# BLUPs
BLUPs <- data.frame(name = names(ER_fa_mod$coefficients$random),
                    BLUP = ER_fa_mod$coefficients$random)
row.names(BLUPs) <- NULL

# Predictions
Preds <- predict(ER_fa_mod, classify="fa(G, k = 1):E")$pred$pvals[,c('G','E','predicted.value')]
colnames(BLUEs) <- c("G","BLUE")
