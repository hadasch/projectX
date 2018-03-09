# Shukla's Stability Variance
#############################
require(asreml)
require(data.table)
require(stringr)
require(nadiv)

#################
dataset <- "aman"
dataset <- "boro"
#################

dat3 <- read.delim(paste(dataset,"_means.txt",sep=""))
dat3 <- dat3[order(dat3$G,dat3$Env),]
dat3$Y <- as.factor(paste(dat3$Y))

shuklamod <- asreml(fixed   = adjmean ~ ri + tj,
                    random  = ~ G + L + Y + Y:L + 
                                L:G + Y:G  + at(G):Env, 
                    #rcov    = ~ at(G):Y:L, #Fehlermeldung?!
                    weights = w,
                    family  = asreml.gaussian(dispersion=1.0),
                    control = asreml.control(pworkspace=8e10 ,
                                             workspace=8e8, 
                                             maxiter=100),
                    data    = dat3, ran.order = "user")



# Format VC estimates
VC1 <- data.table(summary(shuklamod)$varcomp,  keep.rownames="CovParm")
VC2 <- data.table(   aiCI(shuklamod),          keep.rownames="CovParm")
VC  <- VC2[VC1, .(CovParm, LCL, estimate, UCL, std.error),on="CovParm"]
VC$CovParm <- tstrsplit(VC$CovParm,"!",fixed=T)[[1]]
VC$is_Shuk <- str_detect(VC$CovParm,"at\\(G,")
VC$CovParm[VC$is_Shuk] <- str_match(VC$CovParm[VC$is_Shuk], levels(dat3$G))
VC[, names(VC)[2:5]:=round(.SD,4), .SDcols=names(VC)[2:5]]
ShukV <- VC[VC$is_Shuk]

# t-test for trends
shukla_t_test=summary(shuklamod,all=T)$coef.fixed
# Wald-test for trends
options(scipen=10) #change how numbers are displayed: NOT e-4 etc. 
shukla_wald_test=wald(shuklamod, denDF = c("none"),ssType = c("incremental","conditional"))[,c(1,3,4)]
shukla_wald_test[,c(2,3)] <- round(shukla_wald_test[,c(2,3)],4)

# Residuals
dat3$residShukla <- shuklamod$residuals

# Export
fwrite(ShukV, paste(dataset,"_shukla.txt", sep=""), sep="\t")
write.table(shukla_wald_test, paste(dataset,"_shukla_Wald_test.txt", sep=""), row.names = T, sep="\t")
write.table(dat3, paste(dataset,"_resid_Shukla.txt", sep=""), row.names = F, sep="\t")


# Shuklas Variances
source("03b Plot Shuklas Variances.R")
require(devEMF)
emf(paste(dataset,"_shukla.emf", sep=""), width = 8, height = 5)
  print(shuklaplot)
dev.off()
