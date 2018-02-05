# Shukla's Stability Variance
#############################
require(asreml)

#################
dataset <- "aman"
#dataset <- "boro"
#################

dat3 <- read.delim(paste(dataset,"_means.txt",sep=""))
dat3 <- dat3[order(dat3$G,dat3$Env),]
dat3$Y <- as.factor(paste(dat3$Y))

shuklamod <- asreml(fixed   = adjmean ~ ri + tj,
                    random  = ~ G + L + Y + L:G + Y:G + Y:L + at(G):Env, 
                    #rcov    = ~ at(G):Y:L, #Fehlermeldung?!
                    weights = w,
                    family  = asreml.gaussian(dispersion=1.0),
                    control = asreml.control(pworkspace=8e10 ,
                                             workspace=8e8, 
                                             maxiter=100),
                    data    = dat3, ran.order = "user")


# Format VC estimates
shukla_vc  <- summary(shuklamod)$varcomp[2:3]
  vc_names <- rownames(shukla_vc)
  vc_names <- substr(vc_names,1,regexpr('!',vc_names)-1)
  vc_names <- gsub(")"," ",vc_names)
  vc_names <- gsub("at\\(G, ","",vc_names)
shukla_vc$VC <- as.factor(vc_names)
shukla_vc$yesno <- "no"
shukla_vc[which(regexpr(":Env",shukla_vc$VC)>0),"yesno"] <- "yes"
shukla_vc$StabVar <- NA
shukla_vc[which(shukla_vc$yesno=="yes"),"StabVar"] <- gsub(" :Env","",shukla_vc[which(shukla_vc$yesno=="yes"),"VC"])                          
shukla_vc$StabVar   <- as.factor(shukla_vc$StabVar)
rownames(shukla_vc) <- NULL
shukla_vc <- shukla_vc[,c(3,1,2,5)]
colnames(shukla_vc) <- c("VC","Estimate","StdErr","V")
shukla_out <- subset(shukla_vc, is.na(shukla_vc$V)==F)

shukla_out <- shukla_out[,c("V","Estimate","StdErr")]
shukla_out[,c(2,3)] <- round(shukla_out[,c(2,3)],4)

# t-test for trends
shukla_t_test=summary(shuklamod,all=T)$coef.fixed
# Wald-test for trends
options(scipen=10) #change how numbers are displayed: NOT e-4 etc. 
shukla_wald_test=wald(shuklamod, denDF = c("none"),ssType = c("incremental","conditional"))[,c(1,3,4)]
shukla_wald_test[,c(2,3)] <- round(shukla_wald_test[,c(2,3)],4)

# Residuals
dat3$residShukla <- shuklamod$residuals

# Export
write.table(shukla_out, paste(dataset,"_shukla.txt", sep=""), row.names = F, sep="\t")
write.table(shukla_wald_test, paste(dataset,"_shukla_Wald_test.txt", sep=""), row.names = T, sep="\t")
write.table(dat3, paste(dataset,"_resid_Shukla.txt", sep=""), row.names = F, sep="\t")


# Shuklas Variances
source("03b Plot Shuklas Variances.R")
require(devEMF)
emf(paste(dataset,"_shukla.emf", sep=""), width = 8, height = 5)
  print(shuklaplot)
dev.off()
