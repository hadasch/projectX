# Shukla's Stability Variance
#############################

#################
#dataset <- "aman"
dataset <- "boro"
#################

dat3 <- read.delim(paste(dataset,"_means.txt",sep=""))
dat3 <- dat3[order(dat3$G,dat3$Env),] 

shuklamod <- asreml(fixed   = adjmean ~ xj + tj,
                    random  = ~ G + L + Y + L:G + Y:G + Y:L + at(G):Env, 
                    #rcov    = ~ at(G):Y:L, #Fehlermeldung?!
                    weights = w,
                    family  = asreml.gaussian(dispersion=1.0),
                    control = asreml.control(pworkspace=8e10 ,
                                             workspace=8e8, 
                                             maxiter=100),
                    data    = dat3, ran.order = "user")

shuklamod$coefficients$fixed

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

write.table(shukla_out, paste(dataset,"_shukla.txt", sep=""), row.names = F, sep="\t")


# Shuklas Variances
source("12 Plot Shuklas Variances.R")
shuklaplot
