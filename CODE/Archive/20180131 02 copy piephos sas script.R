rm(list = ls())
dat <- read.delim("aman.txt")
dat$Y   <- as.factor(dat$Y)
dat$Rep <- as.factor(dat$Rep)
dat$GG  <- as.factor(paste(dat$Group,dat$G,sep="-"))

# arithmetic means
arimean  <- tapply(dat$Yield,paste(dat$Env,dat$GG),mean,na.rm=F)
dat2     <- data.frame(BY=rownames(arimean),arimean=arimean)
dat2$Env <- as.factor(substr(dat2$BY,1,regexpr(" ",dat2$BY)-1))
dat2$GG  <- as.factor(substr(dat2$BY,  regexpr(" ",dat2$BY)+1,nchar(as.character(dat2$BY))))
rownames(dat2) <- NULL
dat2 <- dat2[,c("Env","GG","arimean")]
attr(dat2$arimean,"dimnames") <- NULL 

# lsmeans (stage I)
lsmeans <- matrix(list(),length(unique(dat$Env)),1)

require(asreml)
for (i in c(1:length(unique(dat$Env)))){
  mod <- asreml(fixed = Yield ~ Rep + GG,
                data  = subset(dat, Env==dat$Env[i]))
  
  pred <- predict(mod,classify="GG")$predictions$pvals[,c(1:3)]
  names(pred) <- c("GG","adjmean","se")
  pred        <- subset(pred,is.na(adjmean)==F)
  pred$ErrVar <- pred$se*pred$se
  pred$w      <- 1/pred$ErrVar
  pred$Env    <- as.factor(paste(unique(dat$Env[i])))
  pred        <- pred[,c("Env","GG","adjmean","se","ErrVar","w")]
  
  lsmeans[[i,1]] <- pred
}

# Combine into Mean-Table
require(plyr)
dat3 <- ldply(lsmeans, data.frame)
dat3 <- merge(dat2, dat3, by=c("Env","GG"))
dat3 <- merge(unique(dat[,!names(dat) %in% c("Rep","Yield")]),
               dat3, by=c("Env","GG"))

# Clean up
rm(list=ls()[ls() %in% c("pred","i","arimean","mod","lsmeans")])

# ...
# Skipped some SAS code
# ...

# Shukla's Stability Variance
#############################
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

# Clean up
rm(list=ls()[ls() %in% c("vc_names")])

# FA-model Eberhart-Russel
##########################
dat3 <- dat3[order(dat3$G,dat3$Env),] 

FAmod <- asreml(fixed   = adjmean ~ xj + tj,
                random  = ~ G + L + Y + L:G + Y:G + fa(G):Env,
                weights = w,
                family  = asreml.gaussian(dispersion=1.0),
                control = asreml.control(pworkspace=8e10 ,
                                         workspace=8e8, 
                                         maxiter=100),
                data    = dat3, ran.order = "user")

# Format VC estimates
FA_vc <- summary(FAmod)$varcomp[2:3]
FA_vc$VC <- rownames(FA_vc)
rownames(FA_vc) <- NULL
FA_vc$isg   <- regexpr(":Env!G.",FA_vc$VC)
FA_vc$isfa  <- regexpr(".fa1",FA_vc$VC) 
FA_vc$group <- NA
FA_vc[which(FA_vc$isg>1) ,"group"] <- "sigma"
FA_vc[which(FA_vc$isfa>1),"group"] <- "lambda"
FA_vc[which(is.na(FA_vc$group)==T),"VC"] <- substr(FA_vc[which(is.na(FA_vc$group)==T),"VC"],1,regexpr('!',FA_vc[which(is.na(FA_vc$group)==T),"VC"])-1)
FA_vc$VC <- gsub("fa\\(G):Env!G.","",FA_vc$VC)
FA_vc$VC <- gsub(".var","",FA_vc$VC)
FA_vc$VC <- gsub(".fa1","",FA_vc$VC)
FA_vc<-FA_vc[,c(3,1,2,6)]
colnames(FA_vc) <- c("VC","Estimate","StdErr","group")

# Transposed Output
G_var  <- subset(FA_vc, group=="sigma")[,-c(4)]
colnames(G_var) <- c("VC","Var_Est","Var_SE")
G_lam  <- subset(FA_vc, group=="lambda")[,-c(4)]
colnames(G_lam) <- c("VC","Lambda_Est","Lambda_SE")
FA_out <- merge(G_var, G_lam, by="VC")

# Clean up
rm(list=ls()[ls() %in% c("G_lam","G_var")])



######### Plots #########
#########################
require(ggplot2)
# GxE LS-means ~ Year
source("11 Plot Yield-Year.R")
adjmean_tj_plot
adjmean_xj_plot

# Shuklas Variances
source("12 Plot Shuklas Variances.R")
shuklaplot