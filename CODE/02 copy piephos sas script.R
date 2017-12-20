rm(list = ls())
aman <- read.delim("aman.txt")
aman$Y <- as.factor(aman$Y)
aman$Rep <- as.factor(aman$Rep)
aman$GG <- as.factor(paste(aman$Group,aman$G,sep="-"))

# arithmetic means
arimean <- tapply(aman$Yield,paste(aman$Env,aman$GG),mean,na.rm=F)
aman2   <- data.frame(BY=rownames(arimean),arimean=arimean)
aman2$Env <- as.factor(substr(aman2$BY,1,regexpr(" ",aman2$BY)-1))
aman2$GG  <- as.factor(substr(aman2$BY,  regexpr(" ",aman2$BY)+1,nchar(as.character(aman2$BY))))
rownames(aman2) <- NULL
aman2 <- aman2[,c("Env","GG","arimean")]
attr(aman2$arimean,"dimnames") <- NULL 

# lsmeans (stage I)
lsmeans <- matrix(list(),length(unique(aman$Env)),1)

require(asreml)
for (i in c(1:length(unique(aman$Env)))){
  mod <- asreml(fixed = Yield ~ Rep + GG,
                data  = subset(aman, Env==aman$Env[i]))
  
  pred <- predict(mod,classify="GG")$predictions$pvals[,c(1:3)]
  names(pred) <- c("GG","adjmean","se")
  pred        <- subset(pred,is.na(adjmean)==F)
  pred$ErrVar <- pred$se*pred$se
  pred$w      <- 1/pred$ErrVar
  pred$Env    <- as.factor(paste(unique(aman$Env[i])))
  pred        <- pred[,c("Env","GG","adjmean","se","ErrVar","w")]
  
  lsmeans[[i,1]] <- pred
}

# Combine into Mean-Table
require(plyr)
aman3 <- ldply(lsmeans, data.frame)
aman3 <- merge(aman2, aman3, by=c("Env","GG"))
aman3 <- merge(unique(aman[,!names(aman) %in% c("Rep","Yield")]),
               aman3, by=c("Env","GG"))

# ...
# Skipped some SAS code
# ...

# Shukla's Stability Variance
#############################
aman3 <- aman3[order(aman3$G,aman3$Env),] 

shuklamod <- asreml(fixed   = adjmean ~ xj + tj,
                    random  = ~ G + L + Y + L:G + Y:G + Y:L + at(G):Env, 
                    #rcov    = ~ at(G):Y:L, #Fehlermeldung?!
                    weights = w,
                    family  = asreml.gaussian(dispersion=1.0),
                    control = asreml.control(pworkspace=8e10 ,
                                             workspace=8e8, 
                                             maxiter=100),
                    data    = aman3, ran.order = "user")

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
shukla_vc$StabVar <- as.factor(shukla_vc$StabVar)
rownames(shukla_vc) <- NULL
shukla_vc <- shukla_vc[,c(3,1,2,5)]
colnames(shukla_vc) <- c("VC","Estimate","StdErr","V")

shuklaVs <- subset(shukla_vc, is.na(shukla_vc$V)==F)

# Display Results 
require(ggplot2)

ggplot(data=shuklaVs,
       aes(x=reorder(V,Estimate),
           y=Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=Estimate-StdErr, 
                    ymax=Estimate+StdErr)) +
  theme(axis.text.x  = element_text(angle=90, hjust=1),
        axis.title.x = element_blank()) +
  scale_y_continuous(name="Shukla's stability variance with s.e.",
                     limits = c(0,max(shuklaVs$Estimate+shuklaVs$StdErr)))
  
# FA-model Eberhart-Russel
##########################
aman3 <- aman3[order(aman3$G,aman3$Env),] 

FAmod <- asreml(fixed   = adjmean ~ xj + tj,
                random  = ~ G + L + Y + L:G + Y:G + fa(G):Env,
                weights = w,
                family  = asreml.gaussian(dispersion=1.0),
                control = asreml.control(pworkspace=8e10 ,
                                         workspace=8e8, 
                                         maxiter=100),
                data    = aman3, ran.order = "user")

# Format VC estimates
FA_vc <- summary(FAmod)$varcomp[2:3]
FA_vc$VC <- rownames(FA_vc)
rownames(FA_vc) <- NULL
FA_vc$isg  <- regexpr(":Env!G.",FA_vc$VC)
FA_vc$isfa <- regexpr(".fa1",FA_vc$VC) 
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
G_var <- subset(FA_vc, group=="sigma")[,-c(4)]
colnames(G_var) <- c("VC","Var_Est","Var_SE")
G_lam <- subset(FA_vc, group=="lambda")[,-c(4)]
colnames(G_lam) <- c("VC","Lambda_Est","Lambda_SE")
FA_t <- merge(G_var, G_lam, by="VC")

