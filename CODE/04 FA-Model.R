# FA-model Eberhart-Russel
##########################

#################
#dataset <- "aman"
dataset <- "boro"
#################

dat3 <- read.delim(paste(dataset,"_means.txt",sep=""))
dat3 <- dat3[order(dat3$G,dat3$Env),] 

FAmod <- asreml(fixed   = adjmean ~ xj + tj,
                random  = ~ G + L + Y + L:G + Y:G + Y:G:L,
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

# t-test for trends
FA_t_test=summary(FAmod,all=T)$coef.fixed
#Wald-test for trends
FA_wald_test=wald(FAmod, denDF = c("none"),ssType = c("incremental","conditional"))#[4,4]


# Transposed Output
G_var  <- subset(FA_vc, group=="sigma")[,-c(4)]
colnames(G_var) <- c("VC","Var_Est","Var_SE")
G_lam  <- subset(FA_vc, group=="lambda")[,-c(4)]
colnames(G_lam) <- c("VC","Lambda_Est","Lambda_SE")
FA_out <- merge(G_var, G_lam, by="VC")


write.table(FA_out, paste(dataset,"_FA.txt", sep=""), row.names = F, sep="\t")
write.table(FA_wald_test, paste(dataset,"_FA_Wald_test.txt", sep=""), row.names = T, sep="\t")

