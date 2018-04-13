# FA-model Eberhart-Russel
##########################
require(asreml)
require(data.table)
require(nadiv)
require(stringr)

#################
dataset <- "aman"
#dataset <- "boro"
#################

dat3 <- fread(paste0(dataset,"_means.txt"), stringsAsFactors = T)
dat3 <- dat3[order(dat3$G,dat3$Env),]
dat3$Y <- as.factor(paste(dat3$Y))


FAmod <- asreml(fixed   = adjmean ~ ri + tj,
                random  = ~ G + L + Y +
                            L:G + Y:G + fa(G):Env,
                weights = w,
                family  = asreml.gaussian(dispersion=1.0),
                control = asreml.control(pworkspace=8e10 ,
                                         workspace=8e8, 
                                         maxiter=100),
                data    = dat3, ran.order = "user")

# all VC estimates
VC1 <- data.table(summary(FAmod)$varcomp,  keep.rownames="CovParm")
VC2 <- data.table(   aiCI(FAmod),          keep.rownames="CovParm")
VC  <- VC2[VC1, .(CovParm, LCL, estimate, UCL, std.error),on="CovParm"]
VC$is_var <- str_detect(VC$CovParm,".var")
VC$is_FA  <- str_detect(VC$CovParm,"fa\\(G\\):Env")
VC[, ("type"):= ifelse(VC$is_var, "sigma", "lambda")]
VC$CovParm[VC$is_FA==F] <- tstrsplit(VC$CovParm[VC$is_FA==F],"!",fixed=T)[[1]]
VC$CovParm[VC$is_FA]    <- str_match(VC$CovParm[VC$is_FA], levels(dat3$G))
VC[, names(VC)[2:5]:=round(.SD,4), .SDcols=names(VC)[2:5]]

# FA VC estimates (wide format)
VCout <- VC[VC$is_FA]
VCout <- dcast(VCout, CovParm ~ type, value.var=names(VC)[2:5])
setcolorder(VCout,c(1,grep("sigma" ,names(VCout)),
                      grep("lambda",names(VCout))))

# t-test for trends
FA_t_test=summary(FAmod,all=T)$coef.fixed
# Wald-test for trends
options(scipen=10) #change how numbers are displayed: NOT e-4 etc.
FA_wald_test=wald(FAmod, denDF = c("none"),ssType = c("incremental","conditional"))[,c(1,3,4)]
FA_wald_test[,c(2,3)] <- round(FA_wald_test[,c(2,3)],4)
FA_wald_test$`Pr(Chisq)` <- ifelse(FA_wald_test$`Pr(Chisq)`<0.0001, "<0.0001",paste(FA_wald_test$`Pr(Chisq)`))

# Residuals
dat3$residFA <- FAmod$residuals

write.table(VCout, paste(dataset,"_FA.txt", sep=""), row.names = F, sep="\t")
write.table(FA_wald_test, paste(dataset,"_FA_Wald_test.txt", sep=""), row.names = T, sep="\t")
write.table(dat3, paste(dataset,"_resid_FA.txt", sep=""), row.names = F, sep="\t")
