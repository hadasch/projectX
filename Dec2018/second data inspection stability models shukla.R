rm(list=ls())
require(asreml)
require(data.table)
setwd("D:/User/shadasch/documents/GitHub/projectX")
dataset="boro"
means=read.csv(paste(getwd(),"/Dec2018/","second data inspection means ",dataset,".txt",sep="",collapse=""))[,-1]

means$Y=as.factor(means$Y)
#shukla
means=means[order(means$G),]
shuklamod=asreml(fixed=adjmean ~ tj+ri,
           random=~Y+G+L+Y:G+Y:L+G:L+Env:at(G),
           #rcov=~units,
           weights=w,
           family  = asreml.gaussian(dispersion=1.0),
           data=means)

AIC=-2*shuklamod$loglik + 2*(length(shuklamod$gammas)) 

require(nadiv)
require(stringr)

# Format VC estimates
VC1 <- data.table(summary(shuklamod)$varcomp,  keep.rownames="CovParm")
VC2 <- data.table(   aiCI(shuklamod),          keep.rownames="CovParm")
VC  <- VC2[VC1, .(CovParm, LCL, estimate, UCL, std.error),on="CovParm"]
VC$CovParm <- tstrsplit(VC$CovParm,"!",fixed=T)[[1]]
VC$is_Shuk <- str_detect(VC$CovParm,"at\\(G,")
VC$CovParm[VC$is_Shuk] <- str_match(VC$CovParm[VC$is_Shuk], levels(means$G))
VC[, names(VC)[2:5]:=round(.SD,4), .SDcols=names(VC)[2:5]]
ShukV <- VC[VC$is_Shuk]

# t-test for trends
shukla_t_test=summary(shuklamod,all=T)$coef.fixed
# Wald-test for trends
options(scipen=10) #change how numbers are displayed: NOT e-4 etc. 
shukla_wald_test=wald(shuklamod, denDF = c("none"),ssType = c("incremental","conditional"))[,c(1,3,4)]
shukla_wald_test[,c(2,3)] <- round(shukla_wald_test[,c(2,3)],4)

# Residuals
means$residShukla <- shuklamod$residuals

# Export
write.csv(ShukV, paste(getwd(),"/Dec2018/","second data inspection means ",dataset," shukla.txt", sep=""))
write.csv(shukla_wald_test, paste(getwd(),"/Dec2018/","second data inspection means ",dataset," shukla wald test.txt", sep=""))
