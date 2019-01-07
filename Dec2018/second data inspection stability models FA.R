rm(list=ls())
require(asreml)
require(data.table)
setwd("D:/User/shadasch/documents/GitHub/projectX")
dataset="boro"
means=read.csv(paste(getwd(),"/Dec2018/","second data inspection means ",dataset,".txt",sep="",collapse=""))[,-1]

means$Y=as.factor(means$Y)
#fa
mod=asreml(fixed=adjmean ~ tj+ri,
           random=~Y+G+L+Y:G+Y:L+G:L+Env:fa(G),
           weights=w,
           family  = asreml.gaussian(dispersion=1.0),
           control=asreml.control(pworkspace=8e8 ,
                                  workspace=8e8,
                                  maxiter=1000),
           data=means)
summary(mod,all=T)$varcomp

