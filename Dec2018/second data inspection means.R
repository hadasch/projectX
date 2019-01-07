rm(list=ls())
require(asreml)
setwd("D:/User/shadasch/documents/GitHub/projectX")
dataset="boro"
data=read.csv(paste(getwd(),"/Dec2018/","second data inspection ",dataset,".txt",sep="",collapse=""))[,-1]
#write.csv(data,paste("D:/User/shadasch/desktop/",dataset," full.txt",sep="",collapse=""))

data$Y=as.factor(data$Y)
data$G=as.factor(data$G)
data$L=as.factor(data$L)
data$Env=as.factor(data$Env)
data$Rep=as.factor(data$Rep)

#means by env
means=c()
for (i in unique(data$Env)) {
  #i=unique(data$Env)[1]
  data_i=data[which(data$Env==i),]
  mod=asreml(fixed=Yield ~ Rep + G, data=data_i)
  
  pred=predict(mod,classify="G")$predictions$pvals[,c(1:3)]
  names(pred)=c("G","adjmean","se")
  pred       =subset(pred,is.na(adjmean)==F)
  pred$ErrVar=pred$se*pred$se
  pred$w     =1/pred$ErrVar
  pred       =pred[,c("G","adjmean","se","ErrVar","w")]
  
  data_i=data_i[which(duplicated(data_i$G)==F),]
  data_i=data_i[,-which(colnames(data_i)%in%c("Yield","Rep","GD","G"))]
  
  means=rbind(means,cbind(data_i,pred))
}

data=write.csv(means,paste(getwd(),"/Dec2018/","second data inspection means ",dataset,".txt",sep="",collapse=""))




