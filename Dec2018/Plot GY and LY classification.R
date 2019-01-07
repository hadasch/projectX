rm(list=ls())
setwd("D:/User/shadasch/documents/GitHub/projectX")
dataset="aman"
data=read.csv(paste(getwd(),"/Dec2018/","second data inspection ",dataset,".txt", sep=""))
data=data[order(data$tj),]
require(devEMF)
emf(paste(getwd(),"/Dec2018/","second data inspection ",dataset," classification.emf", sep=""),
    width = 10, height = 6)

####################################################################################################
par(xpd=T) #to write outside plot region
par(mar=c(1,1,1,1)*5)
par(cex.axis=1.2)
par(mfrow=c(1,2))

require(plyr)
cols=cbind(grey(.8),grey(.6),grey(.4),grey(.2),grey(0))

#genotyp vs jahr    
image_data=matrix(NA,length(unique(data$G)),length(unique(data$Y)))
colnames(image_data)=unique(data$Y)
rownames(image_data)=unique(data$G)
for (k in 1:length(unique(data$Y))){
  data_k=data[which(data$Y==unique(data$Y)[k]),]
  image_data[which(rownames(image_data)%in%count(data_k$G)$x),k]=count(data_k$G)$freq
}

image_x=c(1:length(unique(data$Y)))
image_y=c(1:length(unique(data$G)))
image(x=image_x,
      y=image_y,
      z=t(image_data),
      xlab=NA,#"year",
      ylab=NA,#"genotyp",
      axes=F,
      col=cols)

abline(v=image_x+.5,col="white")
abline(h=image_y+.5,col="white")

axis(1,at=(image_x),labels=unique(data$tj),cex.axis=.8,las=2)
axis(3,at=range(image_x),labels=c("",""))
axis(2,at=(image_y),labels=unique(data$G),cex.axis=.7,las=1)
axis(4,at=range(image_y),labels=c("",""))

legend(x=max(image_x)*1.05,y=mean(image_y)*1.1,
       legend=round(cbind(min(image_data,na.rm=TRUE),
                          mean(image_data,na.rm=TRUE),
                          max(image_data,na.rm=TRUE))),#range(image_data,na.rm=T),
       fill=cols[c(1,3,5)],
       bty="n",
       cex=1.2)



#location vs jahr    
image_data=matrix(NA,length(unique(data$L)),length(unique(data$Y)))
colnames(image_data)=unique(data$Y)
rownames(image_data)=unique(data$L)
for (k in 1:length(unique(data$Y))){
  data_k=data[which(data$Y==unique(data$Y)[k]),]
  image_data[which(rownames(image_data)%in%count(data_k$L)$x),k]=count(data_k$L)$freq
}

image_x=c(1:length(unique(data$Y)))
image_y=c(1:length(unique(data$L)))
image(x=image_x,
      y=image_y,
      z=t(image_data),
      xlab=NA,#"year",
      ylab=NA,#"genotyp",
      axes=F,
      col=cols)

abline(v=image_x+.5,col="white")
abline(h=image_y+.5,col="white")

axis(1,at=(image_x),labels=unique(data$tj),cex.axis=.8,las=2)
axis(3,at=range(image_x),labels=c("",""))
axis(2,at=(image_y),labels=unique(data$L),cex.axis=.8,las=1)
axis(4,at=range(image_y),labels=c("",""))

legend(x=max(image_x)*1.05,y=mean(image_y)*1.1,
       legend=round(cbind(min(image_data,na.rm=TRUE),
                          mean(image_data,na.rm=TRUE),
                          max(image_data,na.rm=TRUE))),#range(image_data,na.rm=T),
       fill=cols[c(1,3,5)],
       bty="n",
       cex=1.2)

dev.off()
