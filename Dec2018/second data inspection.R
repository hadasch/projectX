rm(list=ls())
require(data.table)
setwd("D:/User/shadasch/documents/GitHub/projectX")
dataset="aman"
dat0=read.csv(paste(getwd(),"/CODE/",dataset,".txt",sep="",collapse=""),sep="\t")
dat1=read.csv(paste(getwd(),"/Dec2018/",dataset," plotdat cleaned.txt",sep="",collapse=""))

dat0=dat0[which(is.na(dat0$Yield)==F),]
dat1=dat1[which(is.na(dat1$Yield)==F),]

#Spalten umbenennen
colnames(dat0)
colnames(dat1)=c("Y","L","G","Group","Rep","Yield","GD")

#jahre in dat0 und dat1
unique(dat0$Y)
unique(dat1$Y)

if (dataset=="boro") {dat0$Y=gsub("-20","-",dat0$Y)}

dat01=dat0[which(substr(dat0$Y,1,4)=="2015"),]
dat11=dat1[which(substr(dat1$Y,1,4)=="2015"),]
dat01$source1="1"
dat11$source2="2"
dat01_11=merge(dat01,dat11,by=c("Y","L","G","Rep"),all=T)
#dat01_11$Yield.x-dat01_11$Yield.y

# remove 2015 from old data
dat0=dat0[-which(substr(dat0$Y,1,4)=="2015"),]


# registration years for new data
dat1=merge(dat1,dat0[!duplicated(dat0$G),c("G","ri")],by="G",all=T)
# set registration years of new genotypes to 2015
dat1[which(is.na(dat1$ri)==T),"ri"]=2015
# trial year for new data
dat1$tj=as.numeric(substr(dat1$Y,1,4))
# Env for new data
dat1$Env=paste(dat1$Y,dat1$L,sep="-")

dat0$GD=NA

setcolorder(dat0, c("Y","L","Env","G","tj","ri","Rep","Group","Yield","GD")) # column order
setcolorder(dat1, c("Y","L","Env","G","tj","ri","Rep","Group","Yield","GD")) # column order

#inspection of genotype-group combinations
g0=cbind(unique(paste(dat0$G,sep=" ")))
gg0=cbind(unique(paste(dat0$G,dat0$Group,sep=" ")))

cbind(gg0[order(gg0),])
if(dataset=="aman")  {
  dat0[which(dat0$G=="BRRI dhan53"),"Group"]="Short"
  dat0[which(dat0$G=="BRRI dhan57"),"Group"]="Short"
}

g1=cbind(unique(paste(dat1$G,sep=" ")))
gg1=cbind(unique(paste(dat1$G,dat1$Group,sep=" ")))


#groups of new genotypes of new data
g_new=cbind(as.character(unique(dat1$G)[which((unique(dat1$G)%in%unique(dat0$G))==F)]))
g_new_g=dat1[which(dat1$G%in%g_new),c("G","Group")]
g_new_g=g_new_g[!duplicated(g_new_g$G),c("G","Group")]

if(dataset=="boro")  {
  lm=lm(GD~G-1,dat1[which(dat1$G%in%g_new),])
  g_new_g[-which(rownames(cbind(coef(lm)))=="GBRRI dhan69"),"Group"]="Short"
  g_new_g[which(rownames(cbind(coef(lm)))=="GBRRI dhan69"),"Group"]="Long"
  
}


#groups of new genotypes of old data
g_old_g=dat0[!duplicated(dat0$G),c("G","Group")]
g_g=rbind(g_new_g,g_old_g)

#new groups of new data
dat1=dat1[,-which(colnames(dat1)=="Group")]
dat1=merge(dat1,g_g,by="G")#,all=T)
#dat0=dat0[which(is.na(dat0$Yield)==F),]

data=rbind(dat0,dat1)

#insepction of genotype-group combinations
g01=cbind(unique(paste(data$G,sep=" ")))
gg01=cbind(unique(paste(data$G,data$Group,sep=" ")))
gx01=cbind(unique(paste(data$G,data$xi,sep=" ")))


write.csv(data,paste(getwd(),"/Dec2018/","second data inspection ",dataset,".txt",sep="",collapse=""))
