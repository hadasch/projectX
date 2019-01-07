rm(list=ls())
require(asreml)
require(stringi)
setwd("D:/User/shadasch/documents/GitHub/projectX")
dataset="boro"
data=read.csv(paste(getwd(),"/Dec2018/","second data inspection ",dataset,".txt",sep="",collapse=""))


data$Y=as.factor(data$Y)
data$L=as.factor(data$L)
data$G=as.factor(data$G)
data$Rep=as.factor(data$Rep)

#centering
a_t=min(data$tj)
a_x=min(data$ri)

data$sqrt_tj_cs=((data$tj-a_t))^.5
data$sqrt_ri_cs=((data$ri-a_x))^.5
data$sqrt_aij=(data$tj-data$ri)^.5

data=data[order(data$Env),]
base=c("Y+G+L+Y:L+G:L+Y:G+Y:G:L+Y:G:L+at(Env):Rep")
mod_base=asreml(fixed = Yield ~ ri +tj,
                as.formula(paste("random  = ~ ",base,sep="+",collapse="")),
                rcov=~units:at(Env),
                control = asreml.control(pworkspace=8e10,workspace=8e8,maxiter=5000),
                data = data)
AIC_base=-summary(mod_base)$loglik+2*length(mod_base$gammas)
History=c()
History=rbind(History,cbind(base,"",AIC_base))


kandidate_terms=c("Y:sqrt_tj_cs","G:sqrt_ri_cs",
                  "Y:L:sqrt_tj_cs","G:L:sqrt_ri_cs","Y:G:sqrt_tj_cs","Y:G:sqrt_ri_cs",
                  "Y:G:L:sqrt_tj_cs","Y:G:L:sqrt_ri_cs")
I=length(kandidate_terms)
for (i in 1:I) {
  
  for (t in kandidate_terms) {
    #t=kandidate_terms[1]
    mod=asreml(fixed = Yield ~ ri +tj,
               as.formula(paste("random  = ~ ",base,t,sep="+",collapse="")),
               rcov=~units:at(Env),
               start.values=TRUE,
               control = asreml.control(pworkspace=8e10,workspace=8e8,maxiter=5000),
               data = data)
    
    #if (Art=="GW")                              {path=paste("D:/user/shadasch/Documents/Hadasch/BSA Projekt/Variance trends homogen model JOS/Results/",tolower(Art)," ",Stufe," ",Sortiment," ",Zeilen," ",MM_list[MM],sep="",collapse="")}
    #if (Art%in%c("WW","RW","GS","M_KM","M_SM")) {path=paste("D:/user/shadasch/Documents/Hadasch/BSA Projekt/Variance trends homogen model JOS/Results/",tolower(Art)," ",Stufe," ",Sortiment," ",MM_list[MM],sep="",collapse="")}
  #  
    #VCs1=read.csv(paste(path," variance components.txt",sep="",collapse=""))
    #VCs1=VCs1[,which(colnames(VCs1)%in%c("rownames.vc.","gamma"))]
    #colnames(VCs1)=c("Gamma","Value")
    #VCs1$Gamma=as.character(VCs1$Gamma)
    
    Gpar=mod$gammas.table
    # constraint for sqrt term
    Gpar[which(stri_detect_fixed(Gpar$Gamma,c("sqrt"))==TRUE),"Constraint"]="U"
    # constraint for term associated with sqrt
    main=gsub("\\:sqrt.*","",as.character(Gpar[which(stri_detect_fixed(Gpar$Gamma,c("sqrt"))==TRUE),"Gamma"]))
    #if ("units"%in%main) {
    #  Gpar[which(gsub("\\!.*","",Gpar$Gamma)%in%main),"Constraint"]="U"
    #  Gpar[which(gsub("\\!.*","",Gpar$Gamma)%in%"R"),"Constraint"]="U"
    #}
    #if (!"units"%in%main) {
      Gpar[which(gsub("\\!.*","",Gpar$Gamma)%in%main),"Constraint"]="U"
    #}
    
    #Gpar=Gpar[,-which(colnames(Gpar)=="Value")]
    #Gpar=merge(Gpar,VCs1,by=c("Gamma"),all=T) 
    #Gpar[which(is.na(Gpar$Value)==TRUE),"Value"]=0.1
    
    mod=asreml(fixed = Yield ~ ri +tj,
               as.formula(paste("random  = ~ ",base,t,sep="+",collapse="")),
               rcov=~units:at(Env),
               G.param=Gpar,
               control = asreml.control(pworkspace=8e10,workspace=8e8,maxiter=5000),
               data = data)
    
    if (mod$converge==TRUE)  {AIC_mod=-summary(mod)$loglik+2*length(mod$gammas)}
    if (mod$converge==FALSE) {AIC_mod=10^10}
    
    
    if (t==kandidate_terms[1]) {AICs=(cbind(t,AIC_mod))}
    if (t!=kandidate_terms[1]) {AICs=rbind(AICs,cbind(t,AIC_mod))}
    
    History=rbind(History,cbind(base,t,AIC_mod))
    print(t)
  }#t
  
  AICs=as.data.frame(AICs)
  AICs$AIC_mod=as.numeric(as.character(AICs$AIC_mod))
  min_AIC=AICs[which(AICs$AIC_mod==min(AICs$AIC_mod)),"AIC_mod"]
  min_AIC_term=AICs[which(AICs$AIC_mod==min(AICs$AIC_mod)),"t"]
  
  print(i)
  if (min_AIC>AIC_base) {break}
  
  if (min_AIC<AIC_base) {
    AIC_base=min_AIC
    base=paste(base,min_AIC_term,sep="+",collapse="")
    kandidate_terms=kandidate_terms[-which(kandidate_terms==min_AIC_term)]
  }
  History=rbind(History,cbind(base,"",AIC_base))
  
  print("################################################################")
  print("################################################################")
  print("################################################################")
  print(base)
  print("################################################################")
  print(kandidate_terms)
  print("################################################################")
  print("################################################################")
  print("################################################################")
  
}#i
