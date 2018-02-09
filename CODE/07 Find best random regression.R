rm(list = ls())
require(asreml)
options(scipen=4)
#################
dataset <- "aman"
dataset <- "boro"
#################
dat3 <- read.delim(paste(dataset,"_means.txt",sep=""))
dat3 <- dat3[order(dat3$G,dat3$Env),]
dat3$Y <- as.factor(paste(dat3$Y))

# "Centered" and squareroot
dat3$sc_ri <- sqrt(dat3$ri-min(dat3$ri))
dat3$sc_tj <- sqrt(dat3$tj-min(dat3$tj)) 

unique(dat3$ri-min(dat3$ri))
unique(dat3$tj-min(dat3$tj))

# Set up model construction set
RP1 <- data.frame(part1 ="random=~ G + L + Y + Y:L + G:L + G:Y + G:Env",
       riGL  ="+ sc_ri:G:L",
       riGY  ="+ sc_ri:G:Y",
       tjGY  ="+ sc_tj:G:Y",
       riGYL ="+ sc_ri:G:Env",
       tjGYL ="+ sc_tj:G:Env")
RP <- as.formula(paste0(RP1[,1],RP1[,2],RP1[,3],RP1[,4],RP1[,5],RP1[,6]))

# Create List of Combinations
cols <- c(2,3,4,5,6)
comblist <- matrix(list(),length(cols)+1,1)
for(i in 1:length(cols)){
  comblist[[i]] <- combn(cols,i)
}

out_list  <- matrix(list(),6,10)

###############################
### Fit All Possible Models ###
###############################
# Full model
  # Get Starting Values
  Dmod <- asreml(fixed   = adjmean ~ ri + tj, RP,
             ran.order = "user",
             weights = w, start.values = T,
             family  = asreml.gaussian(dispersion=1.0),
             control = asreml.control(pworkspace=8e10,workspace=8e8,maxiter=100),
             data    = dat3) 
  
  # Manage Constraints
  Gpar <- Dmod$gammas.table
  Gpar$setU <- as.numeric(regexpr("sc_",Gpar$Gamma))
  Gpar[which(Gpar$setU==1),"Constraint"] <- "U"
  
  # Fit Model
  Dmod <- asreml(fixed   = adjmean ~ ri + tj, RP,
             ran.order = "user",
             weights = w, 
             G.param = Gpar,
             family  = asreml.gaussian(dispersion=1.0),
             control = asreml.control(pworkspace=8e10,workspace=8e8,maxiter=100),
             data    = dat3) 
  
  VC      <- summary(Dmod)$varcomp[,c(2,3,5)]
  VC$name <- substr(rownames(VC),1,regexpr('!',rownames(VC))-1)
  
  out <- data.frame(riGL  =round(VC[which(VC$name=="sc_ri:G:L"),  "component"],5),
                    riGY  =round(VC[which(VC$name=="sc_ri:G:Y"),  "component"],5),
                    tjGY  =round(VC[which(VC$name=="sc_tj:G:Y"),  "component"],5),
                    riGYL =round(VC[which(VC$name=="sc_ri:G:Env"),"component"],5),
                    tjGYL =round(VC[which(VC$name=="sc_tj:G:Env"),"component"],5))
  
  out$Conv <- Dmod$converge
  out$AIC  <- -2*Dmod$loglik + 2*length(Dmod$gammas) 
  out_list[[6,1]] <- out
  
  regnames <- c("sc_ri:G:L","sc_ri:G:Y","sc_tj:G:Y","sc_ri:G:Env","sc_tj:G:Env")
  
# All other models
for (i in 1:length(cols)){
  for (j in 1:dim(comblist[[i]])[2]){
    
    # Construng Random Part of Model
    loopRP1 <- RP1
    loopRP1[,comblist[[i]][,j]] <- ""
    loopRP <- as.formula(paste0(loopRP1[,1],loopRP1[,2],
                                loopRP1[,3],loopRP1[,4],
                                loopRP1[,5],loopRP1[,6]))

    # Get Starting Values
    Dmod <- asreml(fixed   = adjmean ~ ri + tj, loopRP,
                   ran.order = "user",
                   weights = w, start.values = T,
                   family  = asreml.gaussian(dispersion=1.0),
                   control = asreml.control(pworkspace=8e10,workspace=8e8,maxiter=100),
                   data    = dat3) 

    # Manage Constraints
    Gpar <- Dmod$gammas.table
    Gpar$setU <- as.numeric(regexpr("sc_",Gpar$Gamma))
    Gpar[which(Gpar$setU==1),"Constraint"] <- "U"
    Gpar[c(1:6),"component"] <- c(0.06,0.51,0.16,0.03,0.01,0.82)

    # Fit Model
    Dmod <- asreml(fixed   = adjmean ~ ri + tj, loopRP,
                   ran.order = "user",
                   weights = w, 
                   G.param = Gpar,
                   family  = asreml.gaussian(dispersion=1.0),
                   control = asreml.control(pworkspace=8e10,workspace=8e8,maxiter=100),
                   data    = dat3) 
    
    VC      <- summary(Dmod)$varcomp[,c(2,3,5)]
    VC$name <- substr(rownames(VC),1,regexpr('!',rownames(VC))-1)

    out <- data.frame(riGL  =if(loopRP1[,2]!=""){"yes"}else{""},
                      riGY  =if(loopRP1[,3]!=""){"yes"}else{""},
                      tjGY  =if(loopRP1[,4]!=""){"yes"}else{""},
                      riGYL =if(loopRP1[,5]!=""){"yes"}else{""},
                      tjGYL =if(loopRP1[,6]!=""){"yes"}else{""})
    
    for (v in 1:5){
    out[v] <- if(out[v]=="yes"){round(VC[which(VC$name==regnames[v]),"component"],5)}else{""} 
    }
    
    out$Conv <- Dmod$converge
    out$AIC  <- -2*Dmod$loglik + 2*length(Dmod$gammas) 
    out_list[[i,j]] <- out
    
  }
}

# Handle Output
output <- do.call(rbind.data.frame, out_list)
output <- output[order(output$AIC),]
output[which(output$Conv==FALSE),"AIC"] <- NA
write.table(output, 
            paste(dataset,"_ranregselect.txt", sep=""), 
            row.names = F, sep="\t")



