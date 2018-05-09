#' ---
#' author: Paul Schmidt
#' date: April 23rd, 2018
#' ---

list.data <- readRDS("list_data.rds")

list.stabtrend <- list()

### Set up model construction set 
  RP1 <- data.frame(part1 ="random=~ GG + L + Y + Y:L + GG:L + GG:Y + GG:Env",
                    riGL  ="+ sc.ri:GG:L",
                    riGY  ="+ sc.ri:GG:Y",
                    tjGY  ="+ sc.tj:GG:Y",
                    riGYL ="+ sc.ri:GG:Env",
                    tjGYL ="+ sc.tj:GG:Env")
  RP <- as.formula(paste0(RP1[,1],RP1[,2],RP1[,3],RP1[,4],RP1[,5],RP1[,6]))
  cols <- c(2,3,4,5,6)
  comblist <- matrix(list(),length(cols)+1,1)
  for(i in 1:length(cols)){
    comblist[[i]] <- combn(cols,i)
    }
  out_list  <- matrix(list(),6,10)

for (sel.dat in names(list.data)) {
  dat     <- list.data[[sel.dat]][["GxE means"]]
  setorder(dat, G, Env)
  
  dat$sc.ri <- sqrt(dat$r.i-min(dat$r.i))
  dat$sc.tj <- sqrt(dat$t.j-min(dat$t.j))

### Get and manage covparm constraints
  Dmod <- asreml(start.values = T,
                 fixed   = adj.mean ~ r.i + t.j,
                 random  = RP, 
                 weights = w,
                 family  = asreml.gaussian(dispersion=1.0),
                 data    = dat, ran.order = "user")
  Gpar <- Dmod$gammas.table
  Gpar[str_detect(Gpar$Gamma,"sc\\."),"Constraint"] <- "U"

### Fit full model
  Dmod <- asreml(fixed   = adj.mean ~ r.i + t.j,
                 random  = RP, 
                 G.param = Gpar,
                 weights = w,
                 family  = asreml.gaussian(dispersion=1.0),
                 control = asreml.control(maxiter = 100),
                 data    = dat, ran.order = "user")
  
### Extract VCs
  VC         <- data.table(summary(Dmod)$varcomp[c(2,3,5)], keep.rownames="CovParm")
  VC$CovParm <- tstrsplit(VC$CovParm,"!",fixed=T)[[1]]
  
  VCwide <- dcast(VC[str_detect(CovParm,"sc.")], . ~ CovParm, value.var = "component")[,-1]
  VCwide$Conv <- Dmod$converge
  VCwide$loglik <- Dmod$loglik
  VCwide$n.par  <- length(Dmod$gammas) - dim(VC[constraint=="Fixed"|constraint=="Constrained"])[1]
  VCwide$AIC    <- -2 * Dmod$loglik + 2*(VCwide$n.par)
  VCwide$t.j <- round(summary(Dmod, all=T)$coef.fixed["t.j",1],4)
  VCwide$t.j_p <- wald(Dmod, denDF=c("numeric"), ssType=c("conditional"))$Wald["t.j","Pr"]
  VCwide$r.i <- round(summary(Dmod, all=T)$coef.fixed["r.i",1],4)
  VCwide$r.i_p <- wald(Dmod, denDF=c("numeric"), ssType=c("conditional"))$Wald["r.i","Pr"]
  out_list[[6,1]] <- VCwide
  
  regnames <- c("sc.ri:GG:L","sc.ri:GG:Y","sc.tj:GG:Y","sc.ri:GG:Env","sc.tj:GG:Env")
  #names(out_list[[6,1]]) <- regnames
  
### Run models with all other random effect combinations
  for (i in 1:length(cols)){
    for (j in 1:dim(comblist[[i]])[2]){
      
      # Construng Random Part of Model
      loopRP1 <- RP1
      loopRP1[,comblist[[i]][,j]] <- ""
      loopRP <- as.formula(paste0(loopRP1[,1],loopRP1[,2],
                                  loopRP1[,3],loopRP1[,4],
                                  loopRP1[,5],loopRP1[,6]))
      
      # Get Starting Values
      Dmod <- asreml(start.values = T,
                     fixed   = adj.mean ~ r.i + t.j, 
                     random  = loopRP,
                     weights = w,
                     family  = asreml.gaussian(dispersion=1.0),
                     control = asreml.control(maxiter=100),
                     data    = dat, ran.order = "user") 
      
      # Manage Constraints
      Gpar <- Dmod$gammas.table
      Gpar[str_detect(Gpar$Gamma,"sc."),"Constraint"] <- "U"
      Gpar[1:6,"component"] <- c(0.06,0.51,0.16,0.03,0.01,0.82) # starting values
      
      # Fit Model
      Dmod <- asreml(fixed   = adj.mean ~ r.i + t.j, 
                     random  = loopRP,
                     weights = w, 
                     G.param = Gpar,
                     family  = asreml.gaussian(dispersion=1.0),
                     control = asreml.control(maxiter=100),
                     data    = dat, ran.order = "user") 
      
      VC         <- data.table(summary(Dmod)$varcomp[c(2,3,5)], keep.rownames="CovParm")
      VC$CovParm <- tstrsplit(VC$CovParm,"!",fixed=T)[[1]]
      
      out <- data.frame(riGL  =if(loopRP1[,2]!=""){"yes"}else{""},
                        riGY  =if(loopRP1[,3]!=""){"yes"}else{""},
                        tjGY  =if(loopRP1[,4]!=""){"yes"}else{""},
                        riGYL =if(loopRP1[,5]!=""){"yes"}else{""},
                        tjGYL =if(loopRP1[,6]!=""){"yes"}else{""})
      
      for (v in 1:5){
        out[v] <- if(out[v]=="yes"){round(VC[CovParm==regnames[v],"component"],5)}else{""} 
      }
      
      out$Conv   <- Dmod$converge
      vc         <- data.table(summary(Dmod)$varcomp)
      out$loglik <- Dmod$loglik
      out$n.par  <- dim(vc[constraint!="Fixed" & constraint!="Constrained"])[1]
      out$AIC    <- -2 * Dmod$loglik + 2*(out$n.par)
      
      out$t.j   <- round(summary(Dmod, all=T)$coef.fixed["t.j",1],4)
      out$t.j_p <- round(wald(Dmod, denDF=c("numeric"), ssType=c("conditional"))$Wald["t.j","Pr"],4)
      out$r.i   <- round(summary(Dmod, all=T)$coef.fixed["r.i",1],4)
      out$r.i_p <- round(wald(Dmod, denDF=c("numeric"), ssType=c("conditional"))$Wald["r.i","Pr"],4)
      
      out_list[[i,j]] <- out
    }
  }  
  
  # Reformat full model output
    names(out_list[[6,1]]) <- names(out_list[[1,1]])
    out_list[[6,1]][,c(1:5,9:13)] <- round(out_list[[6,1]][,c(1:5,9:13)], 4)
  
  modcomp <- data.table(plyr::ldply(out_list, data.frame))
  modcomp[Conv==F, AIC:=NA]
  setorder(modcomp, AIC)
 
  list.stabtrend[[sel.dat]] <- modcomp
} 

### Save output  
saveRDS(list.stabtrend, file="list_stabtrend.rds")
