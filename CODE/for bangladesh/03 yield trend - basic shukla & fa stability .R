rm(list=ls())
library(asreml)
library(nadiv)
library(stringr)

list.data <- readRDS("list_data.rds")

list.results <- setNames(as.list(c(NA, NA)), names(list.data))

# loop through aman & boro
for (sel.dat in names(list.data)) {
  dat      <- list.data[[sel.dat]][["GxE means",1]]
  setorder(dat, G, Env)
  
  ### yield trend analysis ###
  ############################
  
  # basic model
  Basic <- asreml(fixed   = adj.mean ~ r.i + t.j,
                  random  = ~ GG + L + Y + Y:L + L:GG + Y:GG + 
                              GG:Y:L, 
                  weights = w,
                  family  = asreml.gaussian(dispersion=1.0),
                  data    = dat, ran.order = "user")
  
  # shukla's stability variances 
  Shukla <- asreml(fixed   = adj.mean ~ r.i + t.j,
                   random  = ~ GG + L + Y + Y:L + L:GG + Y:GG + 
                               at(GG):Y:L, 
                   weights = w,
                   family  = asreml.gaussian(dispersion=1.0),
                   data    = dat, ran.order = "user")
  
  # factor-analytic
  FA <- asreml(fixed   = adj.mean ~ r.i + t.j,
               random  = ~ GG + L + Y + L:GG + Y:GG + 
                           fa(GG):Env, #fa(G):Y:L does not converge
               weights = w,
               family  = asreml.gaussian(dispersion=1.0),
               control = asreml.control(maxiter=100),
               data    = dat, ran.order = "user")
  
  ##### aggregate results #####
  m.names <- c("Basic", "Shukla", "FA")
  output  <- c("model object", "fixed effect solutions", "fixed effect F-tests", 
               "variance components raw", "variance components formatted")
  results <- matrix(list(), 5, 3)
    rownames(results) <- output
    colnames(results) <- m.names
  results[[1,1]] <- Basic 
  results[[1,2]] <- Shukla 
  results[[1,3]] <- FA
  
  for (i in c(1:3)){
    results[[2,i]] <- summary(results[[1,i]], all=T)$coef.fixed
    results[[3,i]] <- wald(results[[1,i]], denDF=c("numeric"), ssType=c("conditional"))$Wald
      vc1 <- data.table(summary(results[[1,i]])$varcomp,  keep.rownames="CovParm")
      vc2 <- data.table(   aiCI(results[[1,i]]),          keep.rownames="CovParm")
    results[[4,i]] <- vc2[vc1, .(CovParm, LCL, estimate, UCL, std.error), on="CovParm"]
    rm(vc1, vc2)
  }  
  
  # variance component formatting
  for (i in c(1:3)){
    vc <- results[[4,i]]
    vc[, names(vc)[2:5]:=round(.SD,4), .SDcols=names(vc)[2:5]]
      is.var   <- str_detect(vc$CovParm,"\\.var|R!variance")
      is.at    <- str_detect(substr(vc$CovParm,1,3), "at\\(")
      is.FA    <- str_detect(substr(vc$CovParm,1,3), "fa\\(")
      is.basic <- is.at==F & is.FA==F
    if (T %in% is.at) {
      vc$CovParm[is.at] <- str_match(vc$CovParm[is.at], levels(dat$GG))
    }  
    vc$CovParm[is.basic] <- tstrsplit(vc$CovParm[is.basic],"!",fixed=T)[[1]]
    results[[5,i]] <- vc
    if (T %in% is.FA) {
      vc$CovParm[is.FA] <- str_match(vc$CovParm[is.FA], levels(dat$GG))
      vc[, type:= ifelse(is.var, "sigma", "lambda")]
      FA.wide.all <- vc[is.FA, c("CovParm", "type", "LCL", "estimate", "UCL", "std.error")]
      FA.wide     <- dcast(FA.wide.all, CovParm ~ type, value.var="estimate")
      top <- vc[is.FA==F, c("CovParm", "type", "estimate")]
      names(top)[2:3] <- c("lambda", "sigma"); top$lambda <- NA
      FA.wide <- rbind(top, FA.wide); rm(top)
      results[[5,i]] <- FA.wide
    }
    rm(is.var, is.FA, is.at, is.basic)
  }
  
  list.results[[sel.dat]] <- results

}

list.results
list.results$aman[["variance components formatted", "Basic"]]
