#' ---
#' author: Paul Schmidt
#' date: April 23rd, 2018
#' ---

# load required packages
library(asreml)
library(nadiv)
library(stringr)
library(data.table)

list.data <- readRDS("list_data.rds")

list.results <- setNames(as.list(c(NA, NA)), names(list.data))

# loop through aman & boro
for (sel.dat in names(list.data)) {
  dat      <- list.data[[sel.dat]][["GxE means"]]
  setorder(dat, G, Env)
  
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
                           fa(GG):Env, #note: fa(G):Y:L does not converge
               weights = w,
               family  = asreml.gaussian(dispersion=1.0),
               control = asreml.control(maxiter=100),
               data    = dat, ran.order = "user")
  
  #### aggregate results
  m.names <- c("Basic", "Shukla", "FA")
  output  <- c("model object", "model fit", "fixed effect solutions", "fixed effect F-tests", 
               "variance components raw", "variance components formatted")
  results <- matrix(list(), length(output), length(m.names))
    rownames(results) <- output
    colnames(results) <- m.names
  results[[1,1]] <- Basic 
  results[[1,2]] <- Shukla 
  results[[1,3]] <- FA
  
  for (i in c(1:3)){
    # fixed effect solutions
    results[[3,i]] <- summary(results[[1,i]], all=T)$coef.fixed
    
    # fixed effect F-tests 
    results[[4,i]] <- wald(results[[1,i]], denDF=c("numeric"), ssType=c("conditional"))$Wald
    
    # variance components raw
      vc1 <- data.table(summary(results[[1,i]])$varcomp,  keep.rownames="CovParm")
      vc2 <- data.table(   aiCI(results[[1,i]]),          keep.rownames="CovParm")
    results[[5,i]] <- vc2[vc1, .(CovParm, LCL, estimate, UCL, std.error), on="CovParm"]
    rm(vc1, vc2)
    
    # AIC
    loglik    <- summary(results[[1,i]])$loglik
    vc        <- data.table(summary(results[[1,i]])$varcomp)
    n.par.est <- dim(vc[constraint!="Fixed" & constraint!="Constrained"])[1]
    m2logLik  <- -2 * loglik
    AIC       <- -2 * loglik + 2*(n.par.est)
    out <- data.frame(n.par.est, loglik, m2logLik, AIC)
    results[[2,i]] <- out
    rm(out, vc, loglik, n.par.est, m2logLik, AIC)
  }  
  
  # variance component formatting
  for (i in c(1:3)){
    vc <- results[[5,i]]
    vc[, names(vc)[2:5]:=round(.SD,4), .SDcols=names(vc)[2:5]]
      is.var   <- str_detect(vc$CovParm,"\\.var|R!variance")
      is.at    <- str_detect(substr(vc$CovParm,1,3), "at\\(")
      is.FA    <- str_detect(substr(vc$CovParm,1,3), "fa\\(")
      is.basic <- is.at==F & is.FA==F
    if (T %in% is.at) {
      vc$CovParm[is.at] <- str_match(vc$CovParm[is.at], levels(dat$GG))
    }  
    vc$CovParm[is.basic] <- tstrsplit(vc$CovParm[is.basic],"!",fixed=T)[[1]]
    results[[6,i]] <- vc
    if (T %in% is.FA) {
      vc$CovParm[is.FA] <- str_match(vc$CovParm[is.FA], levels(dat$GG))
      vc[, type:= ifelse(is.var, "sigma", "lambda")]
      FA.wide.all <- vc[is.FA, c("CovParm", "type", "LCL", "estimate", "UCL", "std.error")]
      FA.wide     <- dcast(FA.wide.all, CovParm ~ type, value.var="estimate")
      top <- vc[is.FA==F, c("CovParm", "type", "estimate")]
      names(top)[2:3] <- c("lambda", "sigma"); top$lambda <- NA
      FA.wide <- rbind(top, FA.wide); rm(top)
      FA.wide[, lambda:= as.numeric(paste(lambda))]
      results[[6,i]] <- FA.wide
    }
    rm(is.var, is.FA, is.at, is.basic)
  }
  
  list.results[[sel.dat]] <- results

}

### Save output
saveRDS(list.results, file = "list_results.rds")
