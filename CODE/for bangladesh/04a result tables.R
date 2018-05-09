rm(list=ls())
setwd("D:/User/pschmidt/Desktop/GitHub/projectX/CODE/for bangladesh")
library(plyr)

# import raw results
list.results    <- readRDS("list_results.rds")
list.stabtrends <- readRDS("list_stabtrend.rds")

# create list with publishable results
list.publish <- setNames(as.list(c(NA, NA)), names(list.results))
m.names <- colnames(list.results[[1]])
output  <- c("performance trends", "stability variances", "model fit")
publish <- matrix(list(), length(output), 1)
rownames(publish) <- output
colnames(publish) <- "Overall"


for (sel.dat in names(list.results)) {
  
  # Result table: fixed effects
  temp <- list()
   for (sel.mod in m.names) {
      sol <- list.results[[sel.dat]][[3,sel.mod]]; sol <- sol[1:2,1:2]; 
    tes <- list.results[[sel.dat]][[4,sel.mod]]; tes <- tes[2:3,]
    sol <- as.data.table(sol, keep.rownames="Trend")
    tes <- as.data.table(tes, keep.rownames="Trend")
    res <- sol[tes, on="Trend"][,c("Trend", "solution", "std error", "Pr")]
    res[, c("Absolute", "SE", "p-Value"):= list(round(solution, 5), 
                                                round(`std error`,3), 
                                                round(Pr, 4))]
    res[, c("solution", "std error", "Pr"):= list(NULL, NULL, NULL)]
    res$Trend <- mapvalues(res$Trend, c("r.i","t.j"), c("Genetic","Agronomic"))
    temp[[sel.mod]] <- res
   }
  publish[[1,"Overall"]] <- data.table(plyr::ldply(temp, data.frame, .id="Model"))
  
  # Result table: stability variances
  vcsh <- list.results[[sel.dat]][[6,"Shukla"]]; vcsh <- vcsh[,c("CovParm", "estimate")]
  vcfa <- list.results[[sel.dat]][[6,"FA"]]; 
  stab <- vcsh[vcfa, on="CovParm"]
  setnames(stab, c("estimate", "sigma"), c("var.shukla", "var.FA"))
  publish[[2,"Overall"]] <- stab
  
  # Result table: model fit
  temp <- list()
  for (sel.mod in m.names) {
    temp[[sel.mod]] <- list.results[[sel.dat]][[2,sel.mod]]
  }
  publish[[3,"Overall"]] <- data.table(plyr::ldply(temp, data.frame, .id="Model"))
  colnames(publish[[3,"Overall"]])[4] <- "-2logLik"
  
  list.publish[[sel.dat]] <- publish
}

saveRDS(list.publish, file = "list_publish.rds")

list.publish[["aman"]][[3,1]]
list.results[["aman"]][["variance components formatted", "FA"]]

list.publish[["aman"]][3,1]
list.stabtrend[["aman"]][1:5,]
             