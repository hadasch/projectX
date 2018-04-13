rm(list=ls())

datasets <- c("aman", "boro")
dat.nr   <- 1
dat      <- readRDS(paste0(datasets[dat.nr],".GxEmeans.rds"))

#dat <- droplevels(dat[Y %in% levels(dat$Y)[c(1:3)]])

library(asreml)
m.asre <- asreml(fixed   = adjmean ~ ri + tj,
                 random  = ~ G + L + Y + 
                             Y:L + L:G + Y:G + 
                             at(G):Env, 
                 weights = w,
                 family  = asreml.gaussian(dispersion=1.0),
                 data    = dat3, ran.order = "user")


library(sommer)
m.somm <- mmer2(fixed   = adj.mean ~ r.i + t.j,
                random  = ~ G + L + Y + 
                               Y:L + L:G + Y:G + 
                               at(G):Y:L, 
                   #init    = list(0.27, 0.19, 0.04, 0.45, 0.05, 0.02, 0.31, 0),
                   #forced = list( HOW DO I FIX ONLY SIGMA.E? ),
                   weights = w, 
                   data    = as.data.frame(dat))

vc <- data.table(plyr::ldply(m.shukla2$var.comp, data.frame, .id="component"))
names(vc)[2] <- "variance"


library(nlme) # cannot fix variances
m.nlme <- lme(adj.mean ~ r.i + t.j,
                  random  = ~ 1|G + 1|L ,#+ 1|Y + Y|L,# + 1|L:G + 1|Y:G + 1|Y:G:L,
                  weights = varIdent(form= ~1 | G),
                  data    = dat)
summary(m.shukla$modelStruct$varStruct)
  

library(lme4) # cannot fix variances
m.lmer <- lmer(adj.mean ~ r.i + t.j + 
                          (1|G) + (1|L) + (1|Y) + 
                          (1|Y:L) + (1|L:G) + (1|Y:G) + 
                          (G|Y:L),
                 weights = w,
                 data = dat)
summary(m.shukla)

################### Single Stage ######################
datasets <- c("aman", "boro")
dat.nr   <- 1
dat <- data.table(readRDS(paste0(datasets[dat.nr],".rds")))

library(lme4) # cannot fix variances
m.lmer <- lmer(Yield ~ r.i + t.j +
                 (1|GG) + (1|L) + (1|Y) + 
                 (1|Y:L) + (1|L:GG) + (1|Y:GG) + 
                 (1|GG:Env),
               data = dat)

summary(m.shukla)


