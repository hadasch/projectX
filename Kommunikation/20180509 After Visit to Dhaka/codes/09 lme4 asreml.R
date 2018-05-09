rm(list=ls())
setwd("C:/Users/Paul/Desktop/Bangladesh")

# Import data
list.data <- readRDS("list_data.rds")
s.d <- "boro"
dat <- list.data[[s.d]][["GxE means"]]
dat <- dat[, -c("GG", "GG.ari.mean", "GG.adj.mean", "w.GG")]

# Info on actual weights
summary(data.table(dat$w.G))

# Create pseudo observation
dat.extraline <- rbind(dat[1,], dat)
dat.extraline[1, "w.G"] <- 0.005


library(lme4)
library(lmerTest)
lme <- lmer(formula = G.ari.mean ~ r.i + t.k + 
                      (1|G) + (1|L) + (1|Y) + (1|Y:L) + (1|L:G) + (1|Y:G) + (1|Y:L:G),
            weights = w.G,
            data    = dat.extraline)



library(asreml)
asr <- asreml(fixed   = G.ari.mean ~ r.i + t.k,
              random  = ~ G + L + Y + Y:L + L:G + Y:G + G:Y:L, 
              weights = w.G,
              #family  = asreml.gaussian(dispersion=1.0),
              data    = dat.extraline, ran.order = "user")

# asreml
data.frame(VarCorr(lme))[,c(1,4)]

# lme4
summary(asr)$varcomp[,1:3]



