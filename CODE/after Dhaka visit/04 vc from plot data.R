setwd("~/GitHub/projectX/CODE/after Dhaka visit")

rm(list=ls())
library(lme4)
library(lmerTest)

raw <- read.delim("Boro dataset.txt")

raw$t.k   <- as.numeric(substr(raw$Year, 1, 4))
raw$Env   <- as.factor(paste(raw$Year, raw$Location, sep="-"))
raw$r.i   <- as.numeric(raw$Year.of.release)
raw$G     <- as.factor(raw$Variety)
raw$L     <- as.factor(raw$Location)
raw$Y     <- as.factor(raw$t.k)
raw$Rep   <- as.factor(raw$Rep)
raw$Group <- as.factor(raw$Group)

plotdata <- raw[ ,c("Y", "t.k", "r.i", "L", "Env", "G", "Rep", "Group", "Yield")]

# lme4
lme4 <-lmer(formula = Yield ~ 
                      (1|G) + (1|L) + (1|Y) +
                      (1|Y:L) + (1|Y:G) + (1|L:G) + 
                      (1|Y:L:G) + (1|Rep:Y:L),
            data    = plotdata)

vc.lme4 <- data.frame(VarCorr(lme4))[,c(1,4)]
vc.lme4$vcov <- round(vc.lme4$vcov, 3)
vc.lme4

# asreml
# library(asreml)
# asr <- asreml(fixed  = Yield ~ 1,
#               random = ~ G + L + Y + Y:L + L:G + Y:G + Y:L:G + Y:L:Rep, 
#               data   = plotdata, ran.order = "user")
# 
# vc.asr <- round(summary(asr)$varcomp[, c("component", "std.error")],3)
# vc.asr
