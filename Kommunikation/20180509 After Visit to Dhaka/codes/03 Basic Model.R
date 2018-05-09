rm(list=ls())
setwd("C:/Users/Paul/Desktop/Bangladesh")

list.data <- readRDS("list_data.rds")
s.d <- "aman"
dat <- list.data[[s.d]][["GxE means"]]

library(lme4)
library(lmerTest)



withGroup <- lmer(formula = G.adj.mean ~ 0 + Group + r.i:Group + t.k + 
                            (1|G) + (1|L) + (1|Y) + (1|Y:L) + (1|L:G) + (1|Y:G),# + (1|GG:Y:L),
                  weights = w.G, # "Residual" Variance >> 1
                  data    = dat)





summary(withGroup)

# Type III Analysis of Variance Table with Satterthwaite's method
anova(withGroup)         # aman: sig   boro: sig

# Fixed effects. t-tests use Satterthwaite's method
coef(summary(withGroup)) # aman:  -    boro: sig


library(asreml)
asr <- asreml(fixed   = G.adj.mean ~ 0 + Group + r.i:Group + t.k,
              random  = ~ G + L + Y + Y:L + L:G + Y:G, 
              weights = w.G,
              family  = asreml.gaussian(dispersion=1.0),
              data    = dat, ran.order = "user")

wald(asr, denDF=c("numeric"), ssType=c("conditional"))

asr.int <- coef(asr, list=T)$`Group`
asr.slope <- coef(asr, list=T)$`r.i:Group`
data.table(eff=rownames(asr.int), Int=asr.int, Slope=asr.slope)

# noGroup   <- lmer(formula = G.adj.mean ~ r.i       + t.k + 
#                     (1|G) + (1|L) + (1|Y) + (1|Y:L) + (1|L:G) + (1|Y:G),# + (1|GG:Y:L),
#                   weights = w.G, # "Residual" Variance >> 1
#                   data    = dat)
# summary(noGroup)
# anova(noGroup)         # aman:  -    boro: sig
# coef(summary(noGroup)) # aman:  -    boro: sig
