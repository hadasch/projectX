rm(list=ls())
setwd("C:/Users/Paul/Desktop/Bangladesh")

list.data <- readRDS("list_data.rds")
s.d <- "aman"
dat <- list.data[[s.d]][["GxE means"]]

library(lme4)
library(lmerTest)

# noGroup   <- lmer(formula = G.ari.mean ~ r.i       + t.k + 
#                             (1|G) + (1|L) + (1|Y) + (1|Y:L) + (1|L:G) + (1|Y:G),
#                   data    = dat)

withGroup <- lmer(formula = G.ari.mean ~ 0 + Group + r.i:Group + t.k + 
                            (1|G) + (1|L) + (1|Y) + (1|Y:L) + (1|L:G) + (1|Y:G),
                  data    = dat)

summary(withGroup)
# summary(noGroup)

# Type III Analysis of Variance Table with Satterthwaite's method
# anova(noGroup)           # aman:  -    boro: sig
anova(withGroup)         # aman: sig   boro: sig

# Fixed effects. t-tests use Satterthwaite's method
# coef(summary(noGroup))   # aman:  -    boro: sig
coef(summary(withGroup)) # aman:  -    boro: sig
