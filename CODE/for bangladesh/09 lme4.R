#' ---
#' author: Paul Schmidt
#' date: May 5th, 2018
#' ---

```{r echo=F}
# prepare data
setwd("D:/User/pschmidt/Desktop/GitHub/projectX/CODE/for bangladesh")
rm(list=ls())
library(data.table)
list.data <- readRDS("list_data.rds")
dat       <- list.data[["aman"]][["GxE means"]]
setorder(dat, G, Env)
```
### mixed model package comparison
```{r warning=F, message=F, results=F}
# Note that lme4 does not allow for fixing variance components. Thus,
# also in asreml, we run a model without any fixed variance components:

#### asreml
library(asreml)
asr <- asreml(fixed   = adj.mean ~ r.i + t.j,
              random  = ~ GG + L + Y + Y:L + L:GG + Y:GG,# + GG:Y:L, 
              weights = w,
              #family  = asreml.gaussian(dispersion=1.0),
              data    = dat, ran.order = "user")

#### lme4
library(lme4)
lme <- lmer(formula = adj.mean ~ r.i + t.j + 
            (1|GG) + (1|L) + (1|Y) + (1|Y:L) + (1|L:GG) + (1|Y:GG),# + (1|GG:Y:L),
            weights = w,
            data    = dat)

```
### Output comparison
#### variance components
```{r}
summary(asr)$varcomp[,c(2,3)]
data.frame(VarCorr(lme))[,c(1,4)]
```

#### fixed effects estimates
```{r}
summary(asr, all=T)$coef.fixed
fixef(lme)
```

#### test of fixed effects
```{r}
wald(asr, denDF=c("numeric"), ssType=c("conditional"))$Wald
anova(lme)
```