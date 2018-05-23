setwd("~/GitHub/projectX/CODE/after Dhaka visit")

rm(list=ls())
library(lme4)
library(lmerTest)
library(emmeans)
library(plyr)

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

# within-site analyses #
########################

# create list to store result per site 
number.sites    <- length(levels(plotdata$L))
result.per.site <- matrix(list(), number.sites, 2) 
rownames(result.per.site) <- unique(plotdata$L)
colnames(result.per.site) <- c("G.mean", "var.comp")
result.per.site

# loop through sites
for (selected.site in c(1:number.sites)){
  mod <- lmer(formula = Yield ~ G + (1|Y) + (1|Y:G) + (1|Y:Rep),
              data    = subset(plotdata, L==levels(plotdata$L)[selected.site]))
  
  # adjusted genotype means
  m       <- emmeans(mod, "G")
  means   <- as.data.frame(m)[,c("G", "emmean")]
  names(means) <- c("G", "adj.mean")
  result.per.site[[selected.site, "G.mean"]] <- means
  
  # variance components
  result.per.site[[selected.site, "var.comp"]] <- data.frame(VarCorr(mod))[,c(1,4)]

  rm(mod, m, means)
}

# Aggregate individual site results into overall tables
G.means   <- data.table(plyr::ldply(result.per.site[,"G.mean"],   data.frame, .id="Site"))
variances <- data.table(plyr::ldply(result.per.site[,"var.comp"], data.frame, .id="Site"))

# genetic correlation
G.means.wide <- dcast(G.means, G ~ Site, value.var = "adj.mean")
G.means.wide <- merge(G.means.wide, unique(plotdata[, c("G", "Group")]), by="G")

library(GGally)
pcorr <- ggpairs(G.means.wide[,-c("G", "Group")], aes(alpha = 0.4))

library(devEMF)
emf("phenotypic correlation.emf", height=10, width=10)
  pcorr
dev.off()  

# variance components
library(ggplot2)
p <- ggplot(data=variances, aes(x=grp, y=vcov))
p <- p + geom_bar(stat="identity")
p <- p + facet_wrap(~ Site, ncol=5)
p

emf("variance components.emf", height=4, width=10)
  p
dev.off()  

# G.means[Site %in% c("Barisal", "Gazipur")]
# G.means.wide[, c("Barisal", "Gazipur")]
# 
# dat <- data.table(plotdata)
# dat.wide <- dcast(dat, Y + G + Rep  ~ L, value.var = "Yield")
# dat.wide[, c("Y", "G", "Rep", "Barisal", "Gazipur")]
