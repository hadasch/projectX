rm(list=ls())
library(asreml)
library(nadiv)
library(ggrepel)
setwd("C:/Users/Paul/Desktop/Bangladesh")
list.data <- readRDS("list_data.rds")

stab <- matrix(list(), 1, 2)
colnames(stab) <- c("aman", "boro")
rownames(stab) <- c("Stab.var")

for (s.d in names(list.data)) {
  dat <- list.data[[s.d]][["GxE means"]]
  
  asr <- asreml(fixed   = G.adj.mean ~ t.k,
                random  = ~ G + L + Y + Y:L + L:G + Y:G + at(G):Y:L, 
                weights = w.G,
                family  = asreml.gaussian(dispersion=1.0),
                data    = dat, ran.order = "user")
  
  # VC formatting
  vc1 <- data.table(summary(asr)$varcomp,  keep.rownames="CovParm")
  vc2 <- data.table(   aiCI(asr),          keep.rownames="CovParm")
  vc  <- vc2[vc1, .(CovParm, LCL, estimate, UCL, std.error), on="CovParm"]
  rm(vc1, vc2)
  is.at    <- str_detect(substr(vc$CovParm,1,3), "at\\(")
  vc <- vc[is.at] 
  vc$CovParm <- str_match(vc$CovParm, levels(dat$G))
  colnames(vc) <- c("G", "lcl.vc", "vc", "ucl.vc", "se.vc")
  
  # BLUP formatting
  BLUP <- predict(asr, classify="G", only="G")$predictions$pvals[,c(1,2,3)]
  colnames(BLUP) <- c("G", "BLUP", "se.BLUP")
  
  # combining values
  stab[[1,s.d]] <- data.table(merge(BLUP, vc, by="G"))
}

stab <- data.table(plyr::ldply(stab[1,], data.frame, .id="dataset"))


show <- stab$vc > 0.5
categ <- stab$G %in% c("BRRI dhan53", "BRRI dhan57", "BRRI dhan66")
stab$col <- "one"
stab[categ]$col <- "two"

p <- ggplot(data=stab, aes(x=vc, y=BLUP))
p <- p + geom_point()
p <- p + facet_grid(. ~ dataset)
p <- p + labs(ylab("BLUP per genotype ± s.e.")) 
p <- p + ylim(c(min(stab$BLUP-stab$se.BLUP),max(stab$BLUP+stab$se.BLUP)))
p <- p + xlab("Stability variance ± s.e.")
p <- p + xlim(c(0,max(stab$vc+stab$se.vc)))
p <- p + theme_bw()
p <- p + geom_pointrange(aes(ymin=BLUP-se.BLUP, ymax=BLUP+se.BLUP), color="black")
p <- p + geom_errorbarh(aes(xmin=vc-se.vc, xmax=vc+se.vc), color="black")
p <- p + geom_text_repel(data=stab[show], aes(label = G), nudge_y = 0.3)


pdf("mean_var.pdf", height=5, width=8)
p
dev.off()

system(paste0('open "', getwd(),'/','mean_var.pdf"'))


