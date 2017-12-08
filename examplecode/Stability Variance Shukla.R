require(asreml)
wd <- paste("D:/User/pschmidt/bwSyncAndShare/Yield Stability/examplecode")
setwd(wd)
GxEmeans <- read.delim("GxE_means.txt")

###################################
### Shukla's stability variance ### 
###################################
GxEmeans <- GxEmeans[order(GxEmeans$G),]

shuklamod <- asreml(fixed   = GxE_mean ~ -1 + G,
                    random  = ~ E, 
                    rcov    = ~ at(G):E,
                    control = asreml.control(pworkspace=8e8 ,
                                             workspace=8e8, 
                                             maxiter=100),
                    data    = GxEmeans)

# extract variance components
shukla_vc  <- summary(shuklamod)$varcomp[2:3]
temp       <- rownames(shukla_vc)[2:dim(shukla_vc)[1]]
shukla     <- data.frame(G       = substr(temp,3,regexpr('!',temp)-1),
                         StabVar = shukla_vc[c(2:dim(shukla_vc)[1]),1])

# obtain G means
G_mean <- predict(shuklamod, classify="G")$pred$pvals[,c('G','predicted.value')]

# finish
shukla <- merge(shukla, G_mean, by="G")
colnames(shukla) <- c("G","StabVar","G_mean")
shukla <- shukla[order(shukla$StabVar),]
head(shukla)
