# rm(list = ls())
#################
dataset <- "aman"
dataset <- "boro"
#################

dat3 <- read.delim(paste(dataset,"_means.txt",sep=""))
dat3 <- dat3[order(dat3$G,dat3$Env),]
dat3$Y <- as.factor(paste(dat3$Y))

require(asreml)
# Get residuals from standard model
standard <- asreml(fixed   = adjmean ~ ri + tj,
                random  = ~ G + L + Y + L:G + Y:G + at(G):Env,
                ran.order = "user",
                weights = w,
                family  = asreml.gaussian(dispersion=1.0),
                control = asreml.control(pworkspace=8e10 ,
                                         workspace=8e8, 
                                         maxiter=100),
                data    = dat3)

rstd <- dat3
rstd$residstd <- standard$residuals

rshuk <- read.delim(paste(dataset,"_resid_Shukla.txt",sep=""))
rFA   <- read.delim(paste(dataset,"_resid_FA.txt",sep=""))

resids <- merge(rshuk, rFA)
resids <- merge(resids, rstd)

plot(resids$tj, resids$residShukla)
plot(resids$tj, resids$residFA)
plot(resids$tj, resids$residstd)

####
rshuk$model <- "Shukla"
colnames(rshuk)[14] <- "resid"
rFA$model <- "FA"
colnames(rFA)[14]   <- "resid"
rstd$model <- "Standard"
colnames(rstd)[14]   <- "resid"
resids2 <- rbind(rshuk, rFA, rstd)

require(ggplot2)
resplot <- ggplot(data=resids2,
       aes(x=tj, y=resid, colour=model)) +
  geom_hline(yintercept=0, colour="grey") +
  geom_point(position = position_jitterdodge(
              jitter.width = 0.1, dodge.width = 0.8)) +
  ggtitle(dataset) +
  scale_x_continuous(name="Trial Year",
                     breaks=seq(min(resids2$tj),
                                max(resids2$tj),1)) +
  scale_y_continuous(name="Residual",
                     limits = c(-max(abs(resids2$resid)),
                                max(abs(resids2$resid)))) +
  
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.border = element_blank(),
        axis.text.x  = element_text(angle=90,hjust=1,vjust=0.5))

require(devEMF)
emf(paste(dataset,"_residuals.emf", sep=""), width = 8, height = 5)
print(resplot)
dev.off()


