# rm(list = ls())
#################
dataset <- "aman"
#dataset <- "boro"
#################

rshuk <- read.delim(paste(dataset,"_resid_Shukla.txt",sep=""))
rFA   <- read.delim(paste(dataset,"_resid_FA.txt",sep=""))

resids <- merge(rshuk, rFA)

plot(resids$tj, resids$residShukla)
plot(resids$tj, resids$residFA)

####
rshuk$model <- "Shukla"
colnames(rshuk)[14] <- "resid"
rFA$model <- "FA"
colnames(rFA)[14]   <- "resid"
resids2 <- rbind(rshuk, rFA)


resplot <- ggplot(data=resids2,
       aes(x=tj, y=resid, colour=model)) +
  geom_hline(yintercept=0, colour="grey") +
  geom_point(position = position_jitterdodge(
              jitter.width = 0, dodge.width = 0.4)) +
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


