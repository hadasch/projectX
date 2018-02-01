dat <- read.delim("aman.txt")
dat$Y   <- as.factor(dat$Y)
dat$Rep <- as.factor(dat$Rep)
dat$GG  <- as.factor(paste(dat$Group,dat$G,sep="-"))

# arithmetic means
arimean  <- tapply(dat$Yield,paste(dat$Env,dat$GG),mean,na.rm=F)
dat2     <- data.frame(BY=rownames(arimean),arimean=arimean)
dat2$Env <- as.factor(substr(dat2$BY,1,regexpr(" ",dat2$BY)-1))
dat2$GG  <- as.factor(substr(dat2$BY,  regexpr(" ",dat2$BY)+1,nchar(as.character(dat2$BY))))
rownames(dat2) <- NULL
dat2 <- dat2[,c("Env","GG","arimean")]
attr(dat2$arimean,"dimnames") <- NULL 

# lsmeans (stage I)
lsmeans <- matrix(list(),length(unique(dat$Env)),1)

require(asreml)
for (i in c(1:length(unique(dat$Env)))){
  mod <- asreml(fixed = Yield ~ Rep + GG,
                data  = subset(dat, Env==dat$Env[i]))
  
  pred <- predict(mod,classify="GG")$predictions$pvals[,c(1:3)]
  names(pred) <- c("GG","adjmean","se")
  pred        <- subset(pred,is.na(adjmean)==F)
  pred$ErrVar <- pred$se*pred$se
  pred$w      <- 1/pred$ErrVar
  pred$Env    <- as.factor(paste(unique(dat$Env[i])))
  pred        <- pred[,c("Env","GG","adjmean","se","ErrVar","w")]
  
  lsmeans[[i,1]] <- pred
}

# Combine into Mean-Table
require(plyr)
dat3 <- ldply(lsmeans, data.frame)
dat3 <- merge(dat2, dat3, by=c("Env","GG"))
dat3 <- merge(unique(dat[,!names(dat) %in% c("Rep","Yield")]),
              dat3, by=c("Env","GG"))

write.table(dat3, "aman_means.txt", row.names = F, sep="\t")
