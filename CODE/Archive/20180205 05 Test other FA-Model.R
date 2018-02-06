# FA-model Eberhart-Russel
##########################
require(asreml)
options(scipen=10)

#################
dataset <- "aman"
#dataset <- "boro"
#################

dat3 <- read.delim(paste(dataset,"_means.txt",sep=""))
dat3 <- dat3[order(dat3$G,dat3$Env),]
dat3$Y <- as.factor(paste(dat3$Y))



# Env:G + tj:Env:G,

# 5 year pieces - heterogeneous variances
dat3$fiveyr <- as.factor(ceiling((dat3$tj-2000)/5))
unique(dat3[c("tj", "fiveyr")])

fiveyr <- asreml(fixed  = adjmean ~ ri + tj,
                random  = ~ G + L + Y + L:G + Y:G + 
                            diag(fiveyr):Env:G,
                weights = w,
                family  = asreml.gaussian(dispersion=1.0),
                control = asreml.control(pworkspace=8e10 ,
                                         workspace=8e8, 
                                         maxiter=100),
                data    = dat3, 
                ran.order = "user")

summary(fiveyr)$varcomp[2:3]
plot(dat3$tj,residuals(fiveyr, type=("pearson")))

# Random regression
dat3$sqrt_centered_tj <- sqrt(dat3$tj-2000)

randreg <- asreml(fixed  = adjmean ~ ri + tj,
                 random  = ~ G + L + Y + L:G + Y:G + 
                             Env:G + sqrt_centered_tj:Env:G,
                 weights = w,
                 family  = asreml.gaussian(dispersion=1.0),
                 control = asreml.control(pworkspace=8e10 ,
                                          workspace=8e8, 
                                          maxiter=100),
                 data    = dat3, 
                 ran.order = "user")

VC<-summary(randreg)$varcomp[,c(2,3,5)]
VC[,c(1,2)] <- round(VC[,c(1,2)],4)
VC
plot(dat3$tj,residuals(randreg, type=("pearson")))




