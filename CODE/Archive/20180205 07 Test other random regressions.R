require(asreml)
options(scipen=4)
#################
dataset <- "aman"
#dataset <- "boro"
#################
dat3 <- read.delim(paste(dataset,"_means.txt",sep=""))
dat3 <- dat3[order(dat3$G,dat3$Env),]
dat3$Y <- as.factor(paste(dat3$Y))

# "Centered" and squareroot
dat3$sc_ri <- sqrt(dat3$ri-min(dat3$ri))
dat3$sc_tj <- sqrt(dat3$tj-min(dat3$tj)) 
# "Centered"
dat3$c_ri   <- dat3$ri-min(dat3$ri)
dat3$c_tj   <- dat3$tj-min(dat3$tj)

Mod4 <- asreml(fixed   = adjmean ~ ri + tj,
               random  = ~ G + L + Y + 
                           G:L   + sc_ri:G:L   + 
                                    c_ri:G:L   +
                           G:Y   + sc_ri:G:Y   + sc_tj:G:Y   +
                                    c_ri:G:Y   +  c_tj:G:Y   +
                           G:Env + sc_ri:G:Env + sc_tj:G:Env +
                                    c_ri:G:Env +  c_tj:G:Env,
               ran.order = "user",
               weights = w,
               family  = asreml.gaussian(dispersion=1.0),
               control = asreml.control(pworkspace=8e10,workspace=8e8,maxiter=100),
               data    = dat3,
               start.values = T) 

Gpar <- Mod4$gammas.table
Gpar$Constraint[c(5:6,8:11,13:16)] <- "U"

Mod4 <- asreml(fixed   = adjmean ~ ri + tj,
               random  = ~ G + L + Y + 
                 G:L   + sc_ri:G:L   + 
                 c_ri:G:L   +
                 G:Y   + sc_ri:G:Y   + sc_tj:G:Y   +
                 c_ri:G:Y   +  c_tj:G:Y   +
                 G:Env + sc_ri:G:Env + sc_tj:G:Env +
                 c_ri:G:Env +  c_tj:G:Env,
               ran.order = "user",
               G.param = Gpar,
               weights = w,
               family  = asreml.gaussian(dispersion=1.0),
               control = asreml.control(pworkspace=8e10,workspace=8e8,maxiter=100),
               data    = dat3) 

-2* Mod4$loglik / length(Mod4$gammas)

