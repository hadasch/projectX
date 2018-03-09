rm(list = ls())
setwd("D:/User/pschmidt/Desktop/GitHub/projectX/CODE")
require(asreml)
options(scipen=4)
#################
#dataset <- "aman"
dataset <- "boro"
#################
dat3 <- read.delim(paste(dataset,"_means.txt",sep=""))
dat3 <- dat3[order(dat3$G,dat3$Env),]
dat3$Y <- as.factor(paste(dat3$Y))

# "Centered" and squareroot
dat3$sc_ri <- sqrt(dat3$ri-min(dat3$ri))
dat3$sc_tj <- sqrt(dat3$tj-min(dat3$tj)) 

############
#   boro   #
#  ri:G:Y  #
############

# Full model parms
Full<- asreml(fixed   = adjmean ~ ri + tj, 
              random  = ~ G + L + Y + Y:L 
                        + G:L 
                        + G:Y  
                        + G:Y:L 
                        + sc_ri:G:Y
                        ,
       weights = w, start.values = T, ran.order = "user",
       family  = asreml.gaussian(dispersion=1.0),
       control = asreml.control(pworkspace=8e10,workspace=8e8,maxiter=100),
       data    = dat3) 

# Manage Constraints
Gpar <- Full$gammas.table
Gpar$setU <- as.numeric(regexpr("sc_",Gpar$Gamma))
Gpar[which(Gpar$setU==1),"Constraint"] <- "U"

# Full model
Full <- asreml(fixed   = adjmean ~ ri + tj, 
               random  = ~ G + L + Y + Y:L
                         + G:L 
                         + G:Y 
                         + G:Y:L 
                         + sc_ri:G:Y
                         ,
               G.param = Gpar,
               weights = w, ran.order = "user",
               family  = asreml.gaussian(dispersion=1.0),
               control = asreml.control(pworkspace=8e10,workspace=8e8,maxiter=100),
               data    = dat3) 

fullVC  <- summary(Full)$varcomp[2]
fullAIC <- -2*Full$loglik + 2*length(Full$gammas) 

###########################################################
# ri:G:Y #
##########
rGY   <- asreml(fixed   = adjmean ~ ri + tj, 
                random  = ~ G + L + Y + Y:L
                          + G:L 
                          + at(G):Y
                          + G:Y:L 
                          #+ sc_ri:G:Y
                          ,
                G.param = Gpar,
                weights = w, ran.order = "user",
                family  = asreml.gaussian(dispersion=1.0),
                control = asreml.control(pworkspace=8e10,workspace=8e8,maxiter=100),
                data    = dat3) 

rGYVC  <- summary(rGY)$varcomp[2]
rGYAIC <- -2*rGY$loglik + 2*length(rGY$gammas) 

# Format VC
rGYVC$G <- rownames(rGYVC)
rownames(rGYVC) <- NULL
rGYVC <- rGYVC[which(regexpr("at\\(G,",rGYVC$G)>0),]
rGYVC$G <- gsub("at\\(G, ","",rGYVC$G)
rGYVC$G <- gsub("\\):Y!Y.var","",rGYVC$G)
rGYVC <- merge(rGYVC, unique(dat3[c("G","ri","sc_ri")]), by="G")

# Simple Plot
plot(rGYVC$sc_ri, rGYVC$component)
abline(a=fullVC[          "G:Y!G.var","component"], 
       b=fullVC["sc_ri:G:Y!sc_ri.var","component"])
       #plot(rGYVC$ri, rGYVC$component)




# ggplot
rGYVC$xlab <- paste0("sqrt(",rGYVC$ri,")")

breaks <- unique(rGYVC[c("sc_ri","ri","xlab")])
breaks$exp <- as.expression(breaks$xlab)

a <- fullVC[          "G:Y!G.var","component"]
b <- fullVC["sc_ri:G:Y!sc_ri.var","component"]
size <- 20

# R2
rGYVC$SQx <- (rGYVC$sc_ri- mean(rGYVC$sc_ri))^2
rGYVC$SQy <- (rGYVC$component- mean(rGYVC$component))^2
(sum(rGYVC$SQx)*b^2)/sum(rGYVC$SQy)

require(ggplot2)
g <- ggplot() +
  geom_point(data=rGYVC, aes(y=component, x=sc_ri)) +
  theme_bw() +
  scale_x_continuous(name= expression(paste("Registration year ",r[i])), 
                     breaks = breaks$sc_ri, 
                     labels = breaks$ri,
                     sec.axis = sec_axis(~.*1, name=expression(sqrt(r[i] - r[min])),breaks=(c(0:7)))) +
  geom_abline(intercept=a,slope=b,color="blue") +
  ylab(expression(Var(GY))) +
  annotate("text",x=1.5,y=0.021,colour="blue",
           label=paste0("y=",round(a,3),round(b,5),"x")) +
  theme(axis.text.x     =element_text(angle=90, hjust=1,   vjust=0.5, size=0.5*size),
        axis.text.x.top =element_text(angle=0,  hjust=0.5, vjust=0.5, size=size),
        axis.title.x.top=element_text(size=size),
        axis.title.x    =element_text(size=size),
        axis.text.y     =element_text(size=0.5*size),
        axis.title.y    =element_text(size=size))
  
pdf("boro ranreg.pdf", width = 9, height = 4)
  print(g)
dev.off()

output <- rGYVC[,c(1,2,3,4)]
output$component <- round(output$component, 8)
output$sc_ri <- round(output$sc_ri, 2)

write.table(output, 
            paste(dataset,"_bororanreg_at(G).txt", sep=""), 
            row.names = F, sep="\t")

