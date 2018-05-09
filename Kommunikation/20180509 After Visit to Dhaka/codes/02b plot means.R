rm(list=ls())
setwd("C:/Users/Paul/Desktop/Bangladesh")

# load required packages
library(ggplot2)
library(gridExtra)

list.data <- readRDS("list_data.rds")
aman <- list.data[["aman"]][["GxE means"]]
aman$dataset <- "aman"
boro <- list.data[["boro"]][["GxE means"]]
boro$dataset <- "boro"
dat <- rbind(aman, boro)

# define overall plot features beforehand
max.GxE <- max(dat$G.adj.mean)
captiontext <- paste0("Genotype-by-environment means.\n",
                      "Red line: arithmetic mean per year. ",
                      "Black line: linear Regression y = a + bx.",
                      "\nNote that the black line does neither equal the genetic (r.i) nor the agronomic (t.k) trend ")
label.tj <- expression(paste("trial year ",t[j]))
label.ri <- expression(paste("registration year ",r[i]))
scatter  <- geom_jitter(width=0.25)
caption  <- labs(caption=captiontext)
yaxis    <- scale_y_continuous(name   ="genotype-by-environment mean",
                               limits = c(0,max.GxE))
theme    <- theme_bw() + theme(#panel.grid.major.y = element_blank(),
                               panel.grid.minor   = element_blank(),
                               panel.border = element_blank(),
                               axis.text.x  = element_text(angle=90,hjust=1,vjust=0.5),
                               plot.caption = element_text(vjust=0, hjust=0.5),
                               legend.position="top")

# create scatter plot: r.i against GxE mean
p.r <- ggplot(data=dat, aes(x=r.i, y=G.adj.mean, colour=t.k))
p.r <- p.r + scatter + yaxis + caption + theme
p.r <- p.r + scale_x_continuous(name   = label.ri,
                                breaks = unique(dat$r.i))
p.r <- p.r + scale_colour_gradient(name=label.tj,
                                   breaks = c(min(dat$t.k), 
                                              round((min(dat$t.k)+max(dat$t.k))/2), 
                                              max(dat$t.k)),
                                   low="springgreen",high="darkgreen")
p.r <- p.r + stat_summary(colour="red", size=1.5, fun.y=mean,  geom="line")
p.r <- p.r + geom_smooth(color="black", size=1.5, method="lm", formula=y~x, se=F)
p.r <- p.r + facet_grid(Group ~ dataset)


pdf(paste0("plot genetic trend.pdf"), height=8, width=8)
 p.r
dev.off()

library(devEMF)
emf(paste0("plot genetic trend.emf"), height=8, width=8)
p.r
dev.off()


# create scatter plot: t.i against GxE mean
p.t <- ggplot(data=dat, aes(x=t.k, y=G.adj.mean, colour=r.i))
p.t <- p.t + scatter + yaxis + caption + theme
p.t <- p.t + scale_x_continuous(name   = label.tj,
                                breaks = unique(dat$t.k))
p.t <- p.t + scale_colour_gradient2(name     = label.ri,
                                    midpoint = round((min(dat$r.i)+max(dat$r.i))/2),
                                    breaks   = c(min(dat$r.i),
                                                 round((min(dat$r.i)+max(dat$r.i))/2),
                                                 max(dat$r.i)),
                                    low ="orangered3", mid ="mediumspringgreen", high="navy")
p.t <- p.t + stat_summary(colour="red", size=1.5, fun.y=mean,  geom="line")
p.t <- p.t + geom_smooth(color="black", size=1.5, method="lm", formula=y~x, se=F)
p.t <- p.t + facet_grid(Group ~ dataset)
p.t

pdf(paste0("plot agronomic trend.pdf"), height=8, width=8)
p.t
dev.off()

library(devEMF)
emf(paste0("plot agronomic trend.emf"), height=8, width=8)
p.t
dev.off()