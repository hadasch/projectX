rm(list=ls())
library(ggplot2)
list.data <- readRDS("list_data.rds")

sel.dat <- "aman"
dat <- list.data[[sel.dat]][["GxE means",1]]

# define template plot features beforehand
label.tj <- expression(paste("trial year ",t[j]))
label.ri <- expression(paste("registration year ",r[i]))
scatter  <- geom_jitter(width=0.25)
title    <- ggtitle(paste(sel.dat))
yaxis    <- scale_y_continuous(name   ="genotype-by-environment mean",
                               limits = c(0,max(dat$adj.mean)))
theme    <- theme_bw() + theme(panel.grid.major.y = element_blank(),
                               panel.grid.minor   = element_blank(),
                               panel.border = element_blank(),
                               axis.text.x  = element_text(angle=90,hjust=1,vjust=0.5))

# Plot t.i against GxE mean
p.t <- ggplot(data=dat, aes(x=t.j, y=adj.mean, colour=r.i))
p.t <- p.t + scatter + title + yaxis + theme
p.t <- p.t + scale_x_continuous(name   = label.tj,
                                breaks = unique(dat$t.j))
p.t <- p.t + scale_colour_gradient2(name     = label.ri,
                                    midpoint = median(dat$r.i),
                                    breaks   = c(min(dat$r.i), median(dat$r.i), max(dat$r.i)),
                                    low ="orangered3", mid ="mediumspringgreen", high="navy")
p.t

# Plot r.j against GxE mean
p.r <- ggplot(data=dat, aes(x=r.i, y=adj.mean, colour=t.j))
p.r <- p.r + scatter + title + yaxis + theme
p.r <- p.r + scale_x_continuous(name   = label.ri,
                                breaks = unique(dat$r.i))
p.r <- p.r + scale_colour_gradient(name=label.tj,
                                breaks = c(min(dat$t.j),
                                           round(median(dat$t.j)),
                                           max(dat$t.j)),
                                low="springgreen",high="darkgreen")
p.r
