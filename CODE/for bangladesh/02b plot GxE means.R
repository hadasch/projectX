#' ---
#' author: Paul Schmidt
#' date: April 23rd, 2018
#' ---

# load required packages
library(ggplot2)
library(gridExtra)

list.data <- readRDS("list_data.rds")

### Prepare lists
p.t.l <- list()
p.r.l <- list()

# get highest overall mean for common y-axis formatting of plots
max.GxE <- max(max(list.data[["aman"]][["GxE means"]]$adj.mean),
               max(list.data[["boro"]][["GxE means"]]$adj.mean)) 

### loop through aman & boro
for (s.d in names(list.data)) {
 
  dat <- list.data[[s.d]][["GxE means"]]
  
  # define figure caption
  captiontext <- paste0("Genotype-by-environment means from ",s.d," dataset.\n",
                        "Red line: arithmetic mean per year. ",
                        "Black line: linear Regression y = a + bx.")
  
  # define overall plot features beforehand
  label.tj <- expression(paste("trial year ",t[j]))
  label.ri <- expression(paste("registration year ",r[i]))
  scatter  <- geom_jitter(width=0.25)
  caption  <- labs(caption=captiontext)
  yaxis    <- scale_y_continuous(name   ="genotype-by-environment mean",
                                 limits = c(0,max.GxE))
  theme    <- theme_bw() + theme(panel.grid.major.y = element_blank(),
                                 panel.grid.minor   = element_blank(),
                                 panel.border = element_blank(),
                                 axis.text.x  = element_text(angle=90,hjust=1,vjust=0.5),
                                 plot.caption = element_text(vjust=0, hjust=0.5),
                                 legend.position="top")
  
  # create scatter plot: t.i against GxE mean
  p.t <- ggplot(data=dat, aes(x=t.j, y=adj.mean, colour=r.i))
  p.t <- p.t + scatter + yaxis + caption + theme
  p.t <- p.t + scale_x_continuous(name   = label.tj,
                                  breaks = unique(dat$t.j))
  p.t <- p.t + scale_colour_gradient2(name     = label.ri,
                                      midpoint = round((min(dat$r.i)+max(dat$r.i))/2),
                                      breaks   = c(min(dat$r.i), 
                                                   round((min(dat$r.i)+max(dat$r.i))/2), 
                                                   max(dat$r.i)),
                                      low ="orangered3", mid ="mediumspringgreen", high="navy")
  p.t <- p.t + stat_summary(colour="red", size=1.5, fun.y=mean,  geom="line")
  p.t <- p.t + geom_smooth(color="black", size=1.5, method="lm", formula=y~x, se=F)
  p.t.l[[s.d]] <- p.t
  
  # create scatter plot: r.i against GxE mean
  p.r <- ggplot(data=dat, aes(x=r.i, y=adj.mean, colour=t.j))
  p.r <- p.r + scatter + yaxis + caption + theme
  p.r <- p.r + scale_x_continuous(name   = label.ri,
                                  breaks = unique(dat$r.i))
  p.r <- p.r + scale_colour_gradient(name=label.tj,
                                  breaks = c(min(dat$t.j), 
                                             round((min(dat$t.j)+max(dat$t.j))/2), 
                                             max(dat$t.j)),
                                  low="springgreen",high="darkgreen")
  p.r <- p.r + stat_summary(colour="red", size=1.5, fun.y=mean,  geom="line")
  p.r <- p.r + geom_smooth(color="black", size=1.5, method="lm", formula=y~x, se=F)
  p.r.l[[s.d]] <- p.r
  
}

### Save output
saveRDS(list(p.r.l, p.t.l), file = "GxE plots.rds")

### Export both plots as pdf
pdf(paste0("plots scatter tj-ri-GxEmean.pdf"), height=10, width=8)
  grid.arrange(p.r.l[["aman"]], p.t.l[["aman"]], ncol = 1, nrow = 2)
  grid.arrange(p.r.l[["boro"]], p.t.l[["boro"]], ncol = 1, nrow = 2)
dev.off()