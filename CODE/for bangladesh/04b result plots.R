rm(list=ls())
setwd("D:/User/pschmidt/Desktop/GitHub/projectX/CODE/for bangladesh")
library(ggplot2)
library(ggrepel)
library(gridExtra)

# import publish results
list.publish <- readRDS("list_publish.rds")

p.s <- list()

# define template plot features beforehand
shuk.form <- expression((GLY)[ijk], v[ijk])

max.shuk <- max(max(list.publish[["aman"]][[2,1]][!is.na(lambda)]$var.shukla),
                max(list.publish[["boro"]][[2,1]][!is.na(lambda)]$var.shukla))
max.FA   <- max(max(list.publish[["aman"]][[2,1]][!is.na(lambda)]$var.FA),
                max(list.publish[["boro"]][[2,1]][!is.na(lambda)]$var.FA))
min.lamb <- min(min(list.publish[["aman"]][[2,1]][!is.na(lambda)]$lambda),
                min(list.publish[["boro"]][[2,1]][!is.na(lambda)]$lambda))
max.lamb <- max(max(list.publish[["aman"]][[2,1]][!is.na(lambda)]$lambda),
                max(list.publish[["boro"]][[2,1]][!is.na(lambda)]$lambda))
yaxis    <- scale_y_continuous(name   ="Shukla's stability variance",
                               limits = c(0,max.shuk))
xaxis    <- scale_x_continuous(name   ="FA stability variance",
                               limits = c(0,max.FA))

theme    <- theme_bw() + theme(panel.grid.major = element_line(colour = "grey", size=0.3),
                               panel.grid.minor = element_line(colour = "grey", size=0.1),
                               panel.border = element_blank(),
                               plot.caption = element_text(vjust=0, hjust=0.5))
# legend   <- guides(colour = guide_legend("title"), size = guide_legend("title"),
#                    color = guide_legend("title"))




for (s.d in names(list.publish)) {
  
captiontext <- paste0("Stability variances for ",s.d," dataset.\n")
                      # "y-axis: Shukla's stability variance\n",
                      # "x-axis and symbol color: FA variances")
caption  <- labs(caption=captiontext)
  
stab   <- list.publish[[s.d]][[2,1]][!is.na(lambda)]
unstab <- stab$var.shukla>0.5 & stab$var.FA>0.5 & !is.na(stab$lambda)
zero.s <- stab$var.shukla<0.01
zero.F <- stab$var.FA<0.01

p.s[[s.d]] <- ggplot(data=stab, aes(x=var.FA, y=var.shukla))
p.s[[s.d]] <- p.s[[s.d]] + geom_point(aes(color=lambda, size=2))
p.s[[s.d]] <- p.s[[s.d]] + geom_text_repel(data=stab[unstab|zero.s|zero.F,], 
                                           aes(label = CovParm), nudge_y = 0.3)
p.s[[s.d]] <- p.s[[s.d]] + yaxis + xaxis + theme + caption + scale_size_continuous(guide=F)
p.s[[s.d]] <- p.s[[s.d]] + scale_colour_gradient(name   = expression(lambda[i]),
                                                 low    = "orange", high ="green4")
}

saveRDS(p.s, file = "stability plots.rds")

pdf(paste0("both plots scatter stability variances.pdf"), height=10, width=5)
  grid.arrange(p.s[[1]], p.s[[2]], ncol = 1, nrow = 2)
dev.off(); #system(paste0('open "', getwd(),'/','both plots scatter stability variances.pdf"'))