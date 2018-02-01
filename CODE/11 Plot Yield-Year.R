require(ggplot2)

# x-Axis: Trial Year
adjmean_tj_plot <- ggplot(data=dat3,
aes(x=tj, y=adjmean, colour=xj)) +
geom_jitter(width=0.25) +
scale_y_continuous(name="LS-mean: genotype per environment",
                   limits = c(0,max(dat3$adjmean))) +
scale_x_continuous(name="Trial Year",
                   breaks=seq(min(dat3$tj),max(dat3$tj),1)) +
theme_bw() +
theme(panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x  = element_text(angle=90,hjust=1,vjust=0.5)) +
scale_colour_gradient2("Registration\nyear",
                       midpoint = median(dat3$xj),
                       breaks = c(min(dat3$xj),
                                  median(dat3$xj),
                                  max(dat3$xj)),
                       low ="orangered3",
                       mid ="mediumspringgreen",
                       high="navy")
  
# x-Axis: Registration Year
adjmean_xj_plot <- ggplot(data=dat3,
       aes(x=xj, y=adjmean, color=tj)) +
geom_jitter(width=0.25) +
scale_y_continuous(name="lsmean genotype per environment",
                   limits = c(0,max(dat3$adjmean))) +
scale_x_continuous(name="Registration Year",
                   breaks=seq(min(dat3$xj),max(dat3$xj),1)) +
theme_bw() +
theme(panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x  = element_text(angle=90,hjust=1,vjust=0.5)) +
scale_colour_gradient("Trial year",
                      breaks = c(min(dat3$tj),
                                 round(mean(dat3$tj)),
                                 max(dat3$tj)),
                      low="springgreen",high="darkgreen")
