require(ggplot2)

shuklaplot <- ggplot(data=shukla_out,
       aes(x=reorder(V,Estimate),
           y=Estimate)) +
  geom_point() +
  ggtitle(dataset) +
  geom_errorbar(aes(ymin=Estimate-StdErr, 
                    ymax=Estimate+StdErr)) +
  theme(axis.text.x  = element_text(angle=90, hjust=1),
        axis.title.x = element_blank()) +
  scale_y_continuous(name="Shukla's stability variance with s.e.",
                     limits = c(ifelse(min(shukla_out$Estimate-shukla_out$StdErr)<0,
                                       min(shukla_out$Estimate-shukla_out$StdErr),0),
                                max(shukla_out$Estimate+shukla_out$StdErr)))



