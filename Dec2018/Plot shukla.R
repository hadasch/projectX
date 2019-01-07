rm(list=ls())
setwd("D:/User/shadasch/documents/GitHub/projectX")
dataset="boro"
ShukV=read.csv(paste(getwd(),"/Dec2018/","second data inspection means ",dataset," shukla.txt", sep=""))

require(ggplot2)

shuklaplot <- ggplot(data=ShukV,
                     aes(x=reorder(CovParm,estimate),
                         y=estimate)) +
  geom_point() +
  ggtitle(dataset) +
  geom_errorbar(aes(ymin=ShukV$LCL, 
                    ymax=ShukV$UCL)) +
  theme(axis.text.x  = element_text(angle=90, hjust=1),
        axis.title.x = element_blank()) +
  scale_y_continuous(name="Shukla's stability variance with CI",
                     limits = c(ifelse(min(ShukV$LCL)<0,min(ShukV$LCL),0),
                                max(ShukV$UCL)))


require(devEMF)
emf(paste(getwd(),"/Dec2018/","second data inspection means ",dataset," shukla.emf", sep=""),
    width = 8, height = 5)
print(shuklaplot)
dev.off()

