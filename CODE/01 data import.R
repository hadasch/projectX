wd <- getwd()
setwd("..")   #change wd to parent folder of current wd
Aman <- read.delim("DATA/20171208 Stability trial variance components Aman.txt")
Boro <- read.delim("DATA/20171208 Stability trial variance components Boro.txt")
setwd(wd)     #change back to original wd

Aman$x <- "Aman"
Boro$x <- "Boto"
dat <- rbind(Aman,Boro)
colnames(dat) <- c("GL_v","GY_v","GLY_v","E_v","L","Y","rep","sed_yield","x")

head(dat) 
# Correct dataset? In SAS there is "Year_of_release" in first data step
# and different file name in import.        