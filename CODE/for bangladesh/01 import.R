rm(list = ls()) #clear environment

### Individual import and non-uniform changes ###
amanraw <- read.delim("20171212 Corrected Final GG Aman 09.08.2017.txt")
bororaw <- read.delim("20171212 Corrected Final GG Boro 09.08.2017.txt")
amanraw$t.j <- as.numeric(amanraw$Year)
bororaw$t.j <- as.numeric(substr(bororaw$Year,1,4))

### loop through aman & boro ###
dat.names <- c("aman", "boro")
list.data <- setNames(as.list(c(NA, NA)), dat.names)
datasets  <- matrix(list(), 4, 1)
rownames(datasets) <- c("plot data raw", "plot data formatted", "plot data info", "GxE means")
list.data[[1]] <- datasets; list.data[[1]][[1,1]] <- amanraw
list.data[[2]] <- datasets; list.data[[2]][[1,1]] <- bororaw

for (sel.dat in dat.names) {
  
  d <- list.data[[sel.dat]][[1,1]]
  
  # format raw data
    d$r.i   <- as.numeric(d$Year.of.release)
    d$Y     <- as.factor(d$Year)
    d$L     <- as.factor(d$Location)
    d$Env   <- as.factor(paste(d$Y,d$L,sep="-"))
    d$Rep   <- as.factor(d$Rep)
    d$G     <- as.factor(d$Variety)
    d$Group <- as.factor(d$Group)
    d$GG  <- as.factor(paste(d$Group,d$G,sep="-"))
    d <- d[,c("Y","t.j","r.i","L","Env","G","Rep","Group","GG","Yield")]
    d <- subset(d, is.na(d$Yield)==F)
    
  list.data[[sel.dat]][[2,1]] <- d
  
  # export formatted data as text file
  write.table(x = list.data[[sel.dat]][[2,1]], sep="\t",
              file = paste0(sel.dat," formatted.txt"))
  
  # aggregate information on dataset's dimensions
    columns <- names(d)[names(d)!="Yield"]
    info    <- matrix(list(),length(columns),2)
    rownames(info) <- columns
    for (x in c(1:length(columns))){
      info[[x,1]] <-    length(unique(as.factor(d[,c(x)])))
      info[[x,2]] <- data.frame(table(as.factor(d[,c(x)])))
      colnames(info) <- c("number","freq_list")
    }
    
  list.data[[sel.dat]][[3,1]] <- info

}

saveRDS(list.data, file = "list_data.rds")

