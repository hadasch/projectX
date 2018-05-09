setwd("C:/Users/Paul/Desktop/Bangladesh")

### Import
amanraw <- read.delim("20171212 Corrected Final GG Aman 09.08.2017.txt")
bororaw <- read.delim("20171212 Corrected Final GG Boro 09.08.2017.txt")

### individual changes
amanraw$t.k <- as.numeric(amanraw$Year)
bororaw$t.k <- as.numeric(substr(bororaw$Year,1,4))

### prepare lists for loop
dat.names <- c("aman", "boro")
list.data <- setNames(as.list(c(NA, NA)), dat.names)
set.names <- c("plot data raw", "plot data formatted", "plot data info", "GxE means")
datasets  <- setNames(as.list(c(NA, NA, NA, NA)), set.names)
list.data[["aman"]] <- datasets; list.data[["aman"]][["plot data raw"]] <- amanraw
list.data[["boro"]] <- datasets; list.data[["boro"]][["plot data raw"]] <- bororaw

### loop through aman & boro
for (s.d in dat.names) {
  
  d <- list.data[[s.d]][["plot data raw"]]
  
  # format raw data
    d$r.i   <- as.numeric(d$Year.of.release)
    d$Y     <- as.factor(d$Year)
    d$L     <- as.factor(d$Location)
    d$Env   <- as.factor(paste(d$Y,d$L,sep="-"))
    d$Rep   <- as.factor(d$Rep)
    d$G     <- as.factor(d$Variety)
    d$Group <- as.factor(d$Group)
    d$GG  <- as.factor(paste(d$Group,d$G,sep="-"))
    d <- d[,c("Y","t.k","r.i","L","Env","G","Rep","Group","GG","Yield")]
    d <- subset(d, is.na(d$Yield)==F)
    
    list.data[[s.d]][["plot data formatted"]] <- d
  
  # additinally export formatted data as text file
  write.table(x = list.data[[s.d]][["plot data formatted"]], sep="\t",
              file = paste0(s.d," formatted.txt"))
  
  # aggregate information on dataset's dimensions and levels
    columns <- names(d)[names(d)!="Yield"]
    info    <- matrix(list(),length(columns),2)
    rownames(info) <- columns
    for (x in c(1:length(columns))){
      info[[x,1]] <-    length(unique(as.factor(d[,c(x)])))
      info[[x,2]] <- data.frame(table(as.factor(d[,c(x)])))
      colnames(info) <- c("number","freq_list")
    }
    
    list.data[[s.d]][["plot data info"]] <- info

}

### Save output
saveRDS(list.data, file = "list_data.rds")