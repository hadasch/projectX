rm(list = ls()) #clear environment
wd <- getwd()   #save wd
setwd("..")     #change wd to parent folder of current wd
amanraw <- read.delim("DATA/20171212 Corrected Final GG Aman 09.08.2017.txt")
bororaw <- read.delim("DATA/20171212 Corrected Final GG Boro 09.08.2017.txt")
setwd(wd)       #change back to original wd

# Special formatting due to difference between datasets
amanraw$tj  <- as.numeric(amanraw$Year)
bororaw$tj  <- as.numeric(substr(bororaw$Year,1,4))

# Format function
format <- function(data){
  data$Y   <- as.factor(data$Year)
  data$L   <- data$Location
  data$Env <- as.factor(paste(data$Y,data$L,sep="-"))
  data$Rep <- as.factor(data$Rep)
  data$xj  <- as.numeric(data$Year.of.release)
  data$G   <- data$Variety
  data <- data[,c("Y","tj","xj","L","Env","G","Rep","Group","Yield")]
  data <- subset(data, is.na(data$Yield)==F)
  return(data)
}

# Info function
info <- function(data){
  columns <- colnames(data[,names(data)!="Yield"])
  info    <- matrix(list(),length(columns),2)
  rownames(info) <- columns
    for (x in c(1:length(columns))){
      info[[x,1]] <-    length(unique(as.factor(data[,c(x)])))
      info[[x,2]] <- data.frame(table(as.factor(data[,c(x)])))
      colnames(info) <- c("number","freq_list")
    }
  return(info)
}

# Run functions
aman <- format(amanraw)
boro <- format(bororaw)
info_aman <- info(aman)
info_boro <- info(boro)

# Export formatted datasets
write.table(aman,"aman.txt",sep="\t",row.names=F)
write.table(boro,"boro.txt",sep="\t",row.names=F)

# Group:G - sind doppelte möglich?