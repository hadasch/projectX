rm(list=ls())
setwd("D:/User/pschmidt/Desktop/GitHub/projectX/Dec2018")
setwd("D:/User/shadasch/documents/GitHub/projectX/Dec2018")
library(data.table)
aman <- fread("HPP_Stability  Aman 15-17.18.txt")
boro <- fread("HPP_Stability Boro 2015-16 to 17-18.txt")

####### select dataset #########
dat <- aman; seldat <- "aman"
# dat <- boro; seldat <- "boro"
################################

# basic formatting
names(dat)[5:6] <- c("Yield", "GD") # rename columns

makefactor <- c("Year","Station","Variety","Rep","Category") # names of columns that should be factor
dat[, (makefactor) := lapply(.SD, as.factor), .SDcols=makefactor] # define all those columns as.factor

setcolorder(dat, c("Year", "Station", "Variety", "Category", "Rep", "Yield", "GD")) # column order

### check factor levels
# single factor
as.data.table(table(dat$Year))      # Obs per year
as.data.table(table(dat$Station))   # Obs per location
as.data.table(table(dat$Variety))   # problem with names for aman. fixed below.
  library(stringr)
  dat$Variety <- str_replace_all(dat$Variety, "Dhan", "dhan") %>% as.factor()
as.data.table(table(dat$Variety))   # Obs per genotype
as.data.table(table(dat$Category))  # Obs per category

# factor combinations
table(dat$Variety, dat$Year)        # Obs per GxY
table(dat$Variety, dat$Station)     # Obs per GxL
table(dat$Variety, dat$Category)    # Obs per GxCategory
table(dat$Station, dat$Year)        # Obs per Environment
as.data.table(table(dat$Variety, dat$Year, dat$Station)) # Reps per Environment

### check numeric values
# Yield
hist(dat$Yield)
boxplot(dat$Yield ~ dat$Year)

# GD
hist(dat$GD)
boxplot(dat$GD~ dat$Year) # outlier detected for aman
  dat[GD > 500]             # show outlier
  dat[GD > 500, "GD"] <- NA # remove outlier
hist(dat$GD)
boxplot(dat$GD~ dat$Year)

### export
fwrite(dat,    paste0(seldat," plotdat cleaned.txt")) # txt file
save(dat, file=paste0(seldat," plotdat cleaned.rda")) # rda file

