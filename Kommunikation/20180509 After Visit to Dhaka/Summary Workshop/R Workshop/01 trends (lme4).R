##########################
### Step 1: Import dataset
##########################

rm(list=ls()) # remove everything from environment (i.e. clean up)

setwd("...change.this.to.your.folder...") 
  # set the working directory to the folder you are working in.
  # Here is the easy way to get this code the first time you do it in a new project:
  # 1. In the "Files" Tab of RStudio, click on the three dots on the top right corner
  # 2. A window pops up. Select your folder
  # 3. You can now see the content of your selected folder in the "Files" Tab
  # 4. Click on the "More" button at the top of the "Files" Tab
  # 5. Click on "Set as Working directoy"
  # 6. You can now see the correct "setwd(.....)" command in the Console window of RStudio
  # 7. Copy this command into your R script

raw <- read.delim("20171212 Corrected Final GG Boro 09.08.2017.txt")
  # read.delim imports tab-delimited txt files.
  # Note that you can get information about any function in R, by running the function
  # with a question mark before it. You will then see additional info in the "Help"
  # Tab of RStudio. For example, try running: ?read.delim


##########################
### Step 2: Format dataset
##########################
  # All columns of your dataset (or at least the ones you are going to use) need to be
  # formatted correctly. Usually this means, that they need to be either numeric(metric) 
  # or a factor (categorical). If you are not sure what the difference is, you can 
  # find multiple explanations on the internet. E.g.: 
  # http://www.dummies.com/education/math/statistics/types-of-statistical-data-numerical-categorical-and-ordinal/
  # You can check what format your columns are by going to the "Environment"
  # tab in RStudio and clicking on the small blue button next to your dataset. It shows
  # you all the columns in your dataset and which format they are. Common formats are
  # "int", "chr", "num" and "Factor." I suggest you define all your columns as either
  # numeric ("num") or a factor.
  # In addition, I like to give my columns short names, as it makes writing (=coding) 
  # R scripts easier.

# Harder to format columns:
raw$t.k   <- as.numeric(substr(raw$Year, 1, 4))
  # Extract the first four symbols of the "Year" column and define it as the numeric
  # column "t.k". This means e.g. for the first data row that from the cell entry
  # "2001-2002", we extract "2001" and make it numeric i.e. the number 2001.
raw$Env   <- as.factor(paste(raw$Year, raw$Location, sep="-"))
  # Combine the cell entries of the columns "Y" and "L" with a "-" in between them. This
  # means e.g. for the first data row we get "2001" and "Barisal" and combine it into
  # "2001-Barisal"

# Easy to format columns:
  # Copy/overwrite already existing columns, but define them in the  
  # correct format (num or Factor) and give them shorter names
raw$r.i   <- as.numeric(raw$Year.of.release)
raw$G     <- as.factor(raw$Variety)
raw$L     <- as.factor(raw$Location)
raw$Y     <- as.factor(raw$t.k)
raw$Rep   <- as.factor(raw$Rep)
raw$Group <- as.factor(raw$Group)

data <- raw[ ,c("Y", "t.k", "r.i", "L", "Env", "G", "Rep", "Group", "Yield")]
  # Reduce the dataset to only those 9 columns we name above.

write.table(data, "formatted data.txt", row.names=F, sep="\t")
  # Quick export of formatted dataset in our working directory folder.


###################################################
### Step 3: Mean yield per genotype and environment
###################################################
library(data.table) 
  # activates package "data.table", given it is already installed.
  # compared to the default "data.frame" format, the "data.table" format of 
  # any dataset in R is advantageous in many situations.

data <- as.data.table(data) # reformat our data to "data.table" format

data[ , mean.yield:=mean(Yield), by=c("Env", "G")] # creates column "mean.yield"
  # This way of getting the mean is part of the data.table package/format.
  # It creates gets the mean of the numbers in the "Yield" columns for each "Env"-"G"
  # combination and saves it into a new column called "mean.yield".

data <- data[ , -c("Rep", "Yield")] # Drop "Rep" and "Yield" column. 
  # They are now unnessary, since we focus on the mean yields
meandata <- unique(data)            # Drop duplicate rows

write.table(meandata, "GxEmeans.txt", row.names=F, sep="\t")
  # Export meandata as text file

saveRDS(meandata, "GxEmeans.rds")
  # Additionally export meandata as an rds file. These files contain all the information
  # of an R object - for example whether a column is numeric of a factor. Such
  # information is lost, when exporting and reimporting a .txt file.


##################################################
### Step 4: Estimate genetic trends in mixed model
##################################################
# Two packages required: lme4, lmerTest
library(lme4)
library(lmerTest)

# Fitting the model
  # This model is based on the basic model for long-term MET data 
  # (slide 7 in "Piepho - trend.pdf") and extended via incorporating regression 
  # terms for a genetic and a non-genetic trend, respectively
  # (slide 14 in "Piepho - trend.pdf")
mod <- lmer(formula = mean.yield ~ r.i + t.k +
                                   (1|G) + (1|L) + (1|Y) + 
                                   (1|Y:L) + (1|Y:G) + (1|L:G),
            data    = meandata)
  # Fixed effects are written plain, while random effects are in the (1|...) notation
  # as seen above. 
  # Note also that r.i and t.k are numeric variables in our dataset and thus a single 
  # slope will be estimated for them, respectively,  while G, Y, L (and thus all 
  # their interactions, too,) are factors and thus receive an effect estimate 
  # for each level. 
  # When comparing our model to Prof. Piepho's slide 7 in "Piepho - trend.pdf", 
  # it seems that we are missing two effects: "µ" and "(1|Y:L:G)"
  # This is not true, however, since lmer (and most statistical softwares) will
  # by default put a general intercept (i.e. µ) and an error effect in any model.
  # In this case, the three-way-interaction Y:L:G is in fact the error effect.

###################################
### Step 5: Viewing results for mod
###################################
summary(mod) # summary of model results

coef(summary(mod)) # fixed effects solutions
  # the estimate (i.e. slope) for r.i is the genetic trend
  # the estimate (i.e. slope) for t.k is the non-genetic trend
  # you can also find their respective standard errors and p-values from t-tests
  # Detail: the latter are obtained via t-tests which use Satterthwaite's DF

anova(mod) # anova for fixed effects
  # here you can find p-values from F-tests for r.i and t.k
  # Detail: this is a Type III Analysis of Variance Table with Satterthwaite's DF

#############################################################
### Step 4b: Estimate genetic trends per group in mixed model
#############################################################

mod.group <- lmer(formula = mean.yield ~ 0 + Group + Group:r.i + t.k +
                            (1|G) + (1|L) + (1|Y) + 
                            (1|Y:L) + (1|Y:G) + (1|L:G),
                  data    = meandata)

  # the difference between this model (mod.group) and the first one (mod) lies in 
  # the fixed effects we fit. "mod" has 3 fixed effects: intercept (µ), slope for
  # r.i and slope for t.k.
  # "mod.group" has 7 fixed effects: one intercept per group (GroupLong, GroupShort,
  # GroupStress), a slope for t.k and one slope per group for r.i (GroupLong:r.i, 
  # GroupShort:r.i, GroupStress:r.i).

##########################################
### Step 5b: Viewing results for mod.group 
##########################################
summary(mod.group) 
coef(summary(mod.group)) 
anova(mod.group) 
  # analog to Step 5


