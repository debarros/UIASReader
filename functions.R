# functions.R

# Load libraries ####
packs = installed.packages()

if("openxlsx" %in% packs){
  library(openxlsx)
} else {
  warning("You must install openxlsx first.")
}

if("svDialogs" %in% packs){
  library(svDialogs)
} else {
  warning("The package svDialogs is not installed. On Windows, that's fine.  On Macinstosh, you must install it first.")
}


source("readData.R")
source("processData.R")
source("createwb.R")



