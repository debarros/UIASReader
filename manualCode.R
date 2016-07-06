#manual code execution

# Setup
source("functions.R")              #load functions
firstDay = as.Date("2015-07-01")   #enter 20##-07-01 for the first day of the current school year
form = "Old"                       #set whether you are using 'Old' or 'New' format UIAS reports


# Setting folders on a Windows machine
# Only run this if you are using Windows
currentFolder = choose.dir(getwd())  #select the folder where you put the UIAS files
#priorFolder = choose.dir(getwd())    #select the folder where you put the prior (this hasn't been written yet)


# Setting folders on a Macintosh machine
# Only run this if you are using a Mac
currentFolder <- dlgDir()$res #select the folder where you put the UIAS files
#priorFolder <- dlgDir()$res #select the folder where you put the prior (this hasn't been written yet)


# load the data
DataContainer1 = readData(currentFolder, priorFolder = NA, form)


# process the data
DataContainer2 = processData(DataContainer1, firstDay)


# Create Output
createwb(DataContainer2, "Category")


