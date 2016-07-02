#manual code execution

# Setup
source("functions.R")              #load functions
myLEA = "Green Tech High Charter"  #set LEA
firstDay = as.Date("2015-07-01")   #enter 20##-07-01 for the first day of the current school year
currentFolder = choose.dir(getwd())  #select the folder where you put the UIAS files
priorFolder = choose.dir(getwd())    #select the folder where you put the prior 
form = "Old"                         #set whether you are using 'Old' or 'New' format UIAS reports


# load the data
DataContainer1 = readData(currentFolder, priorFolder, form)

# process the data
DataContainer2 = processData(DataContainer1, myLEA, firstDay)

# Create Output
createwb(DataContainer2, "Category")
#wb.Old.LEA = createwb(form = "Old", style = "LEA", DataContainer2)









