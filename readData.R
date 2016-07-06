# readData.R

# This function sets up the list of file names and calls the appopriate reader function
# The reader functions are below
#
# Inputs:
#   form - either "Old" or "New", indicating which type of UIAS report to read
#   files - a data.frame holding info about the files
#   currentFolder - the path to the folder containing the files
#   priorFolder - the path to the folder containing the last reports
#
# Output:
#   DC - a list of all of the R objects read from the files
readData = function(currentFolder, priorFolder, form = "New"){
  if(form %in% c("Old", "New")){
    if(form == "Old"){
      # set up a data.frame that describes the types of files that might be found
      files = data.frame(
        code = c("FD", "FT", "SE", "DP", "FT.AY", "Dem", "Enr"), #variables names
        name = c("False Dropouts.xlsx", "False Transfers.xlsx", "Simultaneous Enrollments.xlsx", "Disappearing Students.xlsx", 
                 "False Transfers Across Years.xlsx", "Demographics.csv", "Enrollment.csv"), #these are the filenames.  They are case sensitive.
        exists = NA, 
        startRow = c(10, 10, 9, 10, 9, 0, 0), #for xlsx, this is the row with the column headings.  For csv, it's nothing.
        myLEA = NA,  #this will be filled in by reading the LEA from the UIAS files 
        stringsAsFactors = F)
      files$exists = file.exists(paste0(currentFolder,"/",files$name))
      DC = readOldFormat(files, currentFolder, priorFolder)
      DC$form = "Old"
    } else {
      DC = list(form = "New")
      warning("That functionality has not been written yet.")
      #DC = readNewFormat(files, currentFolder, priorFolder)
      #DC$form = "New"
    }
    DC$currentFolder = currentFolder
    DC$priorFolder = priorFolder
    return(DC)
  } else {
    warning("form must be either 'Old' or 'New'.")
  }
}




# This function reads in old format files from the selected folder.  It's called from readData()
#
# Inputs:
#   files - a data.frame holding info about the files
#   currentFolder - the path to the folder containing the files
#   priorFolder - the path to the folder containing the last reports
#
# Output:
#   DC - a list of all of the R objects read from the files
readOldFormat = function (files, currentFolder, priorFolder){
  
  # load the files
  filePaths = paste0(currentFolder, "/",files$name)
  for(i in 1:(nrow(files))){
    if(files$exists[i]){ #if the file exists,
      if(substr(filePaths[i], nchar(filePaths[i]), nchar(filePaths[i])) == "x"){ #if it's an xlsx file
        assign(files$code[i], read.xlsx(xlsxFile = filePaths[i], sheet = "violations", startRow = files$startRow[i])) #read it in
        files$myLEA[i] = read.xlsx(xlsxFile = filePaths[i], sheet = "violations", rows = as.integer(c(3,4)), cols = as.integer(2))[1,1]
      } else { #otherwise, it's a csv
        assign(files$code[i], read.csv(file = filePaths[i], header = F, colClasses = "character")) #read it in
      }
    } else { #if the file doesn't exist,
      assign(files$code[i], NA) #create the variable anyway and set it to NA
    }
  }


    
  # Report on the how reading the files went
  if(sum(!files$exists) == 0){
    print("All files were found.")
  } else {
    print(paste0("The following files were not found in ",currentFolder))
    print(files$name[!files$exists])
    print("If you thought you had them, make sure they are in the right folder and the filenames are spelled correctly.")
  }
  
  if(!all(files$myLEA[!is.na(files$myLEA)] == files$myLEA[!is.na(files$myLEA)][1])){
    warning("Not all of the files refer to the same LEA.  The following LEA's were found:")
    print(unique(files$myLEA[!is.na(files$myLEA)]))
  } else {
    myLEA = files$myLEA[!is.na(files$myLEA)][1]
  }
  
  # return the data
  DC = mget(files$code)
  DC$myLEA = myLEA
  return(DC)
}
