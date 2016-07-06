# createwb.R

# This function sets up the list of file names and calls the appopriate reader function
# The reader functions are below
#
# Inputs:
#   DC - a list of R object containing the necessary information
#   style - either "Category" or "LEA", indicating how to organize the output
#
# Output:
#   none
createwb = function(DC, style = "Category"){
  if(style %in% c("Category", "LEA")){
    if(DC$form == "Old" & style == "Category") { 
      createwb.old.cat(DC)
    } else {
      warning("That functionality hasn't been written yet.")
    }
  } else {
    warning("Style must be either 'Category' or 'LEA'.")
  } 
}



createwb.old.cat = function(DC)  {
  
  wb = createWorkbook() # initialize the workbook
  
  #Pull out the relevant data
  FalTr = DC$FalTr
  FalTrAY = DC$FalTrAY
  FT.AY = DC$FT.AY
  DP = DC$DP
  SE = DC$SE
  FT = DC$FT
  FD = DC$FD
  OverLap = DC$OverLap
  SimEnr = DC$SimEnr
  FalDrop = DC$FalDrop
  DisapStu = DC$DisapStu
  currentFolder = DC$currentFolder
  
  # set up the simultaneous enrollments tab
  addWorksheet(wb=wb, sheetName = "Simultaneous")
  freezePane(wb, "Simultaneous", firstActiveRow = 2, firstActiveCol = 2)
  setColWidths(wb, "Simultaneous", cols = 1:10, widths = "auto", ignoreMergedCells = FALSE)
  addStyle(wb, "Simultaneous", createStyle(textDecoration = "bold"), rows = 1, cols = 1:10, gridExpand = T, stack = T)
  
  # set up the overlapping enrollments tab
  addWorksheet(wb=wb, sheetName = "Overlapping")
  freezePane(wb, "Overlapping", firstActiveRow = 2, firstActiveCol = 2)
  setColWidths(wb, "Overlapping", cols = 1:10, widths = "auto", ignoreMergedCells = FALSE)
  addStyle(wb, "Overlapping", createStyle(textDecoration = "bold"), rows = 1, cols = 1:10, gridExpand = T, stack = T)
  
  # set up the False Transfer tab
  addWorksheet(wb=wb, sheetName = "False Transfers")
  freezePane(wb, "False Transfers", firstActiveRow = 2, firstActiveCol = 2)
  setColWidths(wb, "False Transfers", cols = 1:10, widths = "auto", ignoreMergedCells = FALSE)
  addStyle(wb, "False Transfers", createStyle(textDecoration = "bold"), rows = 1, cols = 1:10, gridExpand = T, stack = T)
  if(is.data.frame(FalTrAY)){
    if(!is.data.frame(FalTr)){            #If there are false transfers across years, but not with the current year, highlight blue
      addStyle(wb, "False Transfers", 
               createStyle(fgFill = "cadetblue1"), 
               rows = 2:(nrow(FalTrAY)+1), cols = 1:6, 
               gridExpand = T, stack = T)
    } else {
      addStyle(wb, "False Transfers",     #If there are both kinds of false transfers, highlight blue but not the first few rows
               createStyle(fgFill = "cadetblue1"), 
               rows = (nrow(FalTr) - nrow(FalTrAY) + 2):(nrow(FalTr)+1), cols = 1:6, 
               gridExpand = T, stack = T)  
    }
  }
  
  # Set up the Disappearing Students tab
  addWorksheet(wb=wb, sheetName = "Disappearing")
  freezePane(wb, "Disappearing", firstActiveRow = 2, firstActiveCol = 2)
  setColWidths(wb, "Disappearing", cols = 1:10, widths = "auto", ignoreMergedCells = FALSE)
  addStyle(wb, "Disappearing", createStyle(textDecoration = "bold"), rows = 1, cols = 1:5, gridExpand = T, stack = T)
  if(is.data.frame(DP)){
    addStyle(wb, "Disappearing", createStyle(fgFill = "cadetblue1"), rows = 2:(nrow(DP)+1), cols = 1:10, gridExpand = T, stack = T)
  }
  
  # set up the false dropouts tab
  addWorksheet(wb=wb, sheetName = "False Dropouts")
  freezePane(wb, "False Dropouts", firstActiveRow = 2, firstActiveCol = 2)
  setColWidths(wb, "False Dropouts", cols = 1:10, widths = "auto", ignoreMergedCells = FALSE)
  addStyle(wb, "False Dropouts", createStyle(textDecoration = "bold"), rows = 1, cols = 1:10, gridExpand = T, stack = T)
  
  
  
  # Add data to the tabs
  if(is.data.frame(SE)){writeData(wb=wb, sheet = "Simultaneous", x = SimEnr)}
  if(is.data.frame(FT)){if(is.data.frame(OverLap)){writeData(wb=wb, sheet = "Overlapping", x = OverLap)}}
  if(is.data.frame(FT) | is.data.frame(FT.AY)){writeData(wb=wb, sheet = "False Transfers", x = FalTr)}
  if(is.data.frame(DP)){writeData(wb=wb, sheet = "Disappearing", x = DisapStu)}
  if(is.data.frame(FD)){writeData(wb=wb, sheet = "False Dropouts", x = FalDrop)}
  
  # Export the completed report
  saveWorkbook(wb = wb, file =  paste0(currentFolder, "/UIAS Report by category old format.xlsx"), overwrite = T)
}



