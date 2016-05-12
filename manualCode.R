#manual code execution

#Setup: load functions, set LEA ####
library(openxlsx)
myLEA = "Green Tech High Charter"


# read in the data ####
files = data.frame(
  code = c("FD", "FT", "SE", "DP", "FT.AY", "Dem"),
  name = c("False Dropouts.xlsx", "False Transfers.xlsx", "Simultaneous Enrollments.xlsx", "Disappearing Students.xlsx", "False Transfers Across Years.xlsx", "Demographics.csv"),
  exists = NA, 
  startRow = c(10, 10, 9, 10, 9, 0),
  stringsAsFactors = F)
files$exists = file.exists(files$name)


for(i in 1:(nrow(files))){
  if(files$exists[i]){
    if(substr(files$name[i], nchar(files$name[i]), nchar(files$name[i])) == "x"){
      assign(files$code[i], read.xlsx(xlsxFile = files$name[i], sheet = "violations", startRow = files$startRow[i]))
    } else {
      assign(files$code[i], read.csv(file = files$name[i], stringsAsFactors = F, header = F))
    }
  } else {
    assign(files$code[i], NA)
  }
}



# Simultaneous Enrollments ####
if(is.data.frame(SE)){                 #as long as SE (the simultaneous enrollment input) exists...
  SE.local = SE[SE$LEA == myLEA,]   #split the records into those that are local and those that reference a different LEA
  SE.other = SE[SE$LEA != myLEA,]
  SimEnr = data.frame(NYSSIS = unique(SE$NYSSIS), stringsAsFactors = FALSE)   #Create a dataset with 1 row for each unique NYSSIS ID
  SimEnr$LocalEntry = as.Date(SE.local$Entry.Date[match(x = SimEnr$NYSSIS, table = SE.local$NYSSIS)], format = "%m/%d/%Y")  #load the local and other entry dates
  SimEnr$OtherEntry = as.Date(SE.other$Entry.Date[match(x = SimEnr$NYSSIS, table = SE.other$NYSSIS)], format = "%m/%d/%Y")
  
  SimEnr$LocalContinuer = SimEnr$LocalEntry == as.Date("2015-07-01")   #indicate if the student was enrolled as of July 1
  SimEnr$OtherContinuer = SimEnr$OtherEntry == as.Date("2015-07-01")
  SimEnr$WhoShouldExit = "Unsure"                                          #Set the default WhoShouldExit to "Unsure"
  SimEnr$WhoShouldExit[SimEnr$LocalEntry < SimEnr$OtherEntry] = "Us?"      #If we entered first, set it as "Us?"
  SimEnr$WhoShouldExit[SimEnr$LocalEntry > SimEnr$OtherEntry] = "Them?"    #If they entered first, set it as "Them?"
  SimEnr$OtherSchool = SE.other$LEA[match(SimEnr$NYSSIS, SE.other$NYSSIS)] #
  SimEnr$lastName = SE.local$Last.Name[match(x = SimEnr$NYSSIS, table = SE.local$NYSSIS)]
  SimEnr$firstName = SE.local$First.Name[match(x = SimEnr$NYSSIS, table = SE.local$NYSSIS)]
  SimEnr$ID = SE.local$Local.ID[match(x = SimEnr$NYSSIS, table = SE.local$NYSSIS)]
  SimEnr = SimEnr[,c("firstName", "lastName","ID", "WhoShouldExit", "OtherSchool", "LocalEntry", "OtherEntry")]
  
  SimEnr = SimEnr[order(SimEnr$WhoShouldExit, SimEnr$OtherSchool),]
}


# False Transfers ####
if(is.data.frame(FT)){
  FalTr = FT[FT$Case == "FT",c("Last.Name", "First.Name","NYSSIS","Local.ID","Entry.Date","Exit.Date")]
  OL = FT[FT$Case == "OL",]
  OL.local = OL[OL$LEA == myLEA,]
  OL.other = OL[OL$LEA != myLEA,]
  OverLap = data.frame(NYSSIS = unique(OL$NYSSIS), stringsAsFactors = FALSE)
  OverLap$FirstName = OL.local$First.Name[OL.local$NYSSIS == OverLap$NYSSIS]
  OverLap$LastName = OL.local$Last.Name[OL.local$NYSSIS == OverLap$NYSSIS]
  OverLap$LocalEntry = as.Date(OL.local$Entry.Date[OL.local$NYSSIS == OverLap$NYSSIS], format = "%m/%d/%Y")
  OverLap$LocalExit = as.Date(OL.local$Exit.Date[OL.local$NYSSIS == OverLap$NYSSIS], format = "%m/%d/%Y")
  OverLap$OtherEntry = as.Date(OL.other$Entry.Date[OL.other$NYSSIS == OverLap$NYSSIS], format = "%m/%d/%Y")
  OverLap$OtherExit = as.Date(OL.other$Exit.Date[OL.other$NYSSIS == OverLap$NYSSIS], format = "%m/%d/%Y")
  OverLap$issue = ""
  for( i in 1:nrow(OverLap)){
    if(OverLap$LocalEntry[i] == OverLap$OtherEntry[i] & OverLap$LocalEntry[i] == as.Date("2015-07-01")){
      OverLap$issue[i] = "We both tried to claim the student at the beginning of the year"
    } else {
      if(OverLap$LocalEntry[i] == OverLap$OtherEntry[i] | OverLap$LocalExit[i] == OverLap$OtherExit[i]){
        if(OverLap$LocalExit[i] == OverLap$OtherExit[i]){
          if (OverLap$LocalEntry[i] == OverLap$OtherEntry[i]){
            OverLap$issue[i] = "exact same enrollment"
          } else {OverLap$issue[i] = "Same exit date"}
        } else {OverLap$issue[i] ="Same entry date"}
      } else {
        if(OverLap$LocalEntry[i] > OverLap$OtherEntry[i]){
          if(OverLap$LocalExit[i] > OverLap$OtherExit[i]){
            OverLap$issue[i] = "We entered too soon or they exited too late"
          } else {OverLap$issue[i] = "Our entire enrollment is during theirs"}
        } else { #local entry before other entry
          if(OverLap$LocalExit[i] > OverLap$OtherExit[i]){
            OverLap$issue[i] = "Their entire enrollment is during ours"
          } else {
            OverLap$issue[i] = "We exited too late or they entered too soon"
          }
        }
      }
    }
  }
  OverLap$OtherLEA = OL.other$LEA[match(OverLap$NYSSIS, OL.other$NYSSIS)]
  OverLap = OverLap[order(OverLap$OtherLEA, OverLap$issue),]
}


# False Transfers Across Years ####
if(is.data.frame(FT.AY)){
  FalTrAY = FT.AY[FT.AY$Case == "FT",c("Last.Name", "First.Name","NYSSIS","Local.ID","Entry.Date","Exit.Date")]
  if(!is.data.frame(FalTr)){
    FalTr = FalTrAY
  } else {
    FalTr = rbind.data.frame(FalTr, FalTrAY)
  }
}


# Disappearing Students ####
if(is.data.frame(DP)){
  DisapStu = DP[,c("Last.Name","First.Name","NYSSIS","Local.ID","Entry.Date")]
}


# False Dropouts ####
# this section has not been written yet
if(is.data.frame(FD)){}


# Comparison to prior month ####
# this section has not been written yet
# the plan would be to highlight any rows in the current month that match the prior month


# Create Output ####
wb = createWorkbook()
addWorksheet(wb=wb, sheetName = "Simultaneous")
freezePane(wb, "Simultaneous", firstActiveRow = 2, firstActiveCol = 2)
setColWidths(wb, "Simultaneous", cols = 1:10, widths = "auto", ignoreMergedCells = FALSE)
addStyle(wb, "Simultaneous", createStyle(textDecoration = "bold"), rows = 1, cols = 1:10, gridExpand = T, stack = T)

addWorksheet(wb=wb, sheetName = "Overlapping")
freezePane(wb, "Overlapping", firstActiveRow = 2, firstActiveCol = 2)
setColWidths(wb, "Overlapping", cols = 1:10, widths = "auto", ignoreMergedCells = FALSE)
addStyle(wb, "Overlapping", createStyle(textDecoration = "bold"), rows = 1, cols = 1:10, gridExpand = T, stack = T)

addWorksheet(wb=wb, sheetName = "False Transfers")
freezePane(wb, "False Transfers", firstActiveRow = 2, firstActiveCol = 2)
setColWidths(wb, "False Transfers", cols = 1:10, widths = "auto", ignoreMergedCells = FALSE)
addStyle(wb, "False Transfers", createStyle(textDecoration = "bold"), rows = 1, cols = 1:10, gridExpand = T, stack = T)
if(is.data.frame(FalTrAY)){
  if(!is.data.frame(FalTr)){
    addStyle(wb, "False Transfers", 
             createStyle(fgFill = "cadetblue1"), 
             rows = 2:(nrow(FalTrAY)+1), cols = 1:6, 
             gridExpand = T, stack = T)
  } else {
    addStyle(wb, "False Transfers", 
             createStyle(fgFill = "cadetblue1"), 
             rows = (nrow(FalTr) - nrow(FalTrAY) + 2):(nrow(FalTr)+1), cols = 1:6, 
             gridExpand = T, stack = T)  
  }
}

addWorksheet(wb=wb, sheetName = "Disappearing")
freezePane(wb, "Disappearing", firstActiveRow = 2, firstActiveCol = 2)
setColWidths(wb, "Disappearing", cols = 1:10, widths = "auto", ignoreMergedCells = FALSE)
addStyle(wb, "Disappearing", createStyle(textDecoration = "bold"), rows = 1, cols = 1:5, gridExpand = T, stack = T)
addStyle(wb, "Disappearing", createStyle(fgFill = "cadetblue1"), rows = 2:(nrow(DP)+1), cols = 1:10, gridExpand = T, stack = T)

if(is.data.frame(SE)){writeData(wb=wb, sheet = "Simultaneous", x = SimEnr)}
if(is.data.frame(FT)){if(is.data.frame(OverLap)){writeData(wb=wb, sheet = "Overlapping", x = OverLap)}}
if(is.data.frame(FT) | is.data.frame(FT.AY)){writeData(wb=wb, sheet = "False Transfers", x = FalTr)}
if(is.data.frame(DP)){writeData(wb=wb, sheet = "Disappearing", x = DisapStu)}

saveWorkbook(wb = wb, file = "May UIAS Report.xlsx", overwrite = T)


