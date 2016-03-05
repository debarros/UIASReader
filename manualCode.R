#manual code exectuion

#get functions and set lea ####
library(openxlsx)
myLEA = "Green Tech High Charter"


# read in the data ####
FD = read.xlsx(xlsxFile = "False Dropouts.xlsx", sheet = "violations", startRow = 10)
FT = read.xlsx(xlsxFile = "False Transfers.xlsx", sheet = "violations", startRow = 10)
SE = read.xlsx(xlsxFile = "Simultaneous Enrollments.xlsx", sheet = "violations", startRow = 9)
DP = read.xlsx(xlsxFile = "Disappearing Students.xlsx", sheet = "violations", startRow = 9)


# Simultaneous Enrollments ####
SE.local = SE[SE$LEA == myLEA,]
SE.other = SE[SE$LEA != myLEA,]
SimEnr = data.frame(NYSSIS = unique(SE$NYSSIS), stringsAsFactors = FALSE)
SimEnr$LocalEntry = as.Date(SE.local$Entry.Date[match(x = SimEnr$NYSSIS, table = SE.local$NYSSIS)], format = "%m/%d/%Y")
SimEnr$OtherEntry = as.Date(SE.other$Entry.Date[match(x = SimEnr$NYSSIS, table = SE.other$NYSSIS)], format = "%m/%d/%Y")

SimEnr$LocalContinuer = SimEnr$LocalEntry == as.Date("2015-07-01")
SimEnr$OtherContinuer = SimEnr$OtherEntry == as.Date("2015-07-01")
SimEnr$WhoShouldExit = "Unsure"
SimEnr$WhoShouldExit[SimEnr$LocalEntry < SimEnr$OtherEntry] = "Us?"
SimEnr$WhoShouldExit[SimEnr$LocalEntry > SimEnr$OtherEntry] = "Them?"
SimEnr$OtherSchool = SE.other$LEA[match(SimEnr$NYSSIS, SE.other$NYSSIS)]
SimEnr$lastName = SE.local$Last.Name[match(x = SimEnr$NYSSIS, table = SE.local$NYSSIS)]
SimEnr$firstName = SE.local$First.Name[match(x = SimEnr$NYSSIS, table = SE.local$NYSSIS)]
SimEnr$ID = SE.local$Local.ID[match(x = SimEnr$NYSSIS, table = SE.local$NYSSIS)]
SimEnr = SimEnr[,c("firstName", "lastName","ID", "WhoShouldExit", "OtherSchool", "LocalEntry", "OtherEntry")]

SimEnr = SimEnr[order(SimEnr$WhoShouldExit, SimEnr$OtherSchool),]


# False Transfers ####
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


# Disappearing Students ####
# this section has not been written yet


# False Dropouts ####
# this section has not been written yet


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

writeData(wb=wb, sheet = "Simultaneous", x = SimEnr)
writeData(wb=wb, sheet = "Overlapping", x = OverLap)
writeData(wb=wb, sheet = "False Transfers", x = FalTr)

saveWorkbook(wb = wb, file = "March UIAS Report.xlsx", overwrite = T)


