#newFileFormats.R

#export from L2RPT using "Excel 2007 Data"

library(openxlsx)
myLEA = "Green Tech High Charter"
myDistrict = "GREEN TECH HIGH CHARTER SCHO"

new.170.01 = read.xlsx("170.02 - Enrolled in same district.xlsx")
new.DS.01 = read.xlsx("DS.01 - Enrolled in different district.xlsx")
new.SE.02 = read.xlsx("SE.02 - Concurrent (Other) Enrollment.xlsx")

new.170.01$ENTRY_DATE = as.Date(new.170.01$ENTRY_DATE, origin = "1899-12-30")
new.170.01$EXIT_DATE = as.Date(new.170.01$EXIT_DATE, origin = "1899-12-30")

new.DS.01$ENTRY_DATE = as.Date(new.DS.01$ENTRY_DATE, origin = "1899-12-30")
new.DS.01$EXIT_DATE = as.Date(new.DS.01$EXIT_DATE, origin = "1899-12-30")


new.SE.02$ENTRY_DATE = as.Date(new.SE.02$ENTRY_DATE, origin = "1899-12-30")
new.SE.02$EXIT_DATE = as.Date(new.SE.02$EXIT_DATE, origin = "1899-12-30")

str(new.SE.02)
# Simultaneous Enrollments ####
if(is.data.frame(new.SE.02)){                 #as long as SE (the simultaneous enrollment input) exists...
  SE.local = new.SE.02[new.SE.02$ENRL_DISTRICT_NAME == myDistrict,]   #split the records into those that are local and those that reference a different LEA
  SE.other = new.SE.02[new.SE.02$ENRL_DISTRICT_NAME != myDistrict,]
  SimEnr = data.frame(NYSSIS = unique(new.SE.02$STUDENT_ID_ALT), stringsAsFactors = FALSE)   #Create a dataset with 1 row for each unique NYSSIS ID
  SimEnr$LocalEntry = as.Date(SE.local$ENTRY_DATE[match(x = SimEnr$NYSSIS, table = SE.local$STUDENT_ID_ALT)], format = "%m/%d/%Y")  #load the local and other entry dates
  SimEnr$OtherEntry = as.Date(SE.other$ENTRY_DATE[match(x = SimEnr$NYSSIS, table = SE.other$STUDENT_ID_ALT)], format = "%m/%d/%Y")
  SimEnr$LocalContinuer = SimEnr$LocalEntry == as.Date("2015-07-01")   #indicate if the student was enrolled as of July 1
  SimEnr$OtherContinuer = SimEnr$OtherEntry == as.Date("2015-07-01")
  SimEnr$WhoShouldExit = "Unsure"                                          #Set the default WhoShouldExit to "Unsure"
  SimEnr$WhoShouldExit[SimEnr$LocalEntry < SimEnr$OtherEntry] = "Us?"      #If we entered first, set it as "Us?"
  SimEnr$WhoShouldExit[SimEnr$LocalEntry > SimEnr$OtherEntry] = "Them?"    #If they entered first, set it as "Them?"
  SimEnr$OtherLEA = SE.other$ENRL_DISTRICT_NAME[match(SimEnr$NYSSIS, SE.other$STUDENT_ID_ALT)] #
  SimEnr$OtherSchool = SE.other$LOCATION_NAME[match(SimEnr$NYSSIS, SE.other$STUDENT_ID_ALT)] #
  SimEnr$Name = SE.local$STUDENT_NAME[match(x = SimEnr$NYSSIS, table = SE.local$STUDENT_ID_ALT)]
  SimEnr$ID = SE.local$STUDENT_ID[match(x = SimEnr$NYSSIS, table = SE.local$STUDENT_ID_ALT)]
  SimEnr = SimEnr[,c("Name","ID","NYSSIS", "WhoShouldExit", "OtherSchool","OtherLEA", "LocalEntry", "OtherEntry")]
  SimEnr = SimEnr[order(SimEnr$WhoShouldExit, SimEnr$OtherSchool),]
}






str(new.DS.01)
# District somethings ####
# Don't know what this is.  It seems to show when a student moved from District A in one year to district B in another year.







str(new.170.01)
# False Transfers with 170 exit code ####
#Don't know what this is.  It seems to show when a student has two enrollments for the same district in the same year, even if they don't overlap.

