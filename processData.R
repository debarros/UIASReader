# processData.R


processData = function(DC, myLEA, firstDay){
  if(DC$form %in% c("Old", "New")){
    if(DC$form == "Old"){
      DC = processOldFormat(DC, myLEA, firstDay)
    } else {
      DC = NA
      warning("That functionality has not been written yet.")
    }
    return(DC) 
  } else {
    warning("form must be either 'Old' or 'New'.")
  }
}



processOldFormat = function(DC, myLEA, firstDay){
  
  # Simultaneous Enrollments ####
  SE = DC$SE
  if(is.data.frame(SE)){                 #as long as SE (the simultaneous enrollment input) exists...
    SE.local = SE[SE$LEA == myLEA,]   #split the records into those that are local and those that reference a different LEA
    SE.other = SE[SE$LEA != myLEA,]
    SimEnr = data.frame(NYSSIS = unique(SE$NYSSIS), stringsAsFactors = FALSE)   #Create a dataset with 1 row for each unique NYSSIS ID
    SimEnr$LocalEntry = as.Date(SE.local$Entry.Date[match(x = SimEnr$NYSSIS, table = SE.local$NYSSIS)], format = "%m/%d/%Y")  #load the local and other entry dates
    SimEnr$OtherEntry = as.Date(SE.other$Entry.Date[match(x = SimEnr$NYSSIS, table = SE.other$NYSSIS)], format = "%m/%d/%Y")
    
    SimEnr$LocalContinuer = SimEnr$LocalEntry == firstDay   #indicate if the student was enrolled as of July 1
    SimEnr$OtherContinuer = SimEnr$OtherEntry == firstDay
    SimEnr$WhoShouldExit = "Unsure"                                          #Set the default WhoShouldExit to "Unsure"
    SimEnr$WhoShouldExit[SimEnr$LocalEntry < SimEnr$OtherEntry] = "Us?"      #If we entered first, set it as "Us?"
    SimEnr$WhoShouldExit[SimEnr$LocalEntry > SimEnr$OtherEntry] = "Them?"    #If they entered first, set it as "Them?"
    SimEnr$OtherSchool = SE.other$LEA[match(SimEnr$NYSSIS, SE.other$NYSSIS)] #
    SimEnr$lastName = SE.local$Last.Name[match(x = SimEnr$NYSSIS, table = SE.local$NYSSIS)]
    SimEnr$firstName = SE.local$First.Name[match(x = SimEnr$NYSSIS, table = SE.local$NYSSIS)]
    SimEnr$ID = SE.local$Local.ID[match(x = SimEnr$NYSSIS, table = SE.local$NYSSIS)]
    SimEnr = SimEnr[,c("firstName", "lastName","ID", "NYSSIS", "WhoShouldExit", "OtherSchool", "LocalEntry", "OtherEntry")]
    
    SimEnr = SimEnr[order(SimEnr$WhoShouldExit, SimEnr$OtherSchool),]
  } else {
    SimEnr = NA
  }
  
  
  # False Transfers ####
  FT = DC$FT
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
  } else {
    FalTr = NA
    OverLap = NA
  }
  
  
  # False Transfers Across Years ####
  FT.AY = DC$FT.AY
  if(is.data.frame(FT.AY)){
    FalTrAY = FT.AY[FT.AY$Case == "FT",c("Last.Name", "First.Name","NYSSIS","Local.ID","Entry.Date","Exit.Date")]
    if(!is.data.frame(FalTr)){
      FalTr = FalTrAY
    } else {
      FalTr = rbind.data.frame(FalTr, FalTrAY)
    }
  } else {
    FalTrAY = NA
  }
  
  
  # Disappearing Students ####
  DP = DC$DP
  if(is.data.frame(DP)){
    DisapStu = DP[,c("Last.Name","First.Name","NYSSIS","Local.ID","Entry.Date")]
  } else {
    DisapStu = NA
  }
  
  
  # False Dropouts ####
  # this section has not been written yet
  FD = DC$FD
  if(is.data.frame(FD)){}
  
  
  # Comparison to prior month ####
  # this section has not been written yet
  # the plan would be to mark/highlight any rows in the current month that match the prior month
  
  DC$SimEnr = SimEnr
  DC$FalTr = FalTr
  DC$OverLap = OverLap
  DC$FalTrAY = FalTrAY
  DC$DisapStu = DisapStu
  
  return(DC)
}