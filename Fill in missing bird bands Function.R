#Function to fill in any bird IDs that are known in the banding data but aren't entered into the nestdata

#Requires that the banding data has already been fixed to include a year column
#and a non julian date column 

#These actions can be found in "Filling in missing
#bird bands in nest data" R code file

fillinbandID<-function(nestdata, banddata){
  library(lubridate) #this library's yday() takes a date and gives you the julian date!
  
  
  #Make a hash of the nest locations where there were nests that year
  
  hash_boxes<-new.env(hash=TRUE, parent = emptyenv(), size=length(banddata$Place))
  for (i in 1:length(banddata$Place)){
    site<- banddata$Place[i]
    if(exists(site, hash_boxes)){
      assign(site, append(x=get(site, hash_boxes), value=i), hash_boxes)
    } else {
      assign(site, i, hash_boxes)
    }
  }
  #now we just have to go and see if that hash ccan help us find something useful! 
  #Female
  nestdata$F.Day.measured<-as.Date(NA )
  for (j in 1:length(nestdata$FemaleID)){
    if (is.na(nestdata$FemaleID[j])){
      NestSite <- nestdata$siteID[j]
      if (exists(NestSite, hash_boxes)){
        for (siteIdx in get(NestSite, hash_boxes)){
          if (banddata$Year[siteIdx]==nestdata$Year[j]){
            if(!is.na(banddata$Sex[siteIdx])){
            if (banddata$Sex[siteIdx]=="F"){
              if (is.na(nestdata$FemaleID[j])){
                if(is.na(nestdata$F.Day.measured[j])){
                  next
                }else{
                nestdata$FemaleID[j] <- banddata$Band.Number[siteIdx]
                nestdata$F.Day.measured[j]<-banddata$NJDate[siteIdx]
                message("Found a female band ID (", nestdata$FemaleID[j], ") for nest box", nestdata$siteID[j])
                }
                } else {
                if ( !is.na(nestdata$Fledge.Fail.date[j])){
                  if (banddata$NJDate[siteIdx] < nestdata$Fledge.Fail.date[j] &
                      abs(yday(banddata$NJDate[siteIdx]) - nestdata$First.Egg.Date[j]) <
                      abs(yday(nestdata$F.Day.measured[j]) - nestdata$First.Egg.Date[j])){
                    nestdata$FemaleID[j] <- banddata$Band.Number[siteIdx]
                    nestdata$F.Day.measured[j] <- banddata$NJDate[siteIdx]
                    message("Found a female band ID (", nestdata$FemaleID[j], ")
                            for nest box", nestdata$siteID[j])
                  }
                }else{
                  if (abs(yday(banddata$NJDate[siteIdx]) - nestdata$First.Egg.Date[j]) <
                      abs(yday(nestdata$F.Day.measured[j])- nestdata$First.Egg.Date[j])){
                    nestdata$FemaleID[j] <- banddata$Band.Number[siteIdx]
                    nestdata$F.Day.measured[j]<-banddata$NJDate[siteIdx]
                    message("Found a female band ID (", nestdata$FemaleID[j], ") 
                            for nest box", nestdata$siteID[j])
                  }
                  
                }
              }
            }
            
            }
          }
        }
      }
    }
  }
  #Male
  nestdata$M.Day.measured<-as.Date(NA)
  for (j in 1:length(nestdata$MaleID)){
    if (is.na(nestdata$MaleID[j])){
      NestSite <- nestdata$siteID[j]
      if (exists(NestSite, hash_boxes)){
        
        for (siteIdx in get(NestSite, hash_boxes)){
          
          if (banddata$Year[siteIdx] == nestdata$Year[j]){
            if(!is.na(banddata$Sex[siteIdx])){
            if (banddata$Sex[siteIdx] == "M"){
              if ( is.na(nestdata$MaleID[j])){
                if(is.na(nestdata$M.Day.measured[j])){
                  next
                  }else {
                nestdata$MaleID[j] <- banddata$Band.Number[siteIdx]
                nestdata$M.Day.measured[j]<-banddata$NJDate[siteIdx]
                message("Found a Male band ID (", nestdata$MaleID[j], ") for nest box", nestdata$siteID[j])
                  }
                } else {
                if (!is.na (nestdata$Fledge.Fail.date[j])) {
                  
                  if (banddata$NJDate[siteIdx] < nestdata$Fledge.Fail.date[j] &
                      abs(yday(banddata$NJDate[siteIdx]) - nestdata$First.Egg.Date[j]) <
                      abs(yday(nestdata$M.Day.measured[j])- nestdata$First.Egg.Date[j])){
                    nestdata$MaleID[j] <- banddata$Band.Number[siteIdx]
                    nestdata$M.Day.measured[j] <- banddata$NJDate[siteIdx]
                    message("Found a Male band ID (", nestdata$MaleID[j], ") 
                            for nest box", nestdata$siteID[j])
                  } else{
                    
                    if ( abs( yday(banddata$NJDate[siteIdx] ) - nestdata$First.Egg.Date[j]) <
                        abs( yday(nestdata$M.Day.measured[j] )- nestdata$First.Egg.Date[j])) {
                      nestdata$MaleID[j] <- banddata$Band.Number[siteIdx]
                      nestdata$M.Day.measured[j]<-banddata$NJDate[siteIdx]
                      message("Found a Male band ID (", nestdata$MaleID[j], 
                              ") for nest box", nestdata$siteID[j])
                      
                    }
                  }
                }
                
              }
            }
            }
          }
        }
      }  
    }
  }
  return(nestdata)  
}
