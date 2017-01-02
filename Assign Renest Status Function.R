#Function to add a renest column to the data based on whether you are a repeat or not

#MAKING A RENEST COLUMN
#going to deal with renest column and box IDs at the same time, because now
#it matters whether there are () denoting nest number


AssignRenestStatus<-function (nestdata) {
  nestdata<-nestdata[order(nestdata$BoxID),]
  #reorders nest data by box ID because that's a requirement later
  nestdata$BoxID<-as.character(nestdata$BoxID)
  for (i in 1:length(nestdata$BoxID)){
    nestID<-strsplit(nestdata$BoxID[i], split=" ")
    nestdata$siteID[i]<-sapply(nestID, "[", 1)
    #siteID is now the normal BoxID that doesn't tell us anything about wehther
    #there was a renest or not
    R<-sapply(nestID, "[", 2) #
    if (is.na(R) | R=="(1)" ){
      nestdata$renest.status[i]<-"First"
    } else {
      
      if (is.na(nestdata$FemaleID[i]) | 
          is.na(nestdata$MaleID[i]) |
          is.na(nestdata$FemaleID[i-1]) | 
          is.na(nestdata$MaleID[i-1])  ) {
        if (is.na(nestdata$FemaleID[i]) & 
            is.na(nestdata$MaleID[i]) & 
            is.na(nestdata$FemaleID[i-1]) & 
            is.na(nestdata$MaleID[i-1])){            
          nestdata$renest.status[i] <- "Possible Renest"
        } else {
          if ( ( ! (is.na(nestdata$FemaleID[i-1]) |  # first clause:  neither female is NA and they match
                    is.na(nestdata$Female[i]) ) &
                 nestdata$FemaleID[i] == nestdata$FemaleID[i-1] ) |
               ( ! (is.na(nestdata$MaleID[i-1]) |  # second clause:  neither male is NA and they match
                    is.na(nestdata$MaleID[i]) ) &
                 nestdata$MaleID[i] == nestdata$MaleID[i-1] ) ) {
            nestdata$renest.status[i] <- "Renest"
          } else {
            nestdata$renest.status[i] <- "First" 
          }
        } 
      } else {
        if (nestdata$FemaleID[i]==nestdata$FemaleID[i-1] & nestdata$MaleID[i]==nestdata$MaleID[i-1]){
          nestdata$renest.status[i] <- "Renest"
        } else {
          nestdata$renest.status[i] <- "First" 
        }
        
      }
    }
  }
return(nestdata)
}

