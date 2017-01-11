#Function to add a renest column to the data based on whether you are a repeat or not

#MAKING A RENEST COLUMN
#going to deal with renest column and box IDs at the same time, because now
#it matters whether there are () denoting nest number


AssignRenestStatus <- function (nestdata) {
  nestdata <- nestdata[order(nestdata$BoxID), ]
  #reorders nest data by box ID because that's a requirement later
  nestdata$BoxID <- as.character(nestdata$BoxID)
  for (i in 1:length(nestdata$BoxID)) {
    nestID <- strsplit(nestdata$BoxID[i], split = " ")
    nestdata$siteID[i] <- sapply(nestID, "[", 1)
    #siteID is now the normal BoxID that doesn't tell us anything about wehther
    #there was a renest or not
    R <- sapply(nestID, "[", 2) #
    if (is.na(R) | R == "(1)") {
      nestdata$renest.status[i] <- "First"
    } else {
      
      #If someone is NA....
      if (is.na(nestdata$FemaleID[i]) |
          is.na(nestdata$MaleID[i]) |
          is.na(nestdata$FemaleID[i - 1]) |
          is.na(nestdata$MaleID[i - 1])) {
        #If everyone is NA then it is unknown whether it is a renest or not!
        if (is.na(nestdata$FemaleID[i]) &
            is.na(nestdata$MaleID[i]) &
            is.na(nestdata$FemaleID[i - 1]) &
            is.na(nestdata$MaleID[i - 1])) {
          
          nestdata$renest.status[i] <- "Unknown"
        } else {
          #Females match but the males have and NA
          if ( !is.na(nestdata$FemaleID[i - 1]) & !is.na(nestdata$Female[i])) {
            if(nestdata$FemaleID[i] == nestdata$FemaleID[i - 1]){
              nestdata$renest.status[i] <- "Female Renest"
            } else {
              #Females don't match and the males are NA
              nestdata$renest.status[i] <- "First"
            }
          }
                
          # Males match but the females have an NA
          if ( !is.na(nestdata$MaleID[i]) & !is.na(nestdata$MaleID[i-1])){ 
            if(nestdata$MaleID[i] == nestdata$MaleID[i - 1]){
              nestdata$renest.status[i] <- "Male Renest"
            } else {
              nestdata$renest.status[i] <- "First"
            }
                
          } 
          #If there are diagonal NAs then it's a first nest
          if( (!is.na(nestdata$MaleID[i]) | !is.na(nestdata$MaleID[i-1])) &
              ( !is.na(nestdata$FemaleID[i - 1]) | !is.na(nestdata$Female[i])) ){
            nestdata$renest.status[i] <- "First"
          }
        }
      } else {
        #Now no one is NA
        #if all the individuals match then it's a total renest
        if (nestdata$FemaleID[i] == nestdata$FemaleID[i - 1] &
            nestdata$MaleID[i] == nestdata$MaleID[i - 1]) {
          nestdata$renest.status[i] <- "All Renest"
        } else {
          #If the females match and the males don't then it's a female renest
          if((nestdata$FemaleID[i] == nestdata$FemaleID[i - 1] &
              nestdata$MaleID[i] != nestdata$MaleID[i - 1]) ){
            nestdata$renest.status[i] <- "Female Renest"
          }
          #If the males match and the females don't then it's a male renest
          if((nestdata$FemaleID[i] != nestdata$FemaleID[i - 1] &
              nestdata$MaleID[i] == nestdata$MaleID[i - 1]) ){
            nestdata$renest.status[i] <- "Male Renest"
          }
          #If no one matches then it's a first nest
          if((nestdata$FemaleID[i] != nestdata$FemaleID[i - 1] &
              nestdata$MaleID[i] != nestdata$MaleID[i - 1]) ){
            nestdata$renest.status[i] <- "First"
          }
          
        }
      }
    }
  }
  return(nestdata)
}

