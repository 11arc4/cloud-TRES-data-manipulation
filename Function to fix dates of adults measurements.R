#need to just quickly improve the nestdata measurement dates
CleanUpNestDataObservationDates <- function (nestdata){
  
  for (K in 1:length( nestdata$Year) ){
    if(!is.na(nestdata$F.Day.measured[K])){
      date <- as.character(nestdata$F.Day.measured[K])
      if(nchar(date) ==6){
        nestdata$F.Day.measured[K] <- as.character(as.Date(date, format= "%y%m%d"))
      } else {
        nestdata$F.Day.measured[K] <- as.character(as.Date(date, format= "%Y%m%d"))
        
      }
    }
    if (!is.na(nestdata$M.Day.measured[K])){
      date <- as.character(nestdata$M.Day.measured[K])
      
      if(nchar(nestdata$M.Day.measured[K])==6){
        nestdata$M.Day.measured[K] <- as.character(as.Date(date, format= "%y%m%d"))
      } else {
        nestdata$M.Day.measured[K] <- as.character(as.Date(date, format= "%Y%m%d"))
        
      }
    }
    
  }
  return(nestdata)
}
