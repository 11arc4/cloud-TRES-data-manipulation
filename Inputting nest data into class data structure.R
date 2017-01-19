outerdir <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/4 malaria status added"
listfilenames <- list.files(outerdir)
filename <- paste( outerdir, listfilenames[1], sep="/")
nestdata<- read.csv(filename, as.is=TRUE, na.strings=c("", "NA"))
bird_hash <- new.env(hash=TRUE, parent=emptyenv())
nest_hash <- new.env(hash=TRUE, parent=emptyenv())


BuildClassDataStructure <- function(nestdata){
  year<- nestdata$Year[i]
  
  for (i in 1: length(nestdata$Year)){
  year<- nestdata$Year[i]
  nestID <- paste (year, nestdata$BoxID[i], sep="-")  #This is the unique 
  nest <- Nest(year=year, siteID=nestdata$siteID[i] )
  
  #Need to create (or append) sightings of the parents as TreeSwallows
  if (! is.na(nestdata$FemaleID[i]) ) {
    
    femaleID <- as.character(nestdata$FemaleID[i])
    
    if(exists(femaleID, bird_hash)) {
      # find the female from its ID and enter it here
      nest$femaleID <- TreeSwallowPointer( femaleID, bird_hash)
    } else {
      #need to create a new TreeSwallow entry for that bird, put it into the hash, and  
      TRESentry <- TreeSwallow(bandID= femaleID)
      assign(x=femaleID, value=TRESentry, bird_hash)
      nest$femaleID <- TreeSwallowPointer(femaleID, bird_hash)
    }
  } else {
    nest$femaleID <- TreeSwallowPointer(bandID= NA_character_, bird_hash)
  }
  if (!is.na(nestdata$MaleID[i])){
    maleID <- as.character(nestdata$MaleID[i])
    if(exists(maleID, bird_hash)){
      nest$maleID <- TreeSwallowPointer(maleID, bird_hash)
    } else {
      TRESentry <- TreeSwallow(bandID=maleID)
      assign(x=maleID, value= TRESentry, bird_hash )
      nest$maleID <- TreeSwallowPointer(maleID, bird_hash)
    }
  } else {
    nest$maleID <- TreeSwallowPointer(bandID= NA_character_, bird_hash)
  }
  

  #Here are all the important dates
  if (!is.na(nestdata$First.Egg.Date[i])){
    nest$firstEggDate <- nestdata$First.Egg.Date[i]
  }
  if(!is.na(nestdata$Incubation.Date[i])){
    nest$lastEggDate <- nestdata$Incubation.Date[i]
  }
  
  if(!is.na(nestdata$Hatch.Date[i])){
    nest$hatchDate <- nestdata$Hatch.Date[i]
  }
  if(!is.na(nestdata$Fledge.Fail.date[i])){
    nest$fledgeDate <- nestdata$Fledge.Fail.date[i]
  }
  
  #Breeding success measurements
  if(!is.na(nestdata$Clutch.Size[i])){
    nest$clutchSize <- nestdata$Clutch.Size[i]
  }
  if(!is.na(nestdata$Hatch.Size[i])){
    nest$hatchSize <- nestdata$Hatch.Size[i]
  }
  if(!is.na(nestdata$Fledge.Size[i])){
    nest$fledgeSize <- nestdata$Fledge.Size[i]
  }
  if(!is.na(nestdata$Why.fail.[i])){
    nest$reasonforFailure - as.character(nestdata$Why.fail.[i]) 
  } 
  if(!is.na(nestdata$renest.status[i])){
    nest$renestStatus <- as.character(nestdata$renest.status[i])
  } 
   
#put the new nest into the hash
  assign(x=nestID, value=nest, envir=nest_hash) 
  
}
}

#as.list(nest_hash)
#pulls out all the stuff in nest_hash (the actual stuff not just the names)
