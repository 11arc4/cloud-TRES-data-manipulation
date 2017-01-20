outerdir <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/4 malaria status added"
listfilenames <- list.files(outerdir)
filename <- paste( outerdir, listfilenames[1], sep="/")
nestdata<- read.csv(filename, as.is=TRUE, na.strings=c("", "NA"))
bird_hash <- new.env(hash=TRUE, parent=emptyenv())
nest_hash <- new.env(hash=TRUE, parent=emptyenv())
nestlings_hash <- new.env(hash=TRUE, parent=emptyenv())





year<- nestdata$Year[1]
for (i in 1: length(nestdata$Year)){
  nestID <- paste (as.character(year), nestdata$BoxID[i], sep="-")  #This is the unique 
  nest <- Nest(year=year, siteID=nestdata$siteID[i] )
  
  #Need to create (or append) sightings of the parents as TreeSwallows
  if (! is.na(nestdata$FemaleID[i]) ) {
    
    femaleID <- as.character(nestdata$FemaleID[i])
    
    if(exists(femaleID, bird_hash)) {
      # find the female from its ID and enter it here
      nest$femaleID <- TreeSwallowPointer( femaleID, bird_hash)
      
    } else {
      #need to create a new TreeSwallow entry for that bird, put it into the hash, and  
      TRESentry <- TreeSwallow(bandID=femaleID)  #NOT LETTING ME DO THIS BUT I CAN"T FIGURE OUT WHY NOT
      assign(x=femaleID, value=TRESentry, bird_hash)
      nest$femaleID <- TreeSwallowPointer(femaleID, bird_hash)
    }
  } else {
    nest$femaleID <- TreeSwallowPointer(bandID= NA_character_, bird_hash)
  }
  
  if (!is.na(nestdata$MaleID[i])){
    maleID <- as.character(nestdata$MaleID[i])
    if(exists(maleID, bird_hash)){
      TRESentry <- get(maleID, bird_hash)
      TRESentry$addNest(nestID)
      nest$maleID <- TreeSwallowPointer(maleID, bird_hash)
    } else {
      TRESentry <- TreeSwallow(bandID=maleID)
      TRESentry$addNest(nestID)
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
  
  #Need to make all the nestlings, and also need to add them to the data
  for (j in 1:10){
    bandoptions<-c("band.1","band.2", "band.3",
                   "band.4", "band.5", "band.6",
                   "band.7", "band.8", "band.8","band.10")
    if(!is.na(bandoptions[j]) ){
    chick<- CreateNestlingFromNestData(nestdata=nestdata, rownumber = i, chicknumber = j)
    }
    if(exists(chick$fromNest, nestlings_hash)){
      assign(x=chick$fromNest, values=append(x=get(chick$fromNest, nestlings_hash) , values=chick), nestlings_hash)
      
    } else{
    assign(x=chick$fromNest, values=chick, nestlings_hash)
    }
  }
  nest$nestlings <- get(nestID, nestlings_hash)
  
  #put the new nest into the hash
  assign(x=nestID, value=nest, envir=nest_hash) 
  
  
}


#as.list(nest_hash)
#pulls out all the stuff in nest_hash (the actual stuff not just the names)
