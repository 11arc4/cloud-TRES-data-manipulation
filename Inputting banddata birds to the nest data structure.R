banddir <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data"

bandfilename <- paste( banddir, "1975-2016 Bands.csv", sep="/")

#Load in the updated band data from 1975-2016
band <- read.csv(file=bandfilename, as.is=TRUE, na.strings=c("", "NA"))
#Set all the columns to be the correct type of value
band$BandID <-as.character (band$BandID)
band$Wing.Chord <- as.numeric(band$Wing.Chord)
band$Tarsus <- as.numeric (band$Tarsus)
band$Mass <- as.numeric( band$Mass)
band$Ninth.Primary <- as.numeric(band$Ninth.Primary)

#There are a couple of entries in the banddata where age and sex are both NA.
#Those entries appear to be lost bands and therefore unusable so we need to
#remove them from the band data
band<- band[which(!is.na(band$Age)), ]

#here we want to create TreeSwallows as needed, or add in new observations IF
#the date on those observations doesn't match up with one that already exists
#(this will happen when we already used that measurement to fill in measurements
#in the nest data)
i=0
for ( bandID in band$BandID){
  i=i+1
#If this entry is a nestling, then we check to see if it exists in the hash
  if (band$Age[i]=="L" | band$Age[i]== "HY"){
    if (!exists(bandID, globalData$nestlings)){
      #if the nestling wasn't already made then we need to make this nestling
      #and attach it to the appropriate nest if we can
      #also need to create an associated tree swallow and link it
      fromNest <- paste (as.character(band$Year[i]), band$BoxID[i], sep="-") 
      bird <- TreeSwallow(bandID=bandID, hatchnest=EnvPointer(m_key=fromNest, m_hash=globalData$nests) )
      nestling <- Nestling( nestlingTRES = EnvPointer(m_key=bandID, m_hash=globalData$birds), 
                            fromNest = EnvPointer(m_key=fromNest, m_hash=globalData$nests))
      globalData$insertBird(bird = bird)
      globalData$insertNestling(nestling)
    }
    #if the nestling was made theres no need to do anything
  } else {
    if (exists (bandID, globalData$birds)){
      bird <- get(bandID, globalData$birds)
      #check to see whether this is a new observation of the bird or not. If it's
      #new, add it as an observation. If it's a duplicated observation, just skip
      #it
      
      datesEqual=0
      yearsEqual =0
      date <- band$Date[i]
      if (length(bird$yearsSeen$as.list())>0){
        for (year in bird$yearsSeen$as.list()){
          if(year$year==band$Year[i]){
            yearsEqual= yearsEqual + 1 #my way of checking to see if we've matched a year....
            if(length(year$observations$as.list()) > 0){
              for (obs in year$observations$as.list()){
                if (obs$equal(date)){
                  datesEqual= 1
                } 
              }
              if(datesEqual==0){
                #if none of the dates match up then we have a new observation of this bird and should go and add it
                Obs <- BodyMeasurements(date=date, bird=bird, 
                                        wingChord = band$Wing.Chord[i], 
                                        ninthPrimary = band$Ninth.Primary[i],
                                        mass = band$Mass [i], 
                                        tarsus = band$Tarsus[i] )
                
                year$observations$addElement (Obs)  
                
              }
            }
          }
        }
        if (yearsEqual==0){
          year <- YearsSeen(year=band$Year,
                            age=band$Age,
                            sex= band$Sex)
          Obs <- BodyMeasurements(date=date, bird=bird, 
                                  wingChord = band$Wing.Chord[i], 
                                  ninthPrimary = band$Ninth.Primary[i],
                                  mass = band$Mass [i], 
                                  tarsus = band$Tarsus[i] )
          
          year$observations$addElement (Obs) 
        }
      }
      
      
    } else {
      
      #The bird is an adult and hasn't been seen in a nest
      # (ie it's a floater)
      sex=band$Sex[i]
      bird <- TreeSwallow(bandID=bandID, sex=sex)
      date <- band$Date[i]
      
      Obs <- BodyMeasurements(date=date, bird=bird, 
                              wingChord = band$Wing.Chord[i], 
                              ninthPrimary = band$Ninth.Primary,
                              mass = band$Mass [i], 
                              tarsus = band$Tarsus[i] )
      year <- YearsSeen(year=band$Year[i],
                        age=band$Age[i],
                        sex= band$Sex[i])
      year$observations$addElement(Obs)
      bird$yearsSeen$addElement(year)
      globalData$insertBird(bird = bird)
      
    }
  }
}

saveRDS(globalData, "AllGlobalData.rds")
#To reload this 
#globalData <- readRDS("AllGlobalData.rds")

