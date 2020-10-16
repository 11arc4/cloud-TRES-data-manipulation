#Update the nest data structure to include all the birds in my master band data file
#functionally this adds all the floaters as well as a surprising number of nestlings
library(lubridate)
if ("11arc" == Sys.getenv("USERNAME")) {
  banddir <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data"
  
  
} 

if ("Lab_Users" == Sys.getenv("USERNAME")) {
  banddir <- "~/Amelia TRES data 1975-2016/Improved and Cleaned Data"
} 
if ("CoxA" == Sys.getenv("USERNAME")) {
  banddir <- "C:/Users/coxa/Documents/MyRepositories/TRES-IPM/data/Improved and Cleaned Data"
} 

library(beepr)

bandfilename <- paste( banddir, "1975-2017 Bands.csv", sep="/")

#Load in the updated band data from 1975-2017
band <- read.csv(file=bandfilename, as.is=TRUE, na.strings=c("", "NA"))
#Set all the columns to be the correct type of value
band$Age <- as.character (band$Age)
band$BandID <-as.character (band$BandID)

band$Nineth.Primaries <- as.numeric(band$Nineth.Primaries)
band$Nineth.Primaries[which(band$Nineth.Primaries==0)] <- NA

band$Tarsus <- as.numeric (band$Tarsus)
# band$Tarus[which(band$Tarsus==0)] <- NA  There are no places where tarsus is mistakenly put in as 0 so don't need this

band$Mass <- as.numeric( band$Mass)
band$Mass[which(band$Mass==0)] <- NA

band$Nineth.Primaries <- as.numeric(band$Nineth.Primaries)
band$Nineth.Primaries[which(band$Nineth.Primaries==0)] <- NA

#There are a couple of entries in the banddata where age and sex are both NA.
#Those entries appear to be lost bands and therefore unusable so we need to
#remove them from the band data
band<- band[which(!is.na(band$Age) | !is.na(band$Sex)), ]

#Some of the band IDs are actually color codes--want to remove those from the data
band <- band[which(nchar(band$BandID)>5), ]
band$Sex[which(band$Sex=="m" |band$Sex=="M ")]<- "M"
band$Sex[which(band$Sex=="F ")]<- "F"
band$Sex[which(band$Sex=="?" | band$Sex=="3Y" |band$Sex=="6"| band$Sex=="7"|band$Sex=="u"| band$Sex=="U ")]<- "U"
band$Age[which((band$Sex=="F" | band$Sex=="M") & is.na(band$Age))]<- "AHY"
band$Age[which((band$Sex=="U") & is.na(band$Age))]<- "HY"


#Fix band date format 

band$Date[which(grepl("/", band$Date))] <- as.character(as.Date(as.character(band$Date[which(grepl("/", band$Date))]), format= "%m/%d/%Y"))
band$Date[which(nchar(band$Date)<10)] <- paste(substr(band$Date[which(nchar(band$Date)<10)], 1, 2), "0", substr(band$Date[which(nchar(band$Date)<10)], 3, 6), sep = "")
band$Date[which(nchar(band$Date)<10)] <- as.character(as.Date(band$Date[which(nchar(band$Date)<10)], format="%d%m%y"))

#Fix up years (a couple are NA). 
band <- band[which(!is.na(band$Year) & !is.na(band$Date)),] #If we don't know which year or the date, then there's no good place to put the data. We don't know where it comes from.... OOPS
band$Year[which(is.na(band$Year) & !is.na(band$Date))] <- as.integer(substring(band$Date[which(is.na(band$Year) & !is.na(band$Date))], 1, 4))


#here we want to create TreeSwallows as needed, or add in new observations IF
#the date on those observations doesn't match up with one that already exists
#(this will happen when we already used that measurement to fill in measurements
#in the nest data)
for ( i in 1: length(band$BandID)){
  bandID <- band$BandID[i]  #If this entry is a nestling, then we check to see if it exists in the hash
  if (band$Age[i]=="L" | band$Age[i]== "HY"){
    if (!exists(bandID, globalData$birds) ){
      #if the nestling wasn't already made then we need to make this nestling
      #and attach it to the appropriate nest if we can
      #also need to create an associated tree swallow and link it
      
      #Nestling will appear in this section that aren't already added to the
      #nestdatafiles because when we added the nestlings the first time we only
      #added extra nestlings if there were no other nestlings that we already
      #knew information about (ie if there was already 2 of the 5 nestlings, we
      #didn't add the extra 3 but it turns out there are 3 more! Oh dear. Be
      #extra careful to use the global data NOT the nestdata if you want to do a
      #multiyear analysis)
      
      
      fromNest <- paste (as.character(band$Year[i]), band$BoxID[i], sep="-") 
      bird <- TreeSwallow(bandID=bandID, hatchnest= EnvPointer(fromNest, globalData$nests) )
      yearentry <- YearsSeen(year= band$Year[i],
                             age= "HY",
                             sex = "U",
                             hatchNest = EnvPointer(fromNest, globalData$nests) )
      bird$addYearSeen(yearentry)
      #Have to deal with issues of (1) and (2) nests!
      if(exists(fromNest, globalData$nests)){
        nest <- get(fromNest, globalData$nests)
      } else {
        #then we probably have a nest that is a 1, 2, or maybe even 3 nest so 
        #will need to figure out which one it is based on the day we measured 
        #the nestlings and the fledge or fail date! We only deal with this case
        #a couple of times in the entire dataset so while it seems to work fine
        #for those couple of instances, if you want to use it for more intense
        #things, make sure you check and see if it works well in your cases too!
        
        for (p in 1:4){
          #if there are renests for this site
          if(exists(paste(fromNest, " (", p, ")", sep=""), globalData$nests)){
            nest <- get(paste(fromNest, " (", p, ")", sep=""), globalData$nests)
            #check if we can see if any fledged--if we have less nestlings 
            #checked into this nest than we had fledged then we found our nest! 
            #I'm also kind of banking on the fact that if there are renests it's
            #almost always because the nest failed early and the birds had time
            #to build a new nest and try again
            if(!is.na(nest$fledgeSize)){
              if(nest$fledgeSize>0 & nest$nestlings$length < nest$fledgeSize){
                break
              } else {
                next
              }
            } else {
              #If we don't know anything about whether the nest failed or not we
              #can check to see whether any nestlings hatched--if they hatched
              #then that is probably where these banded nestlings came from
              if(!is.na(nest$hatchSize)){
                if(nest$hatchSize>0){
                  break
                }else {
                  next
                }
              }
            }
          } else {
            message ("No nests for ", fromNest, " that meet the criteria: created new nest")
            nest <- Nest(year=band$Year[i], siteID= fromNest)
            globalData$insertNest(nest$siteID, nest)
            break
          }
        }
                
      }  
      nestlingcode <- paste(nest$siteID,  " nestling " , nest$nestlings$length +1, sep="")
      nestling <- Nestling( nestlingTRES = EnvPointer(bandID, globalData$birds), 
                            fromNest = EnvPointer(nest$siteID, globalData$nests), 
                            nestlingCode=nestlingcode)
      if(!is.na(nest$hatchDate)){
        nstgMeas <- NestlingMeasurements( age = yday(band$Date[i])-nest$hatchDate,
                                          ninthPrimary = band$Nineth.Primaries[i],
                                          mass = band$Mass[i],
                                          tarsus = band$Tarsus[i])
        nestling$addObservation(nstgMeas) 
      }
      nest$addNestling(EnvPointer(nestlingcode, globalData$nestlings)   )
      globalData$insertBird(bird = bird)
      globalData$insertNestling(nestling)
      message ("added a nestling ", bandID, " from ", fromNest, sep= " ")
    } else {
      bird <- get(bandID, globalData$birds)
      if (bird$yearsSeen$length==0) {
        if(is.na(bird$hatchnest$m_key)){
          nestcode <- paste( band$Year[i], "-", band$BoxID[i])
          bird$hatchnest <- EnvPointer(nestcode, globalData$nests)
          year <- YearsSeen(year=band$Year[i],
                            age="HY",
                            sex= "U",
                            returnstatus="Nestling",
                            hatchNest=EnvPointer(nestcode, globalData$nests))
          
          bird$addYearSeen(year)
        }
      } else {
        yseen <- bird$viewYears()
        if(band$Year[i]!=yseen[1] ){
          
          if(is.na(bird$hatchnest$m_key)){
            nestcode <- paste( band$Year[i], "-", band$BoxID[i])
            bird$hatchnest <- EnvPointer(nestcode, globalData$nests)
            year <- YearsSeen(year=band$Year[i],
                              age="HY",
                              sex= "U",
                              returnstatus="Nestling",
                              hatchNest=EnvPointer(nestcode, globalData$nests))
            
            bird$addYearSeen(year)
            #Can't add neslting Measurements because unknown how old they are
          } 
        }
      }
    }
    #if the nestling was made theres no need to do anything
  } else {
    #This is an adult bird
    if (exists (bandID, globalData$birds)){
      bird <- globalData$findBird(bandID)
      #check to see whether this is a new observation of the bird or not. If it's
      #new, add it as an observation. If it's a duplicated observation, just skip
      #it
      datesEqual=0
      yearsEqual =0
      date <- band$Date[i]
      Obs <- BodyMeasurements(date=date, 
                              wingChord = band$Nineth.Primaries[i],
                              mass = band$Mass [i], 
                              tarsus = band$Tarsus[i] )
      if (length(bird$yearsSeen$as.list())>0){
        for (year in bird$yearsSeen$as.list()){
          if(year$year==band$Year[i]){
            if(is.na(year$age)){
              year$age <- band$Age[i]
            }
            yearsEqual= yearsEqual + 1 #my way of checking to see if we've matched a year....
            if( !is.na(date) & year$observations$length>0){
              for (obs in year$observations$as.list()){
                if (!is.na(obs$date)) {
                  if (date == obs$date){
                    datesEqual= 1
                  } 
                }
              }
              if(datesEqual==0){
                #if none of the dates match up then we have a new observation of this bird and should go and add it
                year$addObservation (Obs) 
              } 
            } else {
              #If there aren't any observations from this year add them!
              if(!is.na(date)){
                year$addObservation(Obs)
              }
            } 
          }
        }
        if (yearsEqual==0){
          year <- YearsSeen(year=band$Year[i],
                            age=band$Age[i],
                            sex= band$Sex[i])
          year$addObservation(Obs)
          bird$addYearSeen(year)
          message("added an observation to ",  bandID, "from", band$Year[i], sep= " ")
        }
      }
    } else {
      
      #The bird is an adult and hasn't been seen in a nest
      # (ie it's a floater)
      sex=band$Sex[i]
      bird <- TreeSwallow(bandID=bandID, sex=sex)
      date <- band$Date[i]
      
      Obs <- BodyMeasurements(date=date, 
                              wingChord = band$Nineth.Primaries[i], 
                              mass = band$Mass [i], 
                              tarsus = band$Tarsus[i] )
      year <- YearsSeen(year=band$Year[i],
                        age=band$Age[i],
                        sex= band$Sex[i])
      year$addObservation(Obs)      
      bird$addYearSeen(year)
      globalData$insertBird( bird)
      message("added a floater bird", bandID, "from", year$year, sep=" ")
    }
  }
}
beep(1)

