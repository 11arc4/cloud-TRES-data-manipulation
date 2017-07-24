inputdir<-"~/Masters Thesis Project/Tree Swallow Data/TRES data/Data Sets I really need to ADD to my current dataset/Adult banding records 2002-2016"

#Add 2002 banddata to master file
banddata2002<-read.csv(paste(inputdir, "TRES bands 2002 (MaryS).csv", sep="/"), as.is=TRUE, na.strings = c("", "NA"))
banddata2002$Day<-as.character(banddata2002$Day)
banddata2002$Month <- as.character(banddata2002$Month)
for( i in 1:length(banddata2002$Month)){
  if(!is.na(banddata2002$Day[i]) & nchar(banddata2002$Day[i])<2){
    banddata2002$Day[i]<-paste("0", as.character(banddata2002$Day[i]), sep="")
  }
  if(!is.na(banddata2002$Month[i]) & nchar(banddata2002$Month[i])<2){
    banddata2002$Month[i]<-paste("0", as.character(banddata2002$Month[i]), sep="")
  }
 if(!is.na(banddata2002$Month[i]) & !is.na(banddata2002$Day[i]) & !is.na(banddata2002$Year[i])){
  banddata2002$date[i]<- 
  paste(as.character(banddata2002$Month[i]), as.character(banddata2002$Day[i]) ,as.character(banddata2002$Year[i]), sep="/")
 }
}
banddata2002$date <- as.character(as.Date(banddata2002$date, format= "%m/%d/%Y"))
for (i in 1:length(banddata2002$Year)){
  if(!is.na(banddata2002$Blood..ul.[i])){
    banddata2002$Blood[i]<- "YES"
    
  } else{
    banddata2002$Blood[i]<- "NO"
  }
}

band<-c()
masterfile<-AddtoBandDataFile(inputfile = banddata2002, 
                              band=band, 
                              Species="Species",
                              Year="Year",
                              Date= "date",
                              BoxID="Box.ID",
                              BandID="BandID",
                              Bander="Bander.",
                              Sex="Sex", 
                              Age="Age.", 
                              Ninth.Primary=NA,	
                              Wing.Chord="WingLeft.",	
                              Mass="MassBIRD",	
                              Tarsus="TarsusLeft.",
                              Blood="Blood",
                              Plumage=NA,
                              Head="head",
                              Back="back",
                              Epaulette="epal",
                              Tail="OuterTailLeft.",
                              Forehead="Fhead.mm.",
                              Time="Time.",
                              BroodPatch=NA,	
                              CloacalProtub=NA)

MasterColnames<-  c("Year",
                    "Date", 
                    "BoxID", 
                    "BandID",
                    "Bander",
                    "Sex", 
                    "Age", 
                    "Ninth.Primary",	
                    "Wing.Chord",	
                    "Mass",	
                    "Tarsus",
                    "Blood",
                    "Plumage",
                    "Head",
                    "Back",
                    "Epaulette",
                    "Tail",
                    "Forehead",
                    "Time",
                    "BroodPatch",	
                    "CloacalProtub"
)

colnames(masterfile)<-MasterColnames

#Add in all the 1975-2001 data
banddata1975to2001<-read.csv("~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/Updated band data 1975-2001.csv",
                             as.is=TRUE, na.strings=c("", "NA"))

masterfile<-AddtoBandDataFile(inputfile = banddata1975to2001, 
                              band=masterfile, 
                              Species="Species",
                              Year="Year",
                              Date= "NJDate",
                              BoxID="Place",
                              BandID="Band.Number",
                              Bander="Bander",
                              Sex="Sex", 
                              Age="Age", 
                              Ninth.Primary="Ninth.Primary",	
                              Wing.Chord="Wing.Chord",	
                              Mass="Mass",	
                              Tarsus="Tarsus",
                              Blood="Blood",
                              Plumage="Plumage",
                              Head="Head",
                              Back="Back",
                              Epaulette="Epaulette",
                              Tail="Tail",
                              Forehead="Forehead",
                              Time="Time",
                              BroodPatch="Brood.Patch",	
                              CloacalProtub="Cloacal.Protub.")


#Add in 2003 data

banddata2003_adult<-read.csv(paste(inputdir, "2003_adults w more info.csv", sep="/"), as.is = TRUE, na.strings = c("", "NA"))
#adults first
banddata2003_adult$Year<-c(rep(2003, length(banddata2003_adult$Box)))

banddata2003_adult$Date<- format(as.Date(banddata2003_adult$Date, format="%d/%m/%Y"), "%m/%d/%Y")

masterfile<-AddtoBandDataFile(inputfile =banddata2003_adult , 
                              band=masterfile, 
                              Species=NA,
                              Year="Year",
                              Date= "Date",
                              BoxID="Box",
                              BandID="Band.Number",
                              Bander="bander",
                              Sex="sex", 
                              Age="age", 
                              Ninth.Primary=NA,	
                              Wing.Chord="Wingleft",	
                              Mass="Mass",	
                              Tarsus="TarsusLeft",
                              Blood="Bled.",
                              Tail="TailLeft",
                              
                              Time="time",
                              BroodPatch="BP"	
                              )
#now nestlings
banddata2003_nest<-read.csv(paste(inputdir, "2003tres_Nestlings.csv", sep="/"), as.is = TRUE, na.strings = c("", "NA"))

banddata2003_nest$Year<-c(rep(2003, length(banddata2003_nest$LOCATION)))
banddata2003_nest$Age<-c(rep("L", length(banddata2003_nest$LOCATION)))
masterfile<-AddtoBandDataFile(inputfile = banddata2003_nest, 
                              band=masterfile, 
                              Species=NA,
                              Year="Year",
                              Date= "Date.2ndmeasure",
                              BoxID="LOCATION",
                              BandID="final.band.number",
                              Bander="BRD",
                              Age="Age", 
                              Ninth.Primary="LEFT.9TH.PRIMARY",	
                              Wing.Chord="LEFT.WING.CORD",	
                              
                              Mass="MASS.day15.",	
                              Tarsus="LEFT.TARSUS.1",
                              Blood="BLED.1"
                              )

#Add in 2004  Data
#ADULTS 2004
banddata2004_adult<-read.csv(paste(inputdir, "2004banding_ADULT.csv", sep="/"), as.is = TRUE, na.strings = c("", "NA"))
banddata2004_adult$Year<-c(rep(2004, length(banddata2004_adult$bander)))
banddata2004_adult$date<-as.Date(as.character(banddata2004_adult$date), format="%Y%m%d")
masterfile<-AddtoBandDataFile(inputfile = banddata2004_adult, 
                              band=masterfile, 
                              Species=NA,
                              Year="Year",
                              Date= "date",
                              BoxID="BOX..",
                              BandID="BAND..",
                              Bander="bander",
                              Sex="SEX", 
                              Age="AGE", 
                              Ninth.Primary=NA,	
                              Wing.Chord="rightWC",	
                              Mass="mass",	
                              Tarsus="tarsus",
                              Blood="bled",
                              Plumage="plumage",
                              Head="HEAD",
                              Back="BACK",
                              Epaulette="EPAULETS",
                              Tail="TAIL",
                              Forehead=NA,
                              Time="time",
                              BroodPatch="BP",	
                              CloacalProtub="CP")

#NESTLINGS 2004

banddata2004_nest<-read.csv(paste(inputdir, "2004banding_NESTLING.csv", sep="/"), as.is = TRUE, na.strings = c("", "NA"))
banddata2004_nest$Year<-c(rep(2004, length(banddata2004_nest$bander)))
banddata2004_nest$date<-as.Date(as.character(banddata2004_nest$date), format="%Y%m%d")

masterfile<-AddtoBandDataFile(inputfile = banddata2004_nest, 
                              band=masterfile, 
                              Species=NA,
                              Year="Year",
                              Date= "date",
                              BoxID="BOX..",
                              BandID="BAND..",
                              Bander="bander",
                              Sex="SEX", 
                              Age="AGE", 
                              Ninth.Primary=NA,	
                              Wing.Chord="rightWC",	
                              Mass="mass",	
                              Tarsus="tarsus",
                              Blood="bled",
                              Plumage="plumage",
                              Head="HEAD",
                              Back="BACK",
                              Epaulette="EPAULETS",
                              Tail="TAIL",
                              Forehead=NA,
                              Time="time",
                              BroodPatch="BP",	
                              CloacalProtub="CP")

banddata2005<-read.csv(paste(inputdir, "2005_BANDING_final_26July05.csv", sep="/"), as.is=TRUE, na.strings=c("", "NA"))

masterfile<-AddtoBandDataFile(inputfile = banddata2005, 
                              band=masterfile, 
                              Species=NA,
                              Year="Year",
                              Date= "DATE",
                              BoxID="Box..",
                              BandID="Band.Number",
                              Bander="Bander.",
                              Sex="Sex", 
                              Age="Age", 
                              Ninth.Primary=NA,	
                              Wing.Chord="Left.Wing",	
                              Mass="BIRD.mass",	
                              Tarsus="Left.Tarsus",
                              Blood=NA,
                              Plumage=NA,
                              Head="X..blue.head",
                              Back="X..blue.back",
                              Epaulette="X..blue.epal",
                              Tail="X..blue.tail",
                              Forehead="X..blue.Fhead.mm.",
                              Time="Time"
                              )

#Add 2006
banddata2006<-read.csv(paste(inputdir, "TRES_06.csv", sep="/"), as.is=TRUE, na.strings = c("", "NA"))
banddata2006$Year<-c(rep(2006, length(banddata2006$date)))
masterfile<-AddtoBandDataFile(inputfile = banddata2006, 
                              band=masterfile, 
                              Species=NA,
                              Year="Year",
                              Date= "date",
                              BoxID=NA,
                              BandID="Band..",
                              Bander="Bander",
                              Sex="Sex", 
                              Age="Age" 
                              )
#Add 2007
banddata2007<-read.csv(paste(inputdir, "TRES_07.csv", sep="/"), as.is=TRUE, na.strings = c("", "NA"))
banddata2007$Year<-c(rep(2007, length(banddata2007$Banding.Date)))
masterfile<-AddtoBandDataFile(inputfile = banddata2007, 
                              band=masterfile, 
                              Species="Species",
                              Year="Year",
                              Date= "Banding.Date",
                              BoxID=NA,
                              BandID="Band.Number",
                              Bander=NA,
                              Sex="Sex", 
                              Age="Age", 
                              Ninth.Primary=NA,	
                              Wing.Chord="Wing.Cord",	
                              Mass="Bird.Weight",	
                              Tarsus="Tarsus.Length"
                              )

#Add 2008
#New
banddata2008_new<-read.csv(paste(inputdir, "2008new.csv", sep="/"), as.is=TRUE, na.strings = c("", "NA"))
banddata2008_new$Year<-c( rep(2008, length(banddata2008_new$Band.Number)))
for(test in banddata2008_new$Test.Preformed){
if(!is.na(test)){
  banddata2008_new$Blood<-"YES"
} else{
  banddata2008_new$Blood<-"NO"
}
}
masterfile<-AddtoBandDataFile(inputfile = banddata2008_new, 
                              band=masterfile, 
                              Species="Species",
                              Year="Year",
                              Date= "Banding.Date",
                              BoxID="Location",
                              BandID="Band.Number",
                              Bander="Bander.ID",
                              Sex="Sex", 
                              Age="Age", 
                              Ninth.Primary=NA,	
                              Wing.Chord="Wing.Cord",	
                              Mass="Bird.Weight",	
                              Tarsus="Tarsus.Length",
                              Blood="Blood"
                              )



#Recapture
banddata2008_recapture<-read.csv(paste(inputdir, "2008recapture.csv", sep="/"), as.is=TRUE, na.strings = c("", "NA"))
banddata2008_recapture$Year<-c( rep(2008, length(banddata2008_recapture$Band.Number)))
for(test in banddata2008_recapture$Test.preformed){
  if(!is.na(test)){
    banddata2008_recapture$Blood<-"YES"
  } else{
    banddata2008_recapture$Blood<-"NO"
  }
}
masterfile<-AddtoBandDataFile(inputfile = banddata2008_recapture, 
                              band=masterfile, 
                              Species="Species",
                              Year="Year",
                              Date= "Banding.Date",
                              BoxID="Location",
                              BandID="Band.Number",
                              Bander="Bander.ID",
                              Sex="Sex", 
                              Age="Age", 
                              Ninth.Primary=NA,	
                              Wing.Chord="Wing.Cord",	
                              Mass="Bird.Weight",	
                              Tarsus="Tarsus.Length",
                              Blood="Blood"
)



#Add 2009 
#New 2009
banddata2009_new<-read.csv(paste(inputdir, "2009new.csv", sep="/"), as.is=TRUE, na.strings = c("", "NA"))

for(i in 1:length(banddata2009_new$DISPOSITION)){
if(!is.na(banddata2009_new$MONTH[i]) & 
   !is.na(banddata2009_new$DAY[i]) &
   !is.na( banddata2009_new$YEAR[i])){
banddata2009_new$Date[i]<-paste(banddata2009_new$MONTH[i], banddata2009_new$DAY[i], banddata2009_new$YEAR[i], sep="/")
} else {
  banddata2009_new$Date[i]<-NA
}
}

j=0
for (test in banddata2009_new$TESTS.PERFORMED){
  j=j+1
  if(!is.na(test)){
    banddata2009_new$Blood[j]<-"YES"
  } else{
    banddata2009_new$Blood[j]<-"NO"
  }
}
masterfile<-AddtoBandDataFile(inputfile = banddata2009_new, 
                              band=masterfile, 
                              Species=NA,
                              Year="YEAR",
                              Date= "Date",
                              BoxID=NA,
                              BandID="BAND.NUMBER",
                              Bander="BANDER.ID",
                              Sex="SEX", 
                              Age="AGE", 
                              Ninth.Primary=NA,	
                              Wing.Chord="WING.CHORD",	
                              Mass="BIRD.WEIGHT",	
                              Tarsus="TARSUS.LENGTH",
                              Blood="Blood"
                             
)

#Add 2009 recaptures
banddata2009_recapture<-read.csv(paste(inputdir, "2009recaptures.csv", sep="/"), as.is=TRUE, na.strings = c("", "NA"))
for(i in 1:length(banddata2009_recapture$DISPOSITION)){
  if(!is.na(banddata2009_recapture$MONTH[i]) & 
     !is.na(banddata2009_recapture$DAY[i]) &
     !is.na( banddata2009_recapture$YEAR[i])){
    banddata2009_recapture$Date[i]<-paste(banddata2009_new$MONTH[i], banddata2009_new$DAY[i], banddata2009_new$YEAR[i], sep="/")
  } else {
    banddata2009_recapture$Date[i]<-NA
  }
}

j=0
for (test in banddata2009_recapture$TESTS.PERFORMED){
  j=j+1
  if(!is.na(test)){
    banddata2009_recapture$Blood[j]<-"YES"
  } else{
    banddata2009_recapture$Blood[j]<-"NO"
  }
}
masterfile<-AddtoBandDataFile(inputfile = banddata2009_recapture, 
                              band=masterfile, 
                              Species=NA,
                              Year="YEAR",
                              Date= "Date",
                              BoxID=NA,
                              BandID="BAND.NUMBER",
                              Bander="BANDER.ID",
                              Sex="SEX", 
                              Age="AGE", 
                              Ninth.Primary=NA,	
                              Wing.Chord="WING.CHORD",	
                              Mass="BIRD.WEIGHT",	
                              Tarsus="TARSUS.LENGTH",
                              Blood="Blood"
                              
)


#Add 2010
#Adults
banddata2010_adults<-read.csv(paste(inputdir, "2010adults.csv", sep="/"), as.is=TRUE, na.strings = c("", "NA"))
banddata2010_adults$Year<-2010
masterfile<-AddtoBandDataFile(inputfile = banddata2010_adults, 
                              band=masterfile, 
                              Species=NA,
                              Year="Year",
                              Date= "DATE",
                              BoxID="BOX.ID",
                              BandID="band.number",
                              Bander=NA,
                              Sex="SEX", 
                              Age="AGE", 
                              Ninth.Primary=NA,	
                              Wing.Chord="WING..mm.",	
                              Mass="MASS..g.",	
                              Tarsus="TARSUS..mm.",
                              Blood="blood.",
                              Plumage="cumltive.plumage",
                              Head="plumage.score..H.",
                              Back="plumage.score..M.",
                              Epaulette="plumage.score..E.",
                              Tail="plumage.score..R.",
                              Forehead=NA,
                              Time="release.time",
                              BroodPatch=NA,	
                              CloacalProtub=NA)

#Nestlings
banddata2010_nesting<-read.csv(paste(inputdir, "2010nestlings.csv", sep="/"), as.is=TRUE, na.strings = c("", "NA"))
banddata2010_nesting$Year<-2010
banddata2010_nesting$AGE<-"L"
masterfile<-AddtoBandDataFile(inputfile = banddata2010_nesting, 
                              band=masterfile, 
                              Species=NA,
                              Year="Year",
                              Date= "day.d12",
                              BoxID="BOX.ID",
                              BandID="band.number",
                              Bander=NA,
                              Sex=NA, 
                              Age="AGE", 
                              Ninth.Primary=NA,	
                              Wing.Chord=NA,	
                              Mass="mass.d12",	
                              Tarsus="tarsus.d12",
                              Blood="blood.",
                              Plumage=NA,
                              Head=NA,
                              Back=NA,
                              Epaulette=NA,
                              Tail=NA,
                              Forehead=NA,
                              Time=NA,
                              BroodPatch=NA,	
                              CloacalProtub=NA)

#Add 2011
#2011 adults
banddata2011_adult<-read.csv(paste(inputdir, "2011adult.csv", sep="/"), as.is=TRUE, na.strings=c("", "NA", "N/A", " ", "-"))

banddata2011_adult <- banddata2011_adult[1:95, ]
banddata2011_adult$Year<-2011
for (blood in banddata2011_adult$blood.){
  if(is.na(blood)){
    banddata2011_adult$BLOOD<- "NO"
  } else{
    if(blood=="Y"){
      banddata2011_adult$BLOOD<-"YES"
    } else {
      banddata2011_adult$BLOOD<- "NO"
    }
  }
}


banddata2011_adult$Plumage.value <- ( 25 *banddata2011_adult$plumage.score..H. + 
                                       20 *banddata2011_adult$plumage.score..E. + 
                                       50 * banddata2011_adult$plumage.score..M. +
                                       5* banddata2011_adult$plumage.score..R.)/100

masterfile<-AddtoBandDataFile(inputfile = banddata2011_adult, 
                              band=masterfile, 
                              Species=NA,
                              Year="Year",
                              Date= "DATE",
                              BoxID="BOX.ID",
                              BandID="band.number",
                              Bander=NA,
                              Sex="SEX", 
                              Age=NA, 
                              Ninth.Primary=NA,	
                              Wing.Chord= "WING..mm.",	
                              Mass="MASS..g.",	
                              Tarsus="TARSUS..mm.",
                              Blood="BLOOD",
                              Plumage="Plumage.value",
                              Head="plumage.score..H.",
                              Back="plumage.score..M.",
                              Epaulette="plumage.score..E.",
                              Tail="plumage.score..R.",
                              Time="release.time",
                              BroodPatch=NA,	
                              CloacalProtub=NA)


#2011 nestlings
banddata2011_nest<-read.csv(paste(inputdir
                                  , "2011nestling.csv", sep="/"), as.is=TRUE, na.strings = c("","NA"))
banddata2011_nest$Year<-2011
banddata2011_nest$AGE<-"L"
for (blood in banddata2011_nest$blood.){
  if(blood=="Y"){
    banddata2011_nest$BLOOD<-"YES"
  } else {
    banddata2011_nest$BLOOD<- "NO"
  }
}

masterfile<-AddtoBandDataFile(inputfile = banddata2011_nest, 
                              band=masterfile, 
                              Species=NA,
                              
                              Year="Year",
                              Date= "day.d12",
                              BoxID="BOX.ID",
                              BandID="band.number",
                              Bander=NA,
                              Sex=NA, 
                              Age="AGE", 
                              Ninth.Primary=NA,	
                              Wing.Chord=NA,	
                              Mass="mass.d12",	
                              Tarsus="tarsus.d12",
                              Blood="blood.",
                              Plumage=NA,
                              Head=NA,
                              Back=NA,
                              Epaulette=NA,
                              Tail=NA,
                              Forehead=NA,
                              Time=NA,
                              BroodPatch=NA,	
                              CloacalProtub=NA)


#Add 2012
#Adults 2012
banddata2012_adults<-read.csv(paste(inputdir, "2012adult.csv", sep="/"), as.is=TRUE, na.strings=c("", "N/A", "NA"))
banddata2012_adults$Year<-2012
masterfile<-AddtoBandDataFile(inputfile = banddata2012_adults, 
                              band=masterfile, 
                              Species=NA,
                              Year="Year",
                              Date= "DATE",
                              BoxID="BOX.ID",
                              BandID="band.number",
                              Bander=NA,
                              Sex="SEX", 
                              Age="bird.type", 
                              Ninth.Primary=NA,	
                              Wing.Chord= "WING..mm.",	
                              Mass="MASS..g.",	
                              Tarsus="TARSUS..mm.",
                              Blood="blood.",
                              Plumage="Plumage.value",
                              Head="plumage.score..H.",
                              Back="plumage.score..M.",
                              Epaulette="plumage.score..E.",
                              Tail="plumage.score..R.",
                              Forehead=NA,
                              Time="release.time",
                              BroodPatch=NA,	
                              CloacalProtub=NA)

#Nestlings 2012
banddata2012_nest<-read.csv(paste(inputdir, "2012nestling.csv", sep="/"), as.is=TRUE, na.strings=c("", "NA", "N/A"))
banddata2012_nest$Year<-2011
banddata2012_nest$AGE<-"L"
for (blood in banddata2012_nest$blood.){
  if(blood=="Y"){
    banddata2012_nest$BLOOD<-"YES"
  } else {
    banddata2012_nest$BLOOD<- "NO"
  }
}

masterfile<-AddtoBandDataFile(inputfile = banddata2012_nest, 
                              band=masterfile, 
                              Species=NA,
                              
                              Year="Year",
                              Date= "day.d12",
                              BoxID="BOX.ID",
                              BandID="band.number",
                              Bander=NA,
                              Sex=NA, 
                              Age="AGE", 
                              Ninth.Primary=NA,	
                              Wing.Chord=NA,	
                              Mass="mass.d12",	
                              Tarsus="tarsus.d12",
                              Blood="blood.",
                              Plumage=NA,
                              Head=NA,
                              Back=NA,
                              Epaulette=NA,
                              Tail=NA,
                              Forehead=NA,
                              Time=NA,
                              BroodPatch=NA,	
                              CloacalProtub=NA)

#Add 2013--only adults available in a decent format. 
banddata2013<-read.csv(paste(inputdir, "2013adult.csv", sep="/"), as.is=TRUE, na.strings = c("", "NA"))
banddata2013$Year<-2013
masterfile<-AddtoBandDataFile(inputfile = banddata2013, 
                              band=masterfile, 
                              Species=NA,
                              Year="Year",
                              Date= "DATE",
                              BoxID="BOX.ID",
                              BandID="band.number",
                              Bander=NA,
                              Sex="SEX", 
                              Age="bird.type", 
                              Ninth.Primary=NA,	
                              Wing.Chord= "WING..mm.",	
                              Mass="MASS..g.",	
                              Tarsus="TARSUS..mm.",
                              Blood="blood.",
                              Plumage="Plumage.value",
                              Head="plumage.score..H.",
                              Back="plumage.score..M.",
                              Epaulette="plumage.score..E.",
                              Tail="plumage.score..R.",
                              Time="release.time",
                              BroodPatch=NA,	
                              CloacalProtub=NA)


#Add 2014 birds. Again only the adults are available in a decent format
banddata2014<-read.csv(paste(inputdir, "2014adult.csv", sep="/"), as.is=TRUE, na.strings = c("", "NA"))
banddata2014$Year<-2014
masterfile<-AddtoBandDataFile(inputfile = banddata2014, 
                              band=masterfile, 
                              Species=NA,
                              Year="Year",
                              Date= "DATE",
                              BoxID="BOX.ID",
                              BandID="band.number",
                              Bander=NA,
                              Sex="SEX", 
                              Age="bird.type", 
                              Ninth.Primary=NA,	
                              Wing.Chord= "WING..mm.",	
                              Mass="MASS..g.",	
                              Tarsus="TARSUS..mm.",
                              Blood="blood.",
                              Plumage="Plumage.value",
                              Head="plumage.score..H.",
                              Back="plumage.score..M.",
                              Epaulette="plumage.score..E.",
                              Tail="plumage.score..R.",
                              Forehead=NA,
                              Time="release.time",
                              BroodPatch=NA,	
                              CloacalProtub=NA)
#Add 2015--only the adults are in a decent format for adding
banddata2015<-read.csv(paste(inputdir, "2015adult.csv", sep="/"), as.is=TRUE, na.strings = c("", "NA"))
banddata2015$Year<-2015
masterfile<-AddtoBandDataFile(inputfile = banddata2015, 
                              band=masterfile, 
                              Species=NA,
                              Year="Year",
                              Date= "DATE",
                              BoxID="BOX.ID",
                              BandID="band.number",
                              Bander=NA,
                              Sex="SEX", 
                              Age="bird.type", 
                              Ninth.Primary=NA,	
                              Wing.Chord= "WING..mm.",	
                              Mass="MASS..g.",	
                              Tarsus="TARSUS..mm.",
                              Blood="blood.",
                              Plumage="Plumage.value",
                              Head="plumage.score..H.",
                              Back="plumage.score..M.",
                              Epaulette="plumage.score..E.",
                              Tail="plumage.score..R.",
                              Forehead=NA,
                              Time="release.time",
                              BroodPatch=NA,	
                              CloacalProtub=NA)


#Add 2016 adults only because nestlings are in a bad format

banddata2016<-read.csv(paste(inputdir, "all 2016 TRES data.csv", sep="/"), as.is=TRUE, na.strings = c("", "NA"))
banddata2016$Year<-2016
masterfile<-AddtoBandDataFile(inputfile = banddata2016, 
                              band=masterfile, 
                              Species=NA,
                              Year="Year",
                              Date= "DATE",
                              BoxID="BOX.ID",
                              BandID="band.number",
                              Bander=NA,
                              Sex="SEX", 
                              Age=NA, 
                              Ninth.Primary=NA,	
                              Wing.Chord= "WING..mm.",	
                              Mass="MASS..g.",	
                              Tarsus="TARSUS..mm.",
                              Blood="blood.",
                              Plumage="Plumage.value",
                              Head="plumage.score..H.",
                              Back="plumage.score..M.",
                              Epaulette="plumage.score..E.",
                              Tail="plumage.score..R.",
                              Forehead=NA,
                              Time="time.of.capture",
                              BroodPatch=NA,	
                              CloacalProtub=NA)

#Add in 2017 adults only because nestlings are all already in from the nest data

banddata2017<-read.csv(paste(inputdir, "TRES 2017 adults bands.csv", sep="/"), as.is=TRUE, na.strings = c("", "NA"))
banddata2017$Year<-2017
for (i in 1:nrow(banddata2017)){
  if(!is.na(banddata2017$RFID.shows.bird.belongs.at[i])){
    banddata2017$FinalizedBoxID[i] <-banddata2017$RFID.shows.bird.belongs.at[i] 
      
  } else {
    banddata2017$FinalizedBoxID[i] <- banddata2017$BOX.ID[i] 
      
  }
}

masterfile<-AddtoBandDataFile(inputfile = banddata2017, 
                              band=masterfile, 
                              Species=NA,
                              Year="Year",
                              Date= "DATE",
                              BoxID="FinalizedBoxID",
                              BandID="band.number",
                              Bander=NA,
                              Sex="SEX", 
                              Age="Age..from.feathers.", 
                              Ninth.Primary=NA,	
                              Wing.Chord= "WING..mm.",	
                              Mass="MASS..g.",	
                              Tarsus="TARSUS..mm.",
                              Blood="blood.",
                              Plumage="Plumage.value",
                              Head="plumage.score..H.",
                              Back="plumage.score..M.",
                              Epaulette="plumage.score..E.",
                              Tail="plumage.score..R.",
                              Forehead=NA,
                              Time="time.of.capture",
                              BroodPatch=NA,	
                              CloacalProtub=NA
)


###Phew that was a lot of adding files....
#now lets just quickly put winchord and ninethprimary together since it's actually all one thing

fixWings <- function(wingChord, ninethPrim){
  if(!is.na(wingChord)| !is.na(ninethPrim)){
    if(!is.na(wingChord) & !is.na(ninethPrim)){
      #if both the wing chord and nineth primary slots are filled in, we need to
      #check that they are equal and then we can report only one of them
      assertthat::are_equal(wingChord, ninethPrim)
      return(wingChord)
    } else {
      #if we have only wing chord, report the wing chord
      if(!is.na(wingChord)){
        return(wingChord)
      } else {
        #otherwise report the ninethprimary
        return(ninethPrim)
      }
    }
  } else {
    return(NA)
  }
}
masterfile<- cbind(masterfile, rep(NA, nrow(masterfile)))
colnames(masterfile)[22] <- "Nineth.Primaries"
for (i in 1:nrow(masterfile)){
  masterfile[i, "Nineth.Primaries"]<-fixWings(wingChord= masterfile[i, "Wing.Chord"], ninethPrim = masterfile[i, "Ninth.Primary"])
}

#And now we can remove the wingchord and ninthprim columns
masterfile <- masterfile[, -c(8,9)]

#Now we can just write that matrix out as a csv file and never do that again....
ouputdir<-"~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data"
outputfilename<-paste(ouputdir, "1975-2016 Bands.csv", sep="/")
write.csv(x=masterfile, file=outputfilename, row.names = FALSE, na="")
#YAY I did it and it looks good

