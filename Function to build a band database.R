


#' AddtoBandDataFile
#'
#' @param inputfile The banding file that you would like to add to the master banding database. 
#' @param  band The master banding database being amended. 
#' For each of the following parameters, enter the column names, as characters
#' (i.e. "Box.ID"), of the columns in your input banding file. These values will
#' then be inputed into the master banding database, under that column.
#' @param Year 
#' @param Date 
#' @param BoxID 
#' @param BandID 
#' @param Sex 
#' @param Age 
#' @param Ninth.Primary 
#' @param Wing.Chord 
#' @param Mass 
#' @param Tarsus 
#' @param Blood 
#' @param Plumage 
#' @param Head 
#' @param Back 
#' @param Epaulette
#' @param Tail 
#' @param Forehead 
#' @param Time 
#' @param Bander 
#' @param BroodPatch 
#' @param CloacalProtub 
#'
#' @return Returns the newly updated banding database
#' @export
#'
#' @examples
AddtoBandDataFile<-function(inputfile, 
                            band,
                            Species=NA,
                            Year=NA,
                            Date=NA, 
                            BoxID=NA, 
                            BandID=NA,
                            Bander=NA,
                            Sex=NA, 
                            Age=NA, 
                            Ninth.Primary=NA,	
                            Wing.Chord=NA,	
                            Mass=NA,	
                            Tarsus=NA,
                            Blood=NA,
                            Plumage=NA,
                            Head=NA,
                            Back=NA,
                            Epaulette=NA,
                            Tail=NA,
                            Forehead=NA,
                            Time=NA,
                            BroodPatch=NA,	
                            CloacalProtub=NA){
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
  
  
  InputColnames<-c(Year,
                   Date, 
                   BoxID, 
                   BandID,
                   Bander,
                   Sex, 
                   Age, 
                   Ninth.Primary,	
                   Wing.Chord,	
                   Mass,	
                   Tarsus,
                   Blood,
                   Plumage,
                   Head,
                   Back,
                   Epaulette,
                   Tail,
                   Forehead,
                   Time,
                   BroodPatch,	
                   CloacalProtub)
  
  for(i in 1:length(inputfile[[Date]])){
    if(!is.na(Species)){
    if(!is.na(inputfile[[Species]][i])){
      if(inputfile[[Species]][i] != "TRES"){
        next
      }
    }
    }
    birdrow<-c(rep(NA, length(MasterColnames)))
    birdrow[2]<-as.Date(birdrow[2], format="%Y-%m-%d")
    for(a in 1:length(MasterColnames)){
      if(is.na(InputColnames[a])){
        birdrow[a]<-NA
      } else {
       #message ("column is", InputColnames[a], "row=", i, sep=" ")
        birdrow[a]<-inputfile[i, InputColnames[a]]
      }
    }
    if(length(band)==0){
      band<-birdrow
    }else {
      band<-rbind(band, birdrow)
    }
    
    
  }
  
  return(band)
}
