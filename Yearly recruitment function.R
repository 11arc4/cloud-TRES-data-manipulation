

#' buildYearsNestlingsHash
#' This hash table is a table of all the nestlings for a given year. NAs are not
#' included in this table, so if a value exists within this table it really was
#' a nestling from this year
#'
#' @param nestdata The nestdata file for this year
#'
#' @return Returns the hash table of all the nestlings for the given year
#' @export
#'
#' @examples
buildYearsNestlingsHash<-function(nestdata){
  #First we need to create a hash of the years nestlings, where NA is not in the hash
  hash_yearnestlings<- new.env(hash = TRUE, parent=emptyenv(), size=length(nestdata$FemaleID))
  
  #now we need to populate the hash with all of the nestling IDs for band.1-10
  nestlinglocations<-c("band.1","band.2","band.3","band.4","band.5","band.6","band.7","band.8","band.9","band.10")
 
  for (i in 1:length(nestlinglocations) ){
    if(exists(nestlinglocations[i], nestdata)){
      
      for (j in 1:length(nestdata$BoxID)) { #This is the band._ column
        nestling<-as.character(nestdata[nestlinglocations[i]][j,1])
        if(!is.na(nestling)){
        if(exists(nestling, hash_yearnestlings)){
          assign(nestling, append( values=j, x=get(nestling, hash_yearnestlings)), hash_yearnestlings)
        } else {
          assign(nestling, j, hash_yearnestlings)
        }
        } 
      }
    }  
  }
  return(hash_yearnestlings)
}

#ls(hash_yearnestlings, all.names = TRUE)


#Now we need to create a hash of all the adult birds that ever had a nest (to our knowledge) 

hash_adults<-new.env(hash=TRUE, parent=emptyenv()) 

#' buildAllAdultHash
#'This function fills in a hash table with all of the adult birds from a given
#'year's nest data. It includes both males and females, and does not include NA.
#'Therefore if an entry exists in this hash table it was in fact an adult bird
#'from this year. 
#'The function requires that you tell it what hash you'd like to
#'put the birds into--this will let you create a new hash for each year, or
#'build for all years as you choose.
#'
#' @param nestdata The year in question's nest data file
#' @param hash_adults The hash table you'd like to put the adults into.
#'
#' @return Returns a hash table that includes all adults found in this year. 
#' @export
#'
#' @examples
buildAllAdultHash<-function(nestdata, hash_adults){
adultlocations<-c("FemaleID", "MaleID")

for (location in adultlocations){
  
  for(i in 1:length(nestdata$BoxID)){
    adultband<-as.character(nestdata[location][i, 1])
    if(!is.na(adultband)){
    if(exists(adultband, hash_adults)){
      assign(adultband, append(x=get(adultband, hash_adults), values=i), hash_adults)
    } else {
      assign(adultband, i, hash_adults)
    }
    }
  }
}
return(hash_adults)
}

#ls(hash_adults, all.names = TRUE)

  