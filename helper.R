
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(stringr)
library(reshape2)
library(data.table)
library(leaflet)
library(rmapshaper)
library(rgdal)
library(dplyr)


getPopup <- function(votedata, mapdata, office) {
  popup <-paste0("<strong>Ward:</strong> ", 
                 mapdata$Ward, 
                 "<br><strong>Division:</strong> ",
                 mapdata$Division)
  
  for (candidate in sort(make.names(unique(votedata$Tape_Text[votedata$Office_Prop.Name==office])))) {
    popup<- paste0(popup, "<br><strong>",candidate,":</strong> ", mapdata[[candidate]], "%")
  }
  
  return(popup)
}

# makes a wide table with all of the candidates as columns, with precincts, etc.. and data cleaned up
# also puts in vote totals for each precinct
getOfficeVotes <- function(data,office) {
  # create a table of DA votes by making a tall table wide based on precinct and DA race candidates
  
  officeVotes <-dcast(data[data$Office_Prop.Name == office,], Precinct_Name ~ Tape_Text, value.var="Vote_Count", fun.aggregate=sum)
  # tidy up the names of the columns to remove spaces, commas, etc...
  names(officeVotes)<- make.names(names(officeVotes))

  # create a totals column
  officeVotes$vote_totals <- rowSums(officeVotes[-1])

  #Now split the precinct name into ward/precinct and then create a combined column since we'll need that for the mapping
  officeVotes$Ward <- NULL
  officeVotes$Division <- NULL
  officeVotes[,c("Ward","Division")] <- str_split_fixed(as.character(officeVotes$Precinct_Name),"-",2)
  officeVotes$Precinct_Name <- paste(officeVotes$Ward,officeVotes$Division, sep="")

  return(officeVotes)
}

getOfficePercent <- function(data) {
  #make a parallel table with percentages rather than absolute numbers of votes
  #I bet there is a better wy to do this, without having to have a parallel table, but fuck it.
  # for the second column to fourth to last column, divide by the third to last column, which is vote total and then make a percent
  votePercents <- round(data[,c(2:(ncol(data)-3))]/data[,ncol(data)-2],3) * 100
  votePercents$Precinct_Name <- data$Precinct_Name
  votePercents$Division <- data$Division
  votePercents$Ward <- data$Ward
  
  # reorder teh columns so that the precinct, ward, and division are up front; remove the totals column
  votePercents <- votePercents[,c((ncol(votePercents)-2):ncol(votePercents),1:(ncol(votePercents)-3))]
  return(votePercents)
}

# read the CSV and get rid of the strange last column
votes <- read.csv("PRECINCT_2018517_H22_M9_S34.txt",sep="@")
votes$X <- NULL
# make the votes into integers
votes$Vote_Count <- as.integer(votes$Vote_Count)

#get all of the unque offices that were run for
offices <- sort(unique(votes$Office_Prop.Name[votes$Office_Prop.Name!=""]))


# first turn the shapefile into a usable object
tmp <- tempdir()
unzip("Political_Divisions.zip", exdir = tmp)
precincts <- readOGR(dsn=tmp)
precincts <- rmapshaper::ms_simplify(precincts, keep=.04)


pal <- colorNumeric("YlOrBr",domain=0:100)


#make a temp map with the first office on the list
firstVotes <- getOfficeVotes(votes, offices[1])
firstPercents <- getOfficePercent(firstVotes)
firstPrecincts <- precincts
firstPrecincts@data = firstPrecincts@data %>%
  left_join(firstPercents, by=c("DIVISION_N"="Precinct_Name"), copy=TRUE)



VoteMap <- leaflet(data = precincts) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal, 
              fillOpacity = .65,
              color="#BDBDC3",
              weight = 1, 
              popup = getPopup(votes,firstPrecincts,offices[1])) %>%
  setView(lng = -75.07, lat = 40, zoom=11)


