# Requirements
library('parsedate')
library('jsonlite')
Sys.setlocale(locale = 'UTF-8') # Could not get jsonlite to output UTF-8 without this

# Helpers
printDivider <- function() {
  print('============================================================')
}

clearEnv <- function() {
  remove(list = ls(envir=.GlobalEnv), envir=.GlobalEnv)
  printDivider <<- function() {
    print('============================================================')
  }
  
  clearEnv <<- function() {
    remove(list = ls(envir=.GlobalEnv), envir=.GlobalEnv)
  }
}

# Read stuff exported from firestore, stored in stations/
# Files are structured as they exist in firestore: documents in multiple document folders stored in a main collection folder

getData <- function () {
  # First get all filenames from all folders
  text.files  <<- list.files(path="stations/", recursive=T, pattern="*.json", full.names=T)
  dataset.combined <<- list()
  
  # Loop through files, importing the content of each
  for (i in 1:2) {
    dataset <<- jsonlite::fromJSON(text.files[i])
    dataset.combined[[i]] <<- dataset
  }
}

parseData <- function () {
  # Create empty dataframe to receive values
  dataset.df <<- data.frame('name', 'id', 'date', 'month', 'weekday', 'hour', 'minute', 'bikesAvailable', 'stationSize', stringsAsFactors=FALSE)
  
  # Loop through the combined dataset to break down & flatten entries
  for(i in dataset.combined) {
    
    # Deconstruct timestamp data
    if(!is.na(i$timestamp)) {
      datapoint.date <- parsedate::parse_iso_8601(i$timestamp)
      datapoint.weekday <- strftime(datapoint.date,'%A')
      datapoint.hour <- strftime(datapoint.date,'%H')
      datapoint.month <- strftime(datapoint.date,'%m')
      datapoint.minute <- strftime(datapoint.date, '%M')
    }
    
    # Deconstruct station data
    for(n in i$stations$bikeRentalStations) {
      if(!is.na(n['bikesAvailable'])) {
        
        # Available attributes: allowDropoff, bikesAvailable, lat, lon, name, networks, realtime, spacesAvailable, state, stationId
        datapoint.bikesAvailable <- n['bikesAvailable'][[1]]['value']
        datapoint.stationSize <- strtoi(n['bikesAvailable'][[1]]['value']) + strtoi(n['spacesAvailable'][[1]]['value'])
        datapoint.name <- n['name'][[1]]['value']
        datapoint.id <- n['stationId'][[1]]['value']
        
        # Create a list of values, make sure this fits the dataframe
        datapoint <- list(unlist(datapoint.name), unlist(datapoint.id), datapoint.date[2], datapoint.month[2], datapoint.weekday[2], datapoint.hour[2], datapoint.minute[2], unlist(datapoint.bikesAvailable), unlist(datapoint.stationSize))
        # And push that list into place
        dataset.df[nrow(dataset.df) + 1,] <<- datapoint
      }
    }
  }
}

writeData <- function() {
  # Serialize & write json to file
  dataset.json <- jsonlite::toJSON(dataset.df)
  jsonlite::write_json(dataset.json, "export.json")
}

run <- function() {
  getData()
  parseData()
  writeData()
}
