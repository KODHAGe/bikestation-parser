# Requirements
library('parsedate')
library('jsonlite')
library('tidyr')
library('fastDummies')
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

batch <<- 70
bump <<- 631
getData <- function () {
  # First get all filenames from all folders
  text.files  <<- list.files(path="stations/", recursive=T, pattern="*.json", full.names=T)
  dataset.combined <<- list()
  
  print('Reading data')
  maximum = bump + batch # length(text.files)
  total <<- 1400 #length(text.files)
  readProgress <- txtProgressBar(min = 0, max = maximum, style = 3)
  dataset.combined <<- data.frame('name', 'id', 'date', 'month', 'weekday', 'hour', 'minute', 'bikesAvailable', 'stationSize', stringsAsFactors=FALSE)
  # Loop through files, importing the content of each
  for (i in bump:maximum) {
    Sys.sleep(0.1)
    dataset <<- jsonlite::fromJSON(text.files[i], flatten = TRUE)
    rentalstations <<- dataset$stations$bikeRentalStations
    dataset.df <<- data.frame('name', 'id', 'date', 'month', 'weekday', 'hour', 'minute', 'bikesAvailable', 'stationSize', stringsAsFactors=FALSE)
    lapply(rentalstations, function(station) {
      if(!is.na(dataset$timestamp) && !is.na(station['bikesAvailable'][[1]]['value'])) {
        datapoint.name <<- station['name']
        datapoint.bikesAvailable <<- station['bikesAvailable'][[1]]['value']
        datapoint.stationSize <<- strtoi(station['bikesAvailable'][[1]]['value']) + strtoi(station['spacesAvailable'][[1]]['value'])
        datapoint.name <<- station['name'][[1]]['value']
        datapoint.id <<- station['stationId'][[1]]['value']
        datapoint.date <- parsedate::parse_iso_8601(dataset$timestamp)
        datapoint.weekday <- strftime(datapoint.date,'%A')
        datapoint.hour <- strftime(datapoint.date,'%H')
        datapoint.month <- strftime(datapoint.date,'%m')
        datapoint.minute <- strftime(datapoint.date, '%M')
        datapoint <<- list(unlist(datapoint.name), unlist(datapoint.id), datapoint.date[2], datapoint.month[2], datapoint.weekday[2], datapoint.hour[2], datapoint.minute[2], unlist(datapoint.bikesAvailable), unlist(datapoint.stationSize))
        dataset.df[nrow(dataset.df) + 1,] <<- datapoint
      }
    })
    dataset.df <<- dataset.df[-1,]
    dataset.combined <<- rbind(dataset.combined, dataset.df)
    setTxtProgressBar(readProgress, i)
  }
  print('Make dummies')
  dataset.combined <<- dataset.combined[-1,]
  dataset.with_dummies <<- fastDummies::dummy_cols(dataset.combined, select_columns = c("X.weekday.", "X.name."), remove_first_dummy = TRUE)
  bump <<- bump + batch
  streamData(bump)
  while (bump < total - batch) {
    getData()
  }
}

writeData <- function() {
  # Serialize & write json to file
  print('Writing data to file')
  dataset.json <- jsonlite::toJSON(dataset.with_dummies)
  jsonlite::write_json(dataset.json, "export/export.json")
  print('Done!')
}

streamData <- function(label) {
  output <- file(paste(c("export/stream-", label, ".json"), collapse = ""))
  jsonlite::stream_out(dataset.with_dummies, con = output)
}

run <- function() { 
  getData()
  #streamData()
}
