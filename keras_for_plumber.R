library(plumber)
library(ghql)
library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
library(lubridate)
library(keras)
library(fastDummies)

library(googleCloudStorageR)
Sys.getenv("GCS_AUTH_FILE")
gcs_global_bucket(Sys.getenv("GCS_GLOBAL_BUCKET"))

#* Run learning
#* @get /run
run <- function () {
  # Load data from HSL
  cli <- GraphqlClient$new(
    url = "https://api.digitransit.fi/routing/v1/routers/hsl/index/graphql"
  )
  
  query <- Query$new()
  query$query('allstations', '{
              bikeRentalStations {
              stationId
              name
              bikesAvailable
              spacesAvailable
              lat
              lon
              state
              }
  }')

  json <- cli$exec(query$queries$allstations)
  json_data <- jsonlite::fromJSON(json)
  stations <- json_data$data$bikeRentalStations
  
  # Process data, add time
  load_time <- Sys.time()
  full <- stations %>%
    mutate(
      stationSize = stations$bikesAvailable + stations$spacesAvailable,
      state = recode(stations$state, "Station on" = 1, "Station off" = 0),
      hour = hour(load_time),
      minute = minute(load_time),
      weekday = wday(load_time, week_start = 1),
      month = month(load_time)
    )
  
  # Split into x & y
  x <- as_tibble(full)
  x <- x %>% select(stationId, stationSize, state, hour, minute, weekday, month)
  x <- fastDummies::dummy_cols(x, select_columns = c("stationId"), remove_first_dummy = TRUE)
  column_names <<- colnames(x)
  x$stationId <- NULL
  
  y <- as_tibble(full)
  y <- y %>% select(bikesAvailable)
  
  x_train <- data.matrix(x)
  y_train <- data.matrix(y)
  
  # Checkpoint functions for saving data (currently just overrides)
  checkpoint_dir <- "checkpoints"
  dir.create(checkpoint_dir, showWarnings = FALSE)
  filepath <- file.path(checkpoint_dir, "model.hdf5")
  
  # Create checkpoint callback
  cp_callback <- callback_model_checkpoint(
    filepath = filepath,
    save_weights_only = FALSE,
    verbose = 1
  )
  
  # Load model if it exists, else create a new one
  gcs_get_object("checkpoints/model.hdf5", saveToDisk = "checkpoints/model.hdf5", overwrite = TRUE)
  
  if(file.exists("checkpoints/model.hdf5")) {
    model <- load_model_hdf5("checkpoints/model.hdf5")
  } else {
    model <- keras_model_sequential()
    model %>%
      layer_dense(units = 48, activation = 'relu', input_shape = c(350)) %>%
      layer_dropout(rate = 0.4) %>%
      layer_dense(units = 24, activation = 'relu') %>%
      layer_dropout(rate = 0.3) %>%
      layer_dense(units = 1, activation = 'linear')
    
    summary(model)
    
    model %>% compile(
      loss = "mae",
      optimizer = optimizer_adamax(),
      metrics = c('accuracy')
    )
  }
  
  # Train
  history <- model %>% fit(
    x_train, y_train, 
    epochs = 30, batch_size = 12,
    validation_split = 0.2,
    callbacks = list(cp_callback)
  )
  
  # Evaluate & predict
  model %>% evaluate(x_train, y_train)
  
  test_data <<- x[1,]
  test_data <<- data.matrix(test_data)
  test_predictions <- model %>% predict(test_data) 
  print(test_predictions)
  
  gcs_upload("checkpoints/model.hdf5")
  list(status = "SUCCESS", code = "200",output = list(run = TRUE))
}

#* Run prediction
#* @get /predict
predict <- function() {
  td <- as.data.frame(test_data)
  
  # Itämerentori station
  td$stationId_030 = 1
  td$minute = td$minute + 30
  if(td$minute > 60) {
    print('xd')
    td$minute = td$minute - 60
    td$hour = td$hour + 1
  }
  td <- data.matrix(td)
  prediction <- model %>% predict(td)
}
