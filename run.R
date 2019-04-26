library(plumber)

r <- plumb('keras_for_plumber.R')
r$run(port=9009)
