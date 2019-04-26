library(containerit)
dockerfile <- dockerfile("keras_for_plumber.R", copy="script_dir", soft=TRUE)
write(dockerfile, file="Dockerfile")