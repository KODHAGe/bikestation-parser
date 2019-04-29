FROM trestletech/plumber
LABEL maintainer="wwikgren"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y git-core \
	make \
	python-minimal \
	libsodium-dev
RUN ["install2.r", "assertthat", "base64enc", "crayon", "generics", "glue", "httpuv", "httr", "jsonlite", "later", "lattice", "lubridate", "magrittr", "Matrix", "pillar", "pkgconfig", "promises", "purrr", "R6", "Rcpp", "remotes", "reticulate", "rlang", "rstudioapi", "stringi", "stringr", "tensorflow", "tfruns", "tibble", "tidyr", "tidyselect", "whisker", "zeallot"]
RUN ["installGithub.r", "hadley/dplyr@792ca4909c1c2f7d5a61c4c7369d5731ef092477", "ropensci/ghql@793ab2dc4892a22f283f854de4109dfef4c9ada7", "rstudio/keras@e58f5d5ac118bd416ab926b81baf4e0781c27a55", "trestletech/plumber@796e8fe9cdad22b76b63f8f7bd48a5605a243817"]
WORKDIR /payload/
COPY ["./", "./"]
CMD ["R"]
EXPOSE 8080
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(commandArgs()[4]); pr$run(host='0.0.0.0', port=8080)"]
CMD ["keras_for_plumber.R"]
