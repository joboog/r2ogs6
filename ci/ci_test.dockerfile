FROM rocker/r-ver:4.2.3


ARG RUN_PYTHON_CONFIG_TESTS=true
ENV RUN_PYTHON_CONFIG_TESTS=$RUN_PYTHON_CONFIG_TESTS

# Set RENV_CONFIG_EXTERNAL_LIBRARIES to consider to R system lib
ARG RENV_CONFIG_EXTERNAL_LIBRARIES="/usr/local/lib/R/library"
# Define renv lib 
ARG RENV_PATHS_LIBRARY=/root/.cache/R/renv/library
ENV RENV_PATHS_LIBRARY=$RENV_PATHS_LIBRARY
ARG RENV_PATHS_CACHE=/root/.cache/R/renv/cache
ENV RENV_PATHS_CACHE=$RENV_PATHS_CACHE

# before script
RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y git
RUN apt-get install -y build-essential libssl-dev zlib1g-dev libbz2-dev \
  libreadline-dev libsqlite3-dev curl libncursesw5-dev xz-utils tk-dev \
  libxml2-dev libxmlsec1-dev libffi-dev liblzma-dev \
  libharfbuzz-dev libfribidi-dev libcurl4-openssl-dev \
  libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
RUN apt-get install -y python3.10-venv python3-pip

#RUN rm .Rprofile
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"
COPY .. /root/r2ogs6
RUN rm /root/r2ogs6/.Renviron
WORKDIR /root/r2ogs6
RUN ls -ahl .
RUN echo $R_LIBS
RUN R -e "renv::restore()"

ENV R_LIBS=/usr/local/lib/R/site-library:/usr/local/lib/R/library:$RENV_PATHS_LIBRARY

# # script
RUN R -e "devtools::test()"
RUN R -e "devtools::check(vignettes=F)"
# using of devtools as R CMD could not access renv library, dunno why
# RUN R CMD build . --no-manual --no-build-vignettes
# RUN R CMD check $(ls -1t *.tar.gz | head -n 1) --no-manual --no-build-vignettes

# # after script
RUN R -e 'renv::install("covr")'
RUN R -e 'covr::package_coverage(type = c("tests", "examples"), quiet = F)'