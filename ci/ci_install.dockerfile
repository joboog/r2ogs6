FROM rocker/r-ver:4.2.3

ARG RENV_CONFIG_EXTERNAL_LIBRARIES="/usr/local/lib/R/library"
ARG RUN_PYTHON_CONFIG_TESTS=true
ENV RUN_PYTHON_CONFIG_TESTS=$RUN_PYTHON_CONFIG_TESTS

# before script
RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y git
# Required for building python and rhdf5
RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y git
RUN apt-get install -y build-essential libssl-dev zlib1g-dev libbz2-dev \
  libreadline-dev libsqlite3-dev curl libncursesw5-dev xz-utils tk-dev \
  libxml2-dev libxmlsec1-dev libffi-dev liblzma-dev \
  libharfbuzz-dev libfribidi-dev libcurl4-openssl-dev \
  libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
RUN apt-get install -y python3.10-venv python3-pip

#RUN git clone --branch 90-add_fun_install_ogs https://gitlab.opengeosys.org/ogs/tools/r2ogs6.git /root/r2ogs6
COPY .. /root/r2ogs6
RUN rm /root/r2ogs6/.Renviron
RUN rm /root/r2ogs6/.Rprofile
RUN mkdir /root/test_r2ogs6
WORKDIR /root/test_r2ogs6

# script
RUN R -e 'install.packages(c("remotes", "BiocManager"))'
RUN R -e 'remotes::install_local(path="/root/r2ogs6", dependencies="Imports", repos=BiocManager::repositories())'
RUN R -e 'reticulate::virtualenv_create(envname="r2ogs6", python="/usr/bin/python3")'
RUN R -e 'r2ogs6::install_ogs(ogs_version = "6.4.4", envname = "r2ogs6")'


