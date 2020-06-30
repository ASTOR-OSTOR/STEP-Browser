FROM r-base:3.6.3

LABEL maintainer "Nick Grosenbacher <grosenbacher.1@osu.edu>"

# system libraries of general use
RUN apt-get update && apt-get install -y \
sudo \
pandoc \
pandoc-citeproc \
libcurl4-gnutls-dev \
libcairo2-dev \
libxt-dev \
libssl-dev \
libssh2-1-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown', 'shinycssloaders'), repos='https://cloud.r-project.org/')"

# install dependencies of the app
RUN R -e "install.packages(c('dplyr', 'DT', 'readr', 'ggplot2', 'plotly', 'shinyWidgets', 'survival', 'survminer'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('rjson'), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/app
COPY app /root/app
COPY data /root/data

EXPOSE 3838

CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0'); shiny::runApp('/root/app')"
