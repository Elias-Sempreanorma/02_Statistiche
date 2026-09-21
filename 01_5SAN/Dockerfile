FROM rocker/shiny:4.5.3

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpq-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r --error --skipinstalled \
    shiny \
    dplyr \
    tidyr \
    ggplot2 \
    ggiraph \
    shinyWidgets \
    RColorBrewer \
    here \
    stringr \
    lubridate \
    scales \
    readxl \
    openxlsx \
    RPostgres \
    DBI \
    purrr \
    DT \
    dbscan

RUN rm -rf /srv/shiny-server/*

WORKDIR /srv/shiny-server

COPY . /srv/shiny-server

RUN mkdir -p /srv/shiny-server/02_Output \
    && chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]