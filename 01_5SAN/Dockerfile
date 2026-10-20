FROM rocker/shiny:4.5.3

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpq-dev \
    zlib1g-dev \
    gfortran \
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
    dbscan \
    robustbase \
    gridExtra \
    blastula \
    jsonlite \
    otp 

# Stima robusta NB dell'autore: versione fissata, solo in fase di build.
RUN Rscript -e 'p <- tempfile(fileext=".tar.gz"); download.file("https://raw.githubusercontent.com/williamaeberhard/robnb/7e44f95f2bffdcdb862359e660778da61a79d9da/robNB_0.2.2.tar.gz", p, mode="wb"); stopifnot(unname(tools::md5sum(p)) == "b37b3308cc5e3ecdafe2e05a810fb970"); install.packages(p, repos=NULL, type="source"); stopifnot(requireNamespace("robNB", quietly=TRUE)); unlink(p)'

RUN rm -rf /srv/shiny-server/*

WORKDIR /srv/shiny-server

COPY . /srv/shiny-server

RUN mkdir -p /srv/shiny-server/02_Output \
    && chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
