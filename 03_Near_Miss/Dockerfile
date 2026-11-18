FROM rocker/shiny:4.5.3
RUN apt-get update && apt-get install -y --no-install-recommends libpq-dev libsodium-dev && rm -rf /var/lib/apt/lists/*
RUN install2.r --error --skipinstalled shiny DT DBI RPostgres jsonlite sodium
RUN rm -rf /srv/shiny-server/*
COPY . /srv/shiny-server/
RUN chown -R shiny:shiny /srv/shiny-server
EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
