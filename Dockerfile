# Base image med R, Shiny og s6-overlay
FROM rocker/shiny:latest

# --- Root setup-fase ---
USER root

# Oppdater system og installer nødvendige pakker
RUN apt-get update && apt-get install -y \
    sudo \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Opprett nødvendige kataloger og sikre riktige eierskap
RUN mkdir -p /srv/shiny-server /var/log/shiny-server \
    && chown -R shiny:shiny /srv/shiny-server /var/log/shiny-server \
    && mkdir -p /usr/local/lib/R/etc \
    && touch /usr/local/lib/R/etc/Renviron.site \
    && chmod 0644 /usr/local/lib/R/etc/Renviron.site \
    && chown -R shiny:shiny /usr/local/lib/R/etc

# Installer nødvendige R-pakker
RUN R -e "install.packages(c('shiny', 'tidyverse', 'data.table', 'DT', 'shinyjs', 'dplyr', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# --- Kopier app-spesifikke filer og mapper ---
COPY --chown=shiny:shiny app/ /srv/shiny-server/
COPY --chown=shiny:shiny R/ /srv/shiny-server/R/
COPY --chown=shiny:shiny data/ /srv/shiny-server/data/
COPY --chown=shiny:shiny indicators/ /srv/shiny-server/indicators/
COPY --chown=shiny:shiny style.css /srv/shiny-server/style.css
COPY --chown=shiny:shiny _common.R /srv/shiny-server/_common.R

# --- Shiny-konfig (valgfritt, men anbefales) ---
# COPY --chown=shiny:shiny shiny-server.conf /etc/shiny-server/shiny-server.conf

# --- Ikke-root runtime ---
USER shiny

# Eksponer port for Shiny
EXPOSE 3838

# Start via s6-init (shiny-server startes automatisk)
CMD ["/init"]
