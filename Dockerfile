# Base image
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y     libxml2-dev     libcurl4-openssl-dev     libssl-dev     libfontconfig1-dev     libharfbuzz-dev     libfribidi-dev     libfreetype6-dev     libpng-dev     libtiff5-dev     libjpeg-dev     && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny', 'DT', 'bslib', 'here', 'readxl', 'tidyverse', 'rmarkdown', 'markdown'), repos='https://cran.rstudio.com/')"

# Copy app-specific files and set ownership to shiny user
COPY --chown=shiny:shiny app/ /srv/shiny-server/
COPY --chown=shiny:shiny R/ /srv/shiny-server/R/
COPY --chown=shiny:shiny data/ /srv/shiny-server/data/
COPY --chown=shiny:shiny indicators/ /srv/shiny-server/indicators/
COPY --chown=shiny:shiny style.css /srv/shiny-server/style.css
COPY --chown=shiny:shiny _common.R /srv/shiny-server/_common.R

# Create and set permissions for the cache directory for the shiny user
RUN mkdir -p /srv/shiny-server/app_cache/sass &&     chown -R shiny:shiny /srv/shiny-server/app_cache

# Set working directory
WORKDIR /srv/shiny-server/

# Expose port
EXPOSE 3838
