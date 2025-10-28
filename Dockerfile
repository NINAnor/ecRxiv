# Base image med R, Shiny og s6-overlay
FROM rocker/shiny:latest

# --- Root setup-fase ---
USER root

# Consolidated apt install with --no-install-recommends and explicit cleanup
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl libcurl4-openssl-dev libssl-dev libxml2-dev \
    libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
    libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Optional: use install2.r for faster R package install (comes with rocker)
ENV CRAN_REPO=https://cloud.r-project.org
RUN install2.r --error --skipinstalled -r $CRAN_REPO \
    shiny DT bslib dplyr here rmarkdown readxl tidyverse markdown

# Prepare directories
RUN mkdir -p /srv/shiny-server/R /srv/shiny-server/data /srv/shiny-server/indicators \
    && chown -R shiny:shiny /srv/shiny-server

# Copy app in fewer layers
COPY --chown=shiny:shiny app/app.R app/global.R app/*.md /srv/shiny-server/
COPY --chown=shiny:shiny app/www/ /srv/shiny-server/www/
COPY --chown=shiny:shiny R/Create_metadata.R /srv/shiny-server/R/
COPY --chown=shiny:shiny data/ /srv/shiny-server/data/
COPY --chown=shiny:shiny indicators/ /srv/shiny-server/indicators/
COPY --chown=shiny:shiny style.css _common.R controlled_vocab.yaml /srv/shiny-server/

# Healthcheck (basic)
HEALTHCHECK --interval=30s --timeout=5s --start-period=20s --retries=3 \
    CMD curl -f http://localhost:3838 || exit 1

EXPOSE 3838
CMD ["/init"]
