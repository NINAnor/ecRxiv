# Base image with pinned version 4.5.2
FROM rocker/shiny:4.5.2

# Metadata labels
LABEL maintainer="ecRxiv"
LABEL description="Shiny application for ecRxiv indicators"
LABEL version="1.0"
LABEL org.opencontainers.image.source="https://github.com/NINANor/ecRxiv"

# --- Root setup phase ---
USER root

# Install system dependencies with cleanup
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Set CRAN repository
ENV CRAN_REPO=https://cloud.r-project.org

# Install R packages (separate layer for better caching)
RUN install2.r --error --skipinstalled -r $CRAN_REPO \
    shiny \
    DT \
    bslib \
    dplyr \
    here \
    rmarkdown \
    readxl \
    tidyverse \
    markdown \
    janitor \
    yaml

# Install shiny-server config with sanitize_errors off
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Set working directory
WORKDIR /srv/shiny-server

# Prepare directories and permissions
RUN rm -rf /srv/shiny-server/* \
    && mkdir -p /srv/shiny-server/indicators \
    && chown -R shiny:shiny /srv/shiny-server

# Copy application files in a single consolidated layer
COPY --chown=shiny:shiny app.R Create_metadata.R HowToUse.R contact.R contribute.R global.R overview.R indicators/ www/ style.css ./

# Switch to non-root user
USER shiny

# Healthcheck
RUN echo "ok" > /srv/shiny-server/healthz.html
HEALTHCHECK --interval=30s --timeout=5s --start-period=60s --retries=5 \
    CMD curl -fsS http://localhost:3838/healthz.html > /dev/null || exit 1

EXPOSE 3838

CMD ["/init"]
