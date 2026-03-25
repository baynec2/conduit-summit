FROM rocker/shiny:4.4

# System libraries needed by R packages
RUN apt-get update && apt-get install -y \
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
    libgit2-dev \
    libbz2-dev \
    liblzma-dev \
    && rm -rf /var/lib/apt/lists/*

# Install renv
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')"

# Restore packages from lockfile
WORKDIR /srv/shiny-server/conduit-summit
COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json
RUN R -e "renv::restore()"

# Copy app files
COPY ui.R server.R ./
COPY R/ R/
COPY www/ www/

EXPOSE 3838
