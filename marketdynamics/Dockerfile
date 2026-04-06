FROM rocker/shiny:4.3.3

# System libraries required by R packages
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
    libcairo2-dev \
    && rm -rf /var/lib/apt/lists/*

# Set RSPM as the default repo globally (pre-compiled binaries for Ubuntu 22.04)
RUN echo 'options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"))' \
    >> /usr/local/lib/R/etc/Rprofile.site

# Install CRAN packages (binary via RSPM — no source compilation)
RUN R -e "install.packages(c( \
    'shiny', 'golem', 'bslib', 'bsicons', 'plotly', \
    'dplyr', 'tidyr', 'lubridate', 'zoo', \
    'config', 'htmltools', 'arrow', 'remotes' \
    ), Ncpus=2)"

# Copy package source (includes inst/extdata/*.feather) and install
COPY . /app
RUN R -e "remotes::install_local('/app', dependencies=FALSE)"

EXPOSE 3838

CMD ["Rscript", "-e", "marketdynamics::run_app(options=list(host='0.0.0.0', port=3838))"]
