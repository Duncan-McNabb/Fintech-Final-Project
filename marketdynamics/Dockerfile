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
    && rm -rf /var/lib/apt/lists/*

# Install remotes first (needed for GitHub packages)
RUN R -e "install.packages('remotes', repos='https://cran.rstudio.com/')"

# Install CRAN packages — separate layer so it caches independently of source code changes
RUN R -e "install.packages(c( \
    'shiny', 'golem', 'bslib', 'bsicons', 'plotly', \
    'dplyr', 'tidyr', 'lubridate', 'stringr', 'zoo', \
    'tidyquant', 'config', 'htmltools' \
    ), repos='https://cran.rstudio.com/', Ncpus=2)"

# Install RTL from GitHub — separate layer so CRAN packages stay cached
RUN R -e "remotes::install_github('patzlaw/RTL', dependencies=NA)"

# Copy package source and install it
COPY . /app
RUN R -e "remotes::install_local('/app', dependencies=FALSE)"

EXPOSE 3838

CMD ["Rscript", "-e", "marketdynamics::run_app(host='0.0.0.0', port=3838)"]
