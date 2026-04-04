FROM rocker/shiny:4.3.3

# System libraries required by R packages
# pandoc is required for vignette compilation during GitHub package installs
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
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# Install remotes first (needed for GitHub packages)
RUN R -e "install.packages('remotes', repos='https://packagemanager.posit.co/cran/__linux__/jammy/latest')"

# Install CRAN packages via RSPM binary repo (pre-compiled for Ubuntu 22.04 — no source compilation)
RUN R -e "install.packages(c( \
    'shiny', 'golem', 'bslib', 'bsicons', 'plotly', \
    'dplyr', 'tidyr', 'lubridate', 'stringr', 'zoo', \
    'tidyquant', 'config', 'htmltools', 'arrow' \
    ), repos='https://packagemanager.posit.co/cran/__linux__/jammy/latest', Ncpus=2)"

# Install RTL from GitHub
# - GITHUB_PAT prevents API rate-limit failures on CI
# - build_vignettes=FALSE skips vignette compilation (pandoc version mismatches)
# - upgrade='never' prevents re-installing already-present CRAN packages
ARG GITHUB_PAT
ENV GITHUB_PAT=${GITHUB_PAT}
RUN R -e "remotes::install_github('patzlaw/RTL', dependencies=NA, build_vignettes=FALSE, upgrade='never')"

# Copy package source and install it
COPY . /app
RUN R -e "remotes::install_local('/app', dependencies=FALSE)"

EXPOSE 3838

CMD ["Rscript", "-e", "marketdynamics::run_app(host='0.0.0.0', port=3838)"]
