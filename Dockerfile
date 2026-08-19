# Use an official R runtime as a parent image
FROM rocker/tidyverse:latest
LABEL org.opencontainers.image.source=https://github.com/hoenlab/i-spi

# Switch to Azure mirror for reliable downloads on GitHub Actions runners
RUN sed -i 's|http://archive.ubuntu.com|http://azure.archive.ubuntu.com|g' \
    /etc/apt/sources.list.d/ubuntu.sources 2>/dev/null || \
    sed -i 's|http://archive.ubuntu.com|http://azure.archive.ubuntu.com|g' \
    /etc/apt/sources.list 2>/dev/null || true

# Install any needed packages specified in requirements.txt
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    xtail \
    wget \
    vim
RUN apt-get install libpq-dev -y

# Install Shiny server
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    . /etc/environment

RUN R -e "install.packages(c('plotly', 'shiny', 'shinyjs', 'shinyalert', 'shinydashboard', 'shinyWidgets', 'shinybusy','shinyBS'))"
RUN R -e "install.packages(c('readxl', 'openxlsx', 'RPostgres', 'glue', 'DBI', 'DT', 'pool', 'data.table', 'stringi', 'stringr', 'tidyverse'))"
RUN R -e "install.packages(c('tidyr', 'plyr', 'modelr', 'broom', 'rhandsontable', 'gt', 'gtExtras'))"
RUN R -e "install.packages(c('grid', 'gridExtra', 'gtable', 'httr2', 'auth0', 'janitor', 'bslib'))"
RUN R -e "install.packages(c('bsicons', 'yaml'))"

RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('biolabntua/moach')"

RUN R -e "install.packages(c('scales'))"

RUN R -e "install.packages('Polychrome')"

RUN R -e "install.packages(c('magrittr', 'shinyWidgets', 'future', 'promises'), repos='http://cran.rstudio.com/')"

RUN R -e "remotes::install_github('hardikguptadartmouth/shinyjqui')"

RUN R -e "install.packages(c('progressr'))"

RUN R -e "install.packages(c('tidyr'))"

RUN R -e "install.packages('shinyFeedback')"

RUN R -e "install.packages('later')"

RUN R -e "install.packages(c('httr2', 'jose', 'openssl', 'jsonlite', 'urltools'))"
RUN R -e "install.packages(c('purrr'))"
RUN R -e "install.packages('shiny.destroy')"
RUN R -e "install.packages(c('viridis', 'htmltools'))"
RUN R -e "install.packages(c('ggrepel', 'cowplot'))"
RUN R -e "install.packages('patchwork')"
RUN R -e "install.packages('digest')"
RUN R -e "install.packages('bit64')"
RUN R -e "install.packages('shinycssloaders')"

# stanassay removed (calib refactor): all curve fitting runs in the
# i-spi-compute worker; the app no longer loads stanassay.

RUN rm -rf /srv/shiny-server/*

# Copy the app directory into the image
COPY ./src/ /srv/shiny-server/

WORKDIR /srv/shiny-server
ARG BUILD_STAMP=dev
RUN echo "$BUILD_STAMP" > /srv/shiny-server/BUILD_STAMP

# Set environment and start Shiny server
CMD ["bash", "-c", "env > /srv/shiny-server/.Renviron && /usr/bin/shiny-server"]

