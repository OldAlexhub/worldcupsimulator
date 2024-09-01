# Use the latest Rocker Shiny image as the base image
FROM rocker/shiny:latest

# Install system dependencies required for some R packages, including mongolite
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libsodium-dev \
    libmongoc-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install remotes to ensure dependency resolution
RUN R -e "install.packages('remotes', repos='http://cran.rstudio.com/')"

# Install necessary R packages including mongolite
RUN R -e "remotes::install_cran(c('shiny', 'dplyr', 'DT', 'shinycssloaders', 'mongolite', 'dotenv'))"

# Copy the Shiny app files into the Docker image
COPY shinysoccer.R /srv/shiny-server/

# Expose the Shiny app port
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/shinysoccer.R')"]


