# Use the latest Rocker Shiny image as the base image
FROM rocker/shiny:latest

# Install system dependencies required for some R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libsodium-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages required for the app
RUN R -e "install.packages(c('shiny', 'dplyr', 'DT', 'shinycssloaders', 'mongolite', 'dotenv'), repos='http://cran.rstudio.com/')"

# Copy the Shiny app files into the Docker image
COPY app.R /srv/shiny-server/
COPY .env /srv/shiny-server/

# Expose the Shiny app port
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server')"]

