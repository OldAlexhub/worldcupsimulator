# Use the official R image as a base
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libsodium-dev

# Install R packages
RUN R -e "install.packages(c('shiny', 'dplyr', 'DT', 'rsample', 'Metrics', 'rpart', 'randomForest', 'nnet', 'caret', 'shinycssloaders', 'mongolite', 'dotenv'), repos='http://cran.rstudio.com/')"

# Copy the app files into the Docker container
COPY . /srv/shiny-server/

# Set working directory
WORKDIR /srv/shiny-server/

# Expose the Shiny app port
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server')"]
