
# Start from an R base image
FROM ghcr.io/rocker-org/r-ver:4.4.1

# Arguments
ARG GITHUB_PAT
ARG OXINFER_GMAIL

# Set as environment variables
ENV GITHUB_PAT=$GITHUB_PAT
ENV OXINFER_GMAIL=$OXINFER_GMAIL

# First update package lists with retry logic
RUN apt-get update || apt-get update

# Install system dependencies with retries and skip problematic packages
RUN apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libx11-dev \
    pandoc \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Set default CRAN mirror
RUN echo 'options(repos = c(CRAN = "https://cloud.r-project.org"))' >> /usr/local/lib/R/etc/Rprofile.site

# Install renv and blastula directly first (to handle dependencies)
RUN R -e "install.packages('renv')"

# Create and set working directory
WORKDIR /app

# Copy only necessary files for package restoration
COPY renv.lock .
COPY renv/activate.R renv/

# Restore packages with renv
RUN R -e "renv::restore()"

# Copy the rest of needed files
COPY sendNewsletter.R .
COPY scrapePackages.R .
COPY packages.csv .

# Command to run your script
CMD ["Rscript", "sendNewsletter.R"]