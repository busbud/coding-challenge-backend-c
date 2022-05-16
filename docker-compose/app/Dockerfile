FROM node:18.1-slim

# Install Git and system dependencies
RUN apt-get update && \
    apt-get install -y \
        curl \
        git \
        gnupg

# Install Heroku CLI
RUN curl https://cli-assets.heroku.com/install-ubuntu.sh | sh

# Set user
USER node

# Set working directory
WORKDIR /home/node/busbud-challenge

# Override entrypoint to keep container up
ENTRYPOINT tail -f /dev/null
