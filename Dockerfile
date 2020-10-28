# Light LTS Image only to run tests.
FROM node:lts-buster-slim AS testing

# Set the working directory within the image.
WORKDIR /app

# A wildcard is used to ensure the package.json and package-lock.json
# files are copied (available since npm@5+).
COPY package*.json ./

# To bundle the app's source code inside the Docker image.
COPY . .

# Perform a clean installation without updating the package-lock.json file.
RUN npm ci

# Run the tests and get reports.
RUN npm run cover

# Stable and Secure LTS Image.
FROM node:12.18.4-alpine3.12

# Set the working directory within the image.
WORKDIR /app

# A wildcard is used to ensure the package.json and package-lock.json
# files are copied (available since npm@5+).
COPY package*.json ./

# To bundle the app's source code inside the Docker image.
COPY . .

# Perform a clean installation without updating the package-lock.json file.
RUN npm ci --production

# Change the user to run the app (more secure instead of using root).
USER node

# Expose the port where the app will run.
EXPOSE 2345

# Start the application.
CMD [ "npm", "start" ]
