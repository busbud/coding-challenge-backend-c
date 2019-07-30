FROM node:10.16.0-alpine
WORKDIR /usr/src/app
COPY package*.json ./
RUN npm install
COPY . .
RUN npm install nodemon -g
EXPOSE 2345

