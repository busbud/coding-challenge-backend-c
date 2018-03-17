FROM node:8.10.0-alpine
MAINTAINER github/gabidi

COPY . /usr/app/src/
WORKDIR /usr/app/src/
RUN npm i
# For local testing only
EXPOSE 2345
CMD ["npm","start"]
