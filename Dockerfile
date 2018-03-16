FROM node:8.10.0-alpine
COPY . /usr/app/src/
WORKDIR /usr/app/src/
RUN npm i
EXPOSE 2345
CMD ["npm","start"]
