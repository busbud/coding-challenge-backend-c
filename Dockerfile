FROM node:0.10

RUN mkdir /src

RUN npm install nodemon -g

WORKDIR /code
EXPOSE 8080

CMD nodemon index.js
