ARG CODE_VERSION
FROM node:12 as BASE
MAINTAINER Quy Doan <quydoantran@gmail.com> <https://github.com/robdoan>

RUN mkdir -p /home/node/app/node_modules && chown -R node:node /home/node/app
WORKDIR /home/node/app

USER node

COPY package.json package-lock.json ./

RUN npm install

COPY --chown=node:node . .

EXPOSE 2345

CMD [ "npm", "run", "start:dev" ]

FROM BASE as build
RUN ["npm", "run", "build"]
