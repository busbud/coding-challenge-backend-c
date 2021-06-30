FROM node:14.17.1-alpine3.11

ENV NODE_ENV production

WORKDIR /usr/src/app

RUN apk add --no-cache tini

COPY --chown=node:node package*.json ./
RUN npm ci --only=production

COPY --chown=node:node . .
COPY --chown=node:node .env.docker .env

USER node

EXPOSE 2345
ENTRYPOINT ["/sbin/tini", "--" , "npm", "start"]
