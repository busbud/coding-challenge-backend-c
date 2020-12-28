FROM node:12.19.0-alpine as builder

USER node
WORKDIR /home/node

COPY package*.json ./
RUN npm ci
COPY nest-cli.json .
COPY tsconfig*.json ./
COPY src src
RUN npm run build
# ---

FROM node:12.19.0-alpine

USER node
WORKDIR /home/node

COPY --from=builder /home/node/package*.json /home/node/
COPY --from=builder /home/node/dist/ /home/node/dist/

RUN npm ci --production

CMD ["node", "dist/main"]
EXPOSE 2345
