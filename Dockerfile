FROM node:erbium-alpine AS base

FROM base AS builder
WORKDIR /usr/src/app
COPY package*.json ./
RUN npm ci --only=production

FROM base AS release
WORKDIR /busbud
COPY --from=builder /usr/src/app/node_modules ./node_modules
COPY --from=builder /usr/src/app/package*.json ./
COPY app ./app
CMD ["npm","start"]