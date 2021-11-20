FROM node:10 AS installer
WORKDIR /app
COPY . .
RUN rm -rf node_modules
RUN npm install
RUN npm run build
RUN rm -rf docker-compose.yml license README.md docker src


FROM node:10-alpine
WORKDIR /app
COPY --from=installer /app ./
CMD ["npm", "run", "start:dev"]
