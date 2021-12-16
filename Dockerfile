FROM node:16-alpine

WORKDIR /app

COPY . .

RUN npm i -g @nestjs/cli

# Install dependencies
RUN npm install

# The process this container should run
CMD ["npm", "run", "dev"]