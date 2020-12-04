FROM node:12
WORKDIR /app
COPY package.json /app
COPY package-lock.json /app
RUN npm install
COPY . /app
RUN npm test
CMD npm run start