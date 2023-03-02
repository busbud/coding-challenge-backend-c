FROM mhart/alpine-node:14 AS base
RUN apk --update --no-cache add --virtual build-dependencies make

WORKDIR /code
EXPOSE 80

FROM base AS dependencies
COPY package.json package-lock.json /code/
RUN npm i --pure-lockfile --production --no-progress

FROM dependencies as develop
ENV NODE_ENV=development
RUN npm i --no-progress
COPY . /code
RUN npm run build

FROM base AS release
ENV NODE_ENV production
COPY --from=dependencies /code/node_modules /code/node_modules
COPY --from=develop /code/build /code/build
COPY --from=develop /code/config /code/config
COPY --from=develop /code/package.json /code

CMD ["npm","run","start"]
