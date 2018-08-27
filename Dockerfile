FROM node:carbon-alpine AS app

WORKDIR /code
COPY ./ /code/

RUN apk add --no-cache postgresql-client

CMD npm run start
