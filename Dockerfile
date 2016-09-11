FROM node:5.12-onbuild

RUN npm run build

CMD npm start