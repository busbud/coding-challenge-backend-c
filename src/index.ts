import { app } from './app';

// import DataBase from './data/connection';

const start = async () => {
  // try {
  //   await DataBase.connect();
  //   console.log('Connected to postgres');
  // } catch (err) {
  //   console.error(err);
  // }
  app.listen(2345, () => {
    console.log('Listening on port 2345!!!!!!!!');
  });
};

start();
