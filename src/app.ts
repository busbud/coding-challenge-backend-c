import { Server } from 'api/server';

const init =
  new Server()
  .start()
  .then((port: number) => console.log(`Server listening on port ${port}`))
  .catch((error) => console.error(error));

export default init;


