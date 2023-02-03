import { server } from './server/server';

const port = Number(process.env.PORT) || 1234;

server.listen(port, () => {
    console.log("Listening on %d", port)
})