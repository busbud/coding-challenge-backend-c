import { createServer } from './server'

// supertest does its own listening, so we separate the logic to keep things neat
/*
Supertest Docs: 
You may pass an http.Server, or a Function to request() - if the server is not already listening for connections then it is bound to an ephemeral port for you so there is no need to keep track of ports.
 */
createServer(
    {
        windowMs: 30 * 1000, // 30s
        max: 20, // limits IP to 100 requests per 15 min window
        message:
            'Too many requests from this IP. Please wait and try again later.',
        statusCode: 429,
    },
    true
).listen(4000, () => console.log(`Listening on port ${4000}`))
