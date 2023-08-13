import { createServer } from './server'
import {MAX_TIME_PER_WINDOW, MAX_SERVER_REQUESTS_PER_WINDOW} from '../constants'

createServer(
    { // limit IP to max # of requests per time window
        windowMs: MAX_TIME_PER_WINDOW, 
        max: MAX_SERVER_REQUESTS_PER_WINDOW, 
        message:
            'Too many requests from this IP. Please wait and try again later.',
        statusCode: 429,
    },
    true
).listen(4000, () => console.log(`Listening on port ${4000}`))
