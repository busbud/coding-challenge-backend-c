
const express = require('express')
const app = express()
const port = 3000

const seed = require('./scripts/seed');
const cache = require('./lib/cache');
const locationRoutes = require('./routes/location.routes');
const swaggerRoute = require('./lib/swagger');

app.use(locationRoutes)
app.use(swaggerRoute)

// seed the cache


module.exports = async function startServer(port) {
    return new Promise((resolve, reject) => {
        seed((tries) => {
            cache.init(tries);
            app.listen(port, () => {
                console.log(`Example app listening on port ${port}`)
            })
            resolve(app);
        })
    });
}
