const { Client } = require('pg');
const DATABASE_URL = process.env.DATABASE_URL

module.exports = async (query) => {
    try {
        // Initalize DB Connection
        const client = new Client({
            connectionString: DATABASE_URL
        });

        await client.connect();

        const res = await client.query(query);
        await client.end()

        console.log('db.js >>> sucess', res);

        return res;
    }
    catch(e) {
        // TODO: Throw DB error
        console.log('db.js >>> error', e);
        return e;
    }
}