const { Client } = require('pg');
const DATABASE_URL = process.env.DATABASE_URL || 'postgres://user:pass@localhost:35432/db'

module.exports = async (query) => {
    try {
        // Initalize DB Connection
        const client = new Client({
            connectionString: DATABASE_URL
        });

        await client.connect();

        const res = await client.query(query);
        await client.end()

        return res;
    }
    catch(e) {
        console.error(e);

        return e;
    }
}