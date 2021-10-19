require('module-alias/register');
require('dotenv').config();

import { createConnection, ConnectionOptions, getConnectionOptions } from 'typeorm';

const dataInitialize = require('./src/dataInitialize')
const listenAPI = require('./src/listenAPI')

// Returns typeorm configuration
const getTypeormConfig = async () => {
    let connectionOptions: ConnectionOptions;
    connectionOptions = {
        type: "postgres",
        logging: true,
        synchronize: false,
        ssl: false,
        extra: {
           ssl: {
              rejectUnauthorized: false
           }
        },
        entities: [
           "dist/src/models/entities/*.js" 
        ],
        migrations: [
           "src/migrations/**/*.ts"
        ],
        cli: {
           "migrationsDir": "src/migrations"
        }
    };
    if (process.env.DATABASE_URL) {
      Object.assign(connectionOptions, { url: process.env.DATABASE_URL });
    } else {
      connectionOptions = await getConnectionOptions(); 
    }

    return connectionOptions;
  };

async function main() {
    
    // Creates typeorm connection
    await createConnection(await getTypeormConfig())
    
    // Awaits for database structure initialization
    await dataInitialize(); 

    // Serves the API
    listenAPI();           
        
};

main().catch(console.error);