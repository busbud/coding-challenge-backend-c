require('module-alias/register');
require('dotenv').config();

import { createConnection, ConnectionOptions, getConnectionOptions } from 'typeorm';
import { getTypeormConfig } from "config/getTypeormConfig"

const dataInitialize = require('./src/dataInitialize')
const listenAPI = require('./src/listenAPI')

async function main() {
    
    // Creates typeorm connection
    await createConnection(await getTypeormConfig())
    
    // Awaits for database structure initialization
    await dataInitialize(); 

    // Serves the API
    listenAPI();           
        
};

main().catch(console.error);