import { ConnectionOptions, getConnectionOptions } from 'typeorm';

export async function getTypeormConfig () {
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