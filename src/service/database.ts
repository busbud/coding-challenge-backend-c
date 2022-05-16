import knex from 'knex';
import databaseConfig from 'config/database';

export default knex(databaseConfig);
