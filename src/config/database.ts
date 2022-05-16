import nodeConfig, { Env } from 'config/node';
import postgresConfig from 'config/postgres';
import sqliteConfig from 'config/sqlite';

export default nodeConfig.env === Env.test
  ? {
    client: 'sqlite3',
    connection: sqliteConfig,
    useNullAsDefault: true,
  }
  : {
    client: 'pg',
    connection: postgresConfig,
    pool: { min: 2, max: 10 },
  };

export const migration = {
  tableName: 'migration',
  directory: `${nodeConfig.path}/migration`,
  extension: nodeConfig.ext,
  stub: 'db-migration.stub',
};

export const seed = {
  directory: `${nodeConfig.path}/seed`,
  extension: nodeConfig.ext,
  stub: 'db-seed.stub',
};
