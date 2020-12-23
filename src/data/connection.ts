// import alasql from 'alasql';
// import { createConnection } from 'typeorm';

// class DataBase {
//   static async connect(): Promise<void> {
//     try {
//       // createConnection({
//       //   type: 'postgres',
//       //   host: '/var/run/postgresql',
//       //   port: 5432,
//       //   username: 'mo',
//       //   password: '',
//       //   database: 'postgres',
//       //   logging: false,
//       //   entities: [__dirname + '/../**/*.entity.ts'],
//       //   synchronize: true,
//       // });
//     } catch (error) {
//       console.log(error);
//     }
//   }
// }

// export default DataBase;

// export const DBConnect = async () => {
//   let connection: Connection | undefined;
//   try {
//     connection = getConnection();
//   } catch (e) {}

//   try {
//     if (connection) {
//       if (!connection.isConnected) {
//         await connection.connect();
//       }
//     } else {
//       await createConnection({
//         type: 'postgres',
//         host: '/var/run/postgresql',
//         port: 5432,
//         username: 'mo',
//         password: '',
//         database: 'postgres',
//         logging: false,
//         // entities: [__dirname + '/../**/*.entity.ts'],
//         // migrations: [__dirname + '/../**/*.ts'],
//         // subscribers: ['src/subscriber/**/*.ts'],
//         entities: ['src/entities/**/*.ts'],
//         migrations: ['src/migration/**/*.ts'],
//         subscribers: ['src/subscriber/**/*.ts'],
//         // entities: ['src/entities/**/*.ts'],
//         synchronize: true,
//         cli: {
//           entitiesDir: 'src/entities',
//           migrationsDir: 'src/migration',
//         },
//       });
//     }
//     console.log('🌴 Database connection was successful!');
//   } catch (e) {
//     console.error('ERROR: Database connection failed!!', e);
//     throw e;
//   }
// };

// export const TryDBConnect = async (onError: Function, next?: Function) => {
//   try {
//     await DBConnect();
//     if (next) {
//       next();
//     }
//   } catch (e) {
//     onError();
//   }
// };

// CREATE TABLE LOCATIONS(
//   id   integer NOT NULL,
//   name varchar(100),
//   ascii varchar(140),
//   alt_name varchar,
//   lat decimal,
//   long decimal,
//   feat_class varchar(140),
//   feat_code varchar(100),
//   country varchar(140),
//   cc2 varchar(140) ,
//   admin1  varchar(140),
//   admin2  varchar(140),
//   admin3  varchar(140),
//   admin4  varchar(140),
//   population  integer,
//   elevation   varchar(140),
//   dem         varchar(140),
//   tz    varchar(140),
//   modified_at   varchar(140),

// CONSTRAINT locations_pkey PRIMARY KEY (id));

// CREATE INDEX idx_id
// ON locations (id);

// COPY locations
// FROM '/home/mo/sidebar/coding-challenge-backend-c/data/cities_canada-usa.tsv'
// DELIMITER E'\t'
// QUOTE E'\b'
// CSV HEADER;
