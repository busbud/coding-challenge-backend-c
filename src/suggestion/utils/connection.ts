import {createConnection, getConnection} from 'typeorm';

const connection = {
    async create() {
        // Only for debugging my tests...
        await createConnection({
            name: 'test',
            type: 'postgres',
            host: 'localhost',
            port: 5432,
            username: 'docker',
            password: 'docker',
            database: 'suggestions',
            entities: [
                '../dist/**/**/*.entity{.js,.ts}',
            ],
            logging: false,
        });
    },

    async close() {
        await getConnection().close();
    },

    async clear() {
        const connectionInstance = getConnection();
        const entities = connectionInstance.entityMetadatas;

        entities.forEach(async (entity) => {
            const repository = connectionInstance.getRepository(entity.name);
            await repository.query(`DELETE
                                    FROM ${entity.tableName}`);
        });
    },
};
export default connection;
