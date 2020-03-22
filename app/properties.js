module.exports = {
    server: {
        host: '0.0.0.0',
        port: process.env.PORT || 8080
    },
    db: {
        host: process.env.DB_HOST || '0.0.0.0',
        port: process.env.DB_PORT || '5432',
        user: process.env.DB_USER || 'postgres',
        password: process.env.DB_PASSWORD || 'postgres',
        database: process.env.DB_DATABASE || 'postgres'
    },
    app: {
        weight: {
            similarity: process.env.APP_WEIGHT_SIMILARITY || 0.8
        }
    }
}