module.exports = {
    db: {
        host: process.env.DB_HOST || '0.0.0.0',
        port: process.env.DB_PORT || '5432',
        user: process.env.DB_USER || 'postgres',
        password: process.env.DB_PASSWORD || 'postgres',
        database: process.env.DB_DATABASE || 'postgres'
    }
}