module.exports = {
    development: {
        db: 'mongodb://localhost/code_challenge'
    },
    production: {
        db: process.env.MONGO_LAB,
    }
};