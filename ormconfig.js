module.exports = {
    'type': 'postgres',
    'url': process.env.DB_URL,
    'entities': [process.env.ENTITY_PATH],
    'synchronize': true
}
