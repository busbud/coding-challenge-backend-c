const path = require('path')
const SRC_DIR = path.resolve(__dirname, './src/ui/');

module.exports = {
    DIST_DIR: path.resolve(__dirname, './dist/ui'),
    SRC_DIR,
    RESOURCES_DIR: path.resolve(__dirname, './src/ui/resources')
}