const common = require('./webpack.config');

module.exports = { ...common, ... {
    devtool: 'inline-source-map',
    mode: 'development',
}}