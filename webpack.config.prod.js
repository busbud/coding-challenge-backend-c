const common = require('./webpack.config');
const CssMinimizerPlugin = require("css-minimizer-webpack-plugin");

module.exports = { ...common, ... {
    mode: 'production',
    optimization: {
        minimizer: [
          new CssMinimizerPlugin()
        ],
    },
}}