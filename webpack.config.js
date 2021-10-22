const paths = require('./webpack.paths');
const webpack = require('webpack');

const CopyWebpackPlugin = require('copy-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');

module.exports = {
    name: 'busbudcodechallenge',
    entry: { 
        busbudcodechallenge: `${paths.SRC_DIR}\\busbudcodechallenge.tsx`,
    },
    output: {
        path: paths.DIST_DIR,
        filename: '[name].js',
        publicPath: '/'
    },
    resolve: {
        extensions: ['.js', '.jsx', '.ts', '.tsx']
    },
    plugins: [
        new CopyWebpackPlugin({
            patterns: [
                { 
                    from: paths.RESOURCES_DIR, 
                    to: paths.DIST_DIR, 
                    globOptions: { ignore: ['**/scss'] } 
                },
                { 
                    from: paths.RESOURCES_DIR + '/index.html', 
                    to: paths.DIST_DIR 
                }
            ]
        }),
        new MiniCssExtractPlugin(),
        new webpack.DefinePlugin({ "process.env": "{}" })
    ],
    module: {
        rules: [
            {
                test: /\.(js|jsx)$/i, 
                exclude: /node_modules/, 
                use: ['babel-loader'],
            },
            {   
                test: /\.(ts|tsx)$/i, 
                use: ["ts-loader"], 
                exclude: /node_modules/
            },

            {
                test:/\.css$/i,
                use:[
                    MiniCssExtractPlugin.loader,
                    {
                        loader: 'css-loader'
                    }
                ],
            },
            {
                test: /\.s[ac]ss$/i,
                use: [ 
                    MiniCssExtractPlugin.loader,
                    { 
                        loader: 'css-loader', 
                        options: {
                            sourceMap: true,
                        }
                    },
                    { 
                        loader: 'sass-loader',
                        options: {
                            sourceMap: true,
                        }
                    }
                ],
            },
            {
                test: /\.(?:ico|gif|jpg|jpeg|png)$/i, 
                type: 'asset/resource',
                generator: {
                    filename: 'images/[hash][ext][query]'
                }
            },
            {
                test: /\.(woff(2)?|eot|ttf|otf|svg)$/i, 
                type: 'asset/resource',
                generator: {
                    filename: 'fonts/[hash][ext][query]'
                }
            },
        ]
    }
}
