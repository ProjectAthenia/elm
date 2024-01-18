const path = require('path');
let webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const {CleanWebpackPlugin} = require('clean-webpack-plugin');
const MiniCssExtractPlugin = require("mini-css-extract-plugin");

require('dotenv').config();

const prod = 'production';
const dev = 'development';

// determine build env
const TARGET_ENV = process.env.npm_lifecycle_event === 'build' ? prod : dev;
const isDev = TARGET_ENV === dev;
const isProd = TARGET_ENV === prod;

// entry and output path/filename variables
const entryPath = path.join(__dirname, 'src/index.js');
const outputPath = path.join(__dirname, 'dist/current');
const outputFilename = isProd ? '[name]-[hash].js' : '[name].js';

module.exports = (withDebug) => {
    return {
        entry: './src/index.js',
        output: {
            path: outputPath,
            publicPath: "/",
            filename: `static/js/${outputFilename}`,
        },
        resolve: {
            modules: [path.join(__dirname, "../src"), 'node_modules'],
            extensions: [".elm", ".js"]
        },
        plugins: [
            new HtmlWebpackPlugin({
                template: "./src/index.html",
            }),
            new MiniCssExtractPlugin(),
            new CleanWebpackPlugin(),
            new webpack.EnvironmentPlugin([
                "API_URL",
                "APP_NAME",
                "SOCKET_URL",
                "FOOTER_MESSAGE",
                "STRIPE_PUBLISHABLE_KEY",
                "STORAGE_KEY",
            ]),
        ],
        optimization: {
            // Prevents compilation errors causing the hot loader to lose state
            emitOnErrors: false
        },
        module: {
            rules: [
                {
                    test: /\.elm$/,
                    use: [
                        {loader: "elm-reloader"},
                        {
                            loader: "elm-webpack-loader",
                            options: {
                                // add Elm's debug overlay to output
                                debug: withDebug,
                                optimize: false
                            }
                        }
                    ]
                }, {
                    test: /\.(sa|sc|c)ss$/i,
                    use: ['style-loader', 'css-loader', {
                        loader: "postcss-loader",
                        options: {
                            postcssOptions: {
                                plugins: [
                                    require("autoprefixer"),
                                ],
                            },
                        }
                    }, "sass-loader"],
                }, {
                    test: /\.js$/,
                    exclude: /node_modules/,
                    use: {
                        loader: "babel-loader"
                    }
                },
                {
                    test: /\.(png|svg|jpg|jpeg|gif)$/i,
                    type: 'asset/resource',
                },
            ],
        }
    };
};

