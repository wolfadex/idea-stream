const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
// const CopyPlugin = require('copy-webpack-plugin');

module.exports = ({ prod } = {}) => {
  return {
    mode: prod ? 'production' : 'development',
    node: {
      fs: 'empty',
    },
    entry: path.join(__dirname, 'src', 'index.js'),
    output: {
      path: path.join(__dirname, 'public'),
      filename: 'bundle.js',
    },
    module: {
      rules: [
        {
          exclude: /node_modules|elm-stuff/,
          test: /\.js$/,
          use: {
            loader: 'babel-loader',
            options: {
              presets: ['@babel/preset-env'],
            },
          },
        },
        {
          exclude: /node_modules|elm-stuff/,
          test: /\.elm$/,
          use: {
            loader: 'elm-webpack-loader',
            options: { optimize: !!prod },
          },
        },
      ],
    },
    plugins: [
      new HtmlWebpackPlugin({ template: 'src/index.html' }),
      // new CopyPlugin([
      //   { from: 'src/assets', to: 'assets' },
      //   { from: 'src/manifest.json', to: '' },
      // ]),
    ],
    devServer: {
      port: 8000,
    },
  };
};
