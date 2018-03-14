
module.exports = {
  devtool: "source-map",
  entry: ["./dist/index.js"],
  output: {
    path: __dirname + "/dist",
    filename: "index_bundle.js",
    sourceMapFilename: "index_bundle.js.map"
  },
  module: {
    loaders: [
      {
        test: /\.js$/,
        exclude: /node_modules\//,
        loader: "babel-loader"
      },
    ]
  }
};
