import CopyPlugin from "copy-webpack-plugin";
import CssMinimizerPlugin from "css-minimizer-webpack-plugin";
import FaviconsPlugin from "favicons-webpack-plugin";
import HtmlPlugin from "html-webpack-plugin";
import MiniCssExtractPlugin from "mini-css-extract-plugin";
import path from "path";
import { fileURLToPath } from "url";
import * as lightningcss from "lightningcss";
import browserslist from "browserslist";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

const title = "PureScript Domain";
const description = "Free vanity URLs for PureScript libraries and community resources";

function openGraph(tags) {
  return Object.fromEntries(
    Object
      .entries(tags)
      .map(([ property, content ]) => [ `og:${property}`, { property: `og:${property}`, content } ]),
  );
}

export default ({ pursOutputPath, production, url = "" }) => ({
  mode: production ? "production" : "development",
  entry: [
    path.join(pursOutputPath, "Domain.Homepage.Main", "index.js?main"),
    path.join(pursOutputPath, "Domain.Homepage.CSS", "index.js?css"),
  ],
  output: {
    publicPath: "",
    path: path.join(__dirname, "public"),
    filename: `app${production ? `.[contenthash]` : ""}.js`,
  },
  resolve: {
    alias: Object.fromEntries(
      ["HowItWorks", "Terms", "NotFound"].map(x => [
        path.join(pursOutputPath, `Domain.Homepage.${x}`, "foreign.js"),
        path.resolve(__dirname, "src", `${x}.md`),
      ])
    ),
  },
  plugins: [
    new CopyPlugin({
      patterns: ["./src/banner.png"],
    }),
    new FaviconsPlugin({
      logo: "./src/favicon.svg",
      favicons: {
        appName: title,
        appDescription: description,
        developerName: "PureScript Domain Contributors",
        developerURL: url,
        background: "#1b1f28",
        theme_color: "#bc8a33",
      },
    }),
    new HtmlPlugin({
      inject: false,
      title,
      meta: {
        viewport: "width=device-width, initial-scale=1",
        description,
        ...openGraph({
          title,
          description,
          url,
          image: `${url || "."}/banner.png`,
          site_name: "PureScript Domain",
        }),
      },
      templateContent({ htmlWebpackPlugin: { tags: { headTags, bodyTags }, options: { title } } }) {
        return `
          <html>
            <head>
              <title>${title}</title>
              <link rel="stylesheet" href="//fonts.googleapis.com/icon?family=Material+Icons">
              <link rel="stylesheet" href="//fonts.googleapis.com/css?family=Montserrat|Inconsolata|Roboto">
              <meta charset="utf-8">
              ${headTags}
            </head>
            <body>
              ${bodyTags}
            </body>
          </html>
        `;
      },
    }),
    new MiniCssExtractPlugin({
      filename: `app${production ? ".[contenthash]" : ""}.css`,
    }),
  ],
  module: {
    rules: [
      {
        test: /\.md$/,
        use: {
          loader: "string-replace-loader",
          options: {
            search: "([\\S\\s]*)$",
            flags: "m",
            replace: (_, x) => `export const markdownContent = ${JSON.stringify(x.split("\n"))}.join("\\n");`,
          },
        },
      },
      {
        test: /\.js$/,
        resourceQuery: /main/,
        use: {
          loader: "string-replace-loader",
          options: {
            search: "([\\S\\s]*)$",
            flags: "m",
            replace: (_, x) => x + "\nmain()",
          },
        },
      },
      {
        test: /\.js$/,
        resourceQuery: /css/,
        use: [
          MiniCssExtractPlugin.loader,
          "css-loader",
          {
            loader: "lightningcss-loader",
            options: {
              implementation: lightningcss,
            },
          },
          "execute-module-loader?export=css",
        ],
      },
    ],
  },
  optimization: {
    minimizer: [
      "...",
      new CssMinimizerPlugin({
        minify: CssMinimizerPlugin.lightningCssMinify,
        minimizerOptions: {
          targets: lightningcss.browserslistToTargets(browserslist(">= 0.25%")),
        },
      }),
    ],
  },
  devServer: {
    hot: false,
    client: {
      overlay: false, // disabled due to benign ResizeObserver error triggering it
      webSocketURL: "auto://0.0.0.0:0/ws",
    },
    allowedHosts: "all",
  },
});