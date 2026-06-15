import { defineConfig } from "vite";
import scalaJSPlugin from "@scala-js/vite-plugin-scalajs";

export default defineConfig({
  plugins: [scalaJSPlugin({
    cwd: ".",
    projectID: "ai4p",  
  })],
  
  base: '/ai4p/',
  publicDir: 'assets',

  server: {
    host: true
  }
});