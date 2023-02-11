import { build, emptyDir } from "https://deno.land/x/dnt@0.30.0/mod.ts";
import packageInfo from "./package.json" assert { type: "json" };

await emptyDir("./dist");

//** NPM ES Module for Node.js and Browser */

await build({
  entryPoints: ["./mod.ts"],
  outDir: "./dist",
  test: false,
  scriptModule: false,
  typeCheck: false,
  shims: {},
  package: {
    ...packageInfo,
    engines: {
      node: ">=14",
    },
    main: "./esm/mod.js",
    type: "module",
  },
  mappings: {
    "https://deno.land/x/lucid@0.9.2/mod.ts": {
      name: "lucid-cardano",
      version: "^0.9.2",
      peerDependency: true,
    },
  },
});

Deno.copyFileSync("LICENSE", "dist/LICENSE");
Deno.copyFileSync("README.md", "dist/README.md");
