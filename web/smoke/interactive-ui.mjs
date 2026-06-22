import assert from "node:assert/strict";
import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { extname, join, normalize, relative, resolve, sep } from "node:path";
import { pathToFileURL } from "node:url";

const playwrightCorePath = process.env.PLAYWRIGHT_CORE_PATH;
if (!playwrightCorePath) {
  throw new Error("PLAYWRIGHT_CORE_PATH must point to nixpkgs#playwright-driver");
}

if (!process.env.PLAYWRIGHT_BROWSERS_PATH) {
  throw new Error(
    "PLAYWRIGHT_BROWSERS_PATH must point to nixpkgs#playwright-driver.browsers"
  );
}

const { chromium } = await import(
  pathToFileURL(join(playwrightCorePath, "index.mjs")).href
);

const distRoot = resolve(import.meta.dirname, "../dist");
const editedInput = `utxo input-a 1000000
utxo input-b 2500000
utxo input-c 4000000
output target-address 6000000
`;

const expectedRows = [
  ["input-a", "1000000"],
  ["input-b", "2500000"],
  ["input-c", "4000000"],
];
const expectedChange = "1500000";

const contentTypes = new Map([
  [".html", "text/html; charset=utf-8"],
  [".js", "text/javascript; charset=utf-8"],
  [".css", "text/css; charset=utf-8"],
  [".wasm", "application/wasm"],
]);

function serveDist() {
  const server = createServer(async (request, response) => {
    try {
      const requestUrl = new URL(request.url ?? "/", "http://127.0.0.1");
      const pathname =
        requestUrl.pathname === "/" ? "/index.html" : requestUrl.pathname;
      const filePath = normalize(join(distRoot, decodeURIComponent(pathname)));
      const fileRelative = relative(distRoot, filePath);

      if (fileRelative.startsWith(`..${sep}`) || fileRelative === "..") {
        response.writeHead(403);
        response.end("Forbidden");
        return;
      }

      const body = await readFile(filePath);
      response.writeHead(200, {
        "content-type":
          contentTypes.get(extname(filePath)) ??
          "application/octet-stream",
      });
      response.end(body);
    } catch (error) {
      response.writeHead(404);
      response.end(error instanceof Error ? error.message : "Not found");
    }
  });

  return new Promise((resolveServer) => {
    server.listen(0, "127.0.0.1", () => {
      const address = server.address();
      assert(address && typeof address === "object");
      resolveServer({ server, url: `http://127.0.0.1:${address.port}/` });
    });
  });
}

const { server, url } = await serveDist();
const browser = await chromium.launch();

try {
  const page = await browser.newPage();
  await page.goto(url);
  await page.getByLabel("Coin selection input").fill(editedInput);
  await page.getByRole("button", { name: "Run coin selection" }).click();

  for (const [inputId, lovelace] of expectedRows) {
    await page
      .getByRole("row", { name: new RegExp(`${inputId}\\s+${lovelace}`) })
      .waitFor();
  }

  await page
    .getByText(new RegExp(`Change\\s+${expectedChange}\\b`, "i"))
    .waitFor();
} finally {
  await browser.close();
  await new Promise((resolveClose, rejectClose) => {
    server.close((error) => (error ? rejectClose(error) : resolveClose()));
  });
}
