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
const smokeBasePath = normalizeBasePath(process.env.SMOKE_BASE_PATH ?? "/");

const contentTypes = new Map([
  [".html", "text/html; charset=utf-8"],
  [".js", "text/javascript; charset=utf-8"],
  [".css", "text/css; charset=utf-8"],
  [".wasm", "application/wasm"],
]);

function normalizeBasePath(basePath) {
  if (!basePath || basePath === "/") {
    return "/";
  }

  const withLeadingSlash = basePath.startsWith("/") ? basePath : `/${basePath}`;
  return withLeadingSlash.endsWith("/")
    ? withLeadingSlash
    : `${withLeadingSlash}/`;
}

function toDistPathname(pathname) {
  if (smokeBasePath === "/") {
    return pathname === "/" ? "/index.html" : pathname;
  }

  const basePathWithoutSlash = smokeBasePath.slice(0, -1);
  if (pathname === basePathWithoutSlash || pathname === smokeBasePath) {
    return "/index.html";
  }

  if (!pathname.startsWith(smokeBasePath)) {
    return undefined;
  }

  return `/${pathname.slice(smokeBasePath.length)}`;
}

function serveDist() {
  const server = createServer(async (request, response) => {
    try {
      const requestUrl = new URL(request.url ?? "/", "http://127.0.0.1");
      const pathname = toDistPathname(requestUrl.pathname);

      if (pathname === undefined) {
        response.writeHead(404);
        response.end("Not found");
        return;
      }

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

async function assertSelectedRow(page, inputId) {
  const row = page.locator(`[data-utxo-id="${inputId}"]`);
  await row.waitFor();
  assert.match(await row.getAttribute("class"), /\butxo-row\b/);
  assert.match(await row.getAttribute("class"), /\bselected\b/);
}

async function assertTotal(page, label, lovelace) {
  await page
    .getByText(new RegExp(`${label}\\s+${lovelace}\\b`, "i"))
    .waitFor();
}

const { server, url } = await serveDist();
const browser = await chromium.launch();

try {
  const page = await browser.newPage();
  await page.goto(new URL(smokeBasePath, url).href);
  await page.getByRole("button", { name: "One big UTxO" }).click();
  await page.getByLabel("Lovelace for big-1").fill("9000000");
  await page.getByRole("button", { name: "Run coin selection" }).click();

  await assertSelectedRow(page, "big-1");
  await assertTotal(page, "Selected total", "9000000");
  await assertTotal(page, "Target", "6000000");
  await assertTotal(page, "Change", "3000000");

  await page.getByRole("button", { name: "Near-exact match" }).click();
  await assertSelectedRow(page, "exact-1");
  await assertTotal(page, "Change", "0");
} finally {
  await browser.close();
  await new Promise((resolveClose, rejectClose) => {
    server.close((error) => (error ? rejectClose(error) : resolveClose()));
  });
}
