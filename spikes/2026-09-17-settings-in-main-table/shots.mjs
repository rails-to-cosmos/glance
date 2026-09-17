import { chromium, KEY } from "../2026-09-13-frozen-columns/cdp.mjs";
import { fileURLToPath, pathToFileURL } from "node:url";
import { dirname, join } from "node:path";

const here = dirname(fileURLToPath(import.meta.url));
const page = await chromium(process.env.CHROMIUM);
let failed = 0;
const check = (claim, value) => {
  console.log(`${value ? "ok  " : "FAIL"}  ${claim}`);
  if (!value) failed += 1;
};
const open = async (variant) => {
  const url = new URL(pathToFileURL(join(here, "index.html")));
  url.searchParams.set("variant", variant);
  await page.goto(url.href);
  await page.settle();
};

await page.resize(1280, 760);
await open("flat");
check("flat settings is one table-view mount",
  await page.eval(() => document.querySelectorAll("#mount .tv-root").length === 1));
check("flat settings exposes Key, Source and Value",
  await page.eval(() => [...document.querySelectorAll("#mount th")].map((x) => x.textContent.trim()).join("|").includes("Key|Source|Value")));
check("there is no dialog or settings popup",
  await page.eval(() => !document.querySelector("dialog, [role=dialog], .sheet, .popup")));
await page.shot(join(here, "a-flat.png"));

await open("category");
await page.keys([KEY.Enter]);
check("category drill extends the shell breadcrumb",
  await page.eval(() => document.querySelector("#crumbs").textContent.includes("Interface")));
await page.keys([KEY.Enter]);
check("a Value cell opens table-view's native editor",
  await page.eval(() => !!document.querySelector("#mount tbody input")));
await page.keys([KEY.Escape]);
await page.settle();
await page.eval(() => { document.activeElement.blur(); scrollTo(0, 0); });
await page.shot(join(here, "b-category.png"));

await open("source");
check("source variant begins with ownership routes",
  await page.eval(() => document.querySelector("#mount tbody").textContent.includes("Browser")
    && document.querySelector("#mount tbody").textContent.includes("system.org")));
await page.shot(join(here, "c-source.png"));

console.log(failed ? `\n${failed} failed` : "\nall spike checks passed");
await page.close();
process.exit(failed ? 1 : 0);
