// Progressive enhancement: syntax highlighting via shiki from CDN.
// If offline, the import fails and the report stays readable, unhighlighted.
import { codeToHtml } from "https://esm.sh/shiki@3";

const opts = {
  lang: "r",
  themes: { light: "github-light", dark: "github-dark" },
  structure: "inline",
};

// Highlight each file's source as one block so multi-line constructs
// tokenize correctly, then distribute lines back to their elements.
for (const src of document.querySelectorAll(".src")) {
  const els = [...src.querySelectorAll(".txt")];
  const code = els
    .map((el) => el.textContent.replace(/\u00a0/g, ""))
    .join("\n");
  const lines = (await codeToHtml(code, opts)).split("<br>");
  if (lines.length === els.length) {
    els.forEach((el, i) => {
      el.innerHTML = lines[i].trim() === "" ? "&nbsp;" : lines[i];
    });
  }
}

for (const el of document.querySelectorAll(".mut .del, .mut .add")) {
  el.innerHTML = await codeToHtml(el.textContent, opts);
}
