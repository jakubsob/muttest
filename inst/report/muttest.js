(function () {
  var body = document.body;
  var cur = -1;

  function matches(el, f) {
    return f === "all" || el.classList.contains("has-" + f);
  }

  // openFiles: expand matching files (chip clicks); on first load files stay
  // collapsed, but matching lines are pre-opened inside them.
  function setFilter(f, openFiles) {
    body.dataset.f = f;
    cur = -1;
    document.querySelectorAll(".toolbar .chip").forEach(function (b) {
      b.setAttribute("aria-pressed", String(b.dataset.f === f));
    });
    document.querySelectorAll("details.file:not(.plan)").forEach(function (d) {
      if (f !== "all") d.open = matches(d, f) && (openFiles || d.open);
    });
    document.querySelectorAll("details.ln").forEach(function (d) {
      if (f !== "all") d.open = matches(d, f);
    });
    var files = document.querySelectorAll("details.file:not(.plan)");
    var shown = 0;
    files.forEach(function (d) {
      if (matches(d, f)) shown++;
    });
    var fc = document.getElementById("fcount");
    if (fc) {
      fc.textContent =
        f === "all"
          ? files.length + (files.length === 1 ? " file" : " files")
          : "showing " + shown + " of " + files.length + " files";
    }
  }

  document.querySelectorAll(".toolbar .chip").forEach(function (b) {
    b.addEventListener("click", function () {
      setFilter(b.dataset.f, true);
    });
  });

  document.querySelectorAll(".tools [data-open]").forEach(function (b) {
    b.addEventListener("click", function () {
      var open = b.dataset.open === "1";
      var f = body.dataset.f;
      document
        .querySelectorAll("details.file, details.ln")
        .forEach(function (d) {
          d.open = open && matches(d, f);
        });
    });
  });

  function jump(delta) {
    var f = body.dataset.f;
    var sel = f === "all" ? "details.ln.hit" : "details.ln.has-" + f;
    var t = document.querySelectorAll(sel);
    if (!t.length) return;
    cur = (cur + delta + t.length) % t.length;
    var d = t[cur];
    d.closest("details.file").open = true;
    d.open = true;
    var s = d.querySelector("summary");
    if (s) s.focus({ preventScroll: true });
    d.scrollIntoView({ block: "center" });
    d.classList.remove("flash");
    void d.offsetWidth;
    d.classList.add("flash");
  }

  var themes = ["auto", "light", "dark"];
  var themeIcon = { auto: "◐", light: "☀", dark: "☾" };
  var themeBtn = document.getElementById("theme");
  function theme() {
    try {
      return localStorage.getItem("muttest-theme") || "auto";
    } catch (e) {
      return "auto";
    }
  }
  function applyTheme(t) {
    if (t === "auto") delete document.documentElement.dataset.theme;
    else document.documentElement.dataset.theme = t;
    themeBtn.textContent = themeIcon[t];
    themeBtn.dataset.tip = "Theme: " + t;
    try {
      localStorage.setItem("muttest-theme", t);
    } catch (e) {}
  }
  themeBtn.addEventListener("click", function () {
    applyTheme(themes[(themes.indexOf(theme()) + 1) % themes.length]);
  });
  applyTheme(theme());

  document.addEventListener("keydown", function (e) {
    if (e.metaKey || e.ctrlKey || e.altKey) return;
    if (e.key === "n") jump(1);
    if (e.key === "p") jump(-1);
  });

  // Jump-to-file menu: populated from the rendered files, hidden for a
  // single-file report. Picking a file that the active filter hides falls back
  // to "all" so the target is actually reachable.
  var fj = document.getElementById("filejump");
  var fjFiles = document.querySelectorAll("details.file:not(.plan)");
  if (fj) {
    if (fjFiles.length < 2) {
      fj.style.display = "none";
    } else {
      fjFiles.forEach(function (d, i) {
        if (!d.id) d.id = "file-" + i;
        var o = document.createElement("option");
        o.value = d.id;
        o.textContent = d.querySelector(".fname").textContent;
        fj.appendChild(o);
      });
      fj.addEventListener("change", function () {
        var d = document.getElementById(fj.value);
        fj.selectedIndex = 0;
        if (!d) return;
        var f = body.dataset.f;
        if (f !== "all" && !d.classList.contains("has-" + f)) {
          setFilter("all", true);
        }
        d.open = true;
        d.scrollIntoView({ block: "start" });
      });
    }
  }

  setFilter(body.dataset.f || "all", false);
})();
