const $ = (id) => document.getElementById(id);
const status = (msg) => {
  $("status").textContent = msg;
};

function ymd(ms) {
  const d = new Date(Number(ms));
  const y = d.getFullYear(),
    m = String(d.getMonth() + 1).padStart(2, "0"),
    day = String(d.getDate()).padStart(2, "0");
  return `${y}-${m}-${day}`;
}

function escapeHtml(s) {
  return String(s).replace(
    /[&<>"']/g,
    (c) =>
      ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" })[
        c
      ],
  );
}

async function search(q) {
  status("loading…");
  const url = `/elfeed/search?q=${encodeURIComponent(q)}`;
  try {
    const res = await fetch(url, { credentials: "same-origin" });
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const data = await res.json(); // elfeed returns an array
    renderResults(data);
    status(`loaded ${data.length} items`);
  } catch (e) {
    console.warn("search failed", e);
    status("offline or backend unavailable — showing cache if any");
  }
}

function renderResults(entries) {
  const host = $("results");
  host.innerHTML = "";
  if (!Array.isArray(entries) || entries.length === 0) {
    host.innerHTML = '<p class="muted">No results.</p>';
    return;
  }
  for (const e of entries) {
    const item = document.createElement("div");
    item.className = "entry";

    const date = e.date || e.date_ms || 0;
    const feedTitle = e.feed?.title || "";
    const title = e.title || "(no title)";

    item.innerHTML = `
      <span class="date">${ymd(date)}</span>
      <span class="title">${escapeHtml(title)}</span>
      <span class="feed">${escapeHtml(feedTitle)}</span>
    `;

    item.addEventListener("click", () => {
      if (e.content) {
        $("content").src = `/elfeed/content/${e.content}`;
      } else if (e.link) {
        window.open(e.link, "_blank");
      }
    });
    host.appendChild(item);
  }
}

document.getElementById("search-form").addEventListener("submit", (ev) => {
  ev.preventDefault();
  const q = $("q").value.trim() || "@3-days-old";
  search(q);
});

// initial load
$("q").value = "@30-days-old";
search($("q").value);
