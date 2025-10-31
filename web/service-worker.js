const SHELL = [
  "/",
  "/index.html",
  "/manifest.webmanifest",
  "/css/app.css",
  "/js/app.js",
];
const C_SHELL = "shell-v1";
const C_CONTENT = "content-v1";

const withHeader = (resp, key, value) => {
  const headers = new Headers(resp.headers);
  headers.set(key, value);
  return new Response(resp.body, {
    status: resp.status,
    statusText: resp.statusText,
    headers: headers,
  });
};

self.addEventListener("install", (e) => {
  e.waitUntil(caches.open(C_SHELL).then((c) => c.addAll(SHELL)));
  self.skipWaiting();
});

self.addEventListener("activate", (e) => {
  e.waitUntil(
    caches
      .keys()
      .then((keys) =>
        Promise.all(
          keys
            .filter((k) => ![C_SHELL, C_CONTENT].includes(k))
            .map((k) => caches.delete(k)),
        ),
      ),
  );
  self.clients.claim();
});

self.addEventListener("fetch", (e) => {
  const url = new URL(e.request.url);
  if (e.request.method !== "GET" || url.origin !== location.origin) return;

  if (SHELL.includes(url.pathname)) {
    e.respondWith(
      (async () => {
        const cache = await caches.open(C_SHELL);
        try {
          const resp = await fetch(e.request);
          await cache.put(e.request, resp.clone());
          return resp;
        } catch (err) {
          console.warn("Fetch failed; returning cached page instead.", err);
          const cached = await cache.match(e.request);
          if (cached) return withHeader(cached, "X-Cache", "HIT");
        }
      })(),
    );
  } else if (
    url.pathname.startsWith("/elfeed/content/") ||
    url.pathname === "/elfeed/search"
  ) {
    e.respondWith(
      (async () => {
        const cache = await caches.open("content-v1");
        try {
          const resp = await fetch(e.request);
          // We delete and put since keys are FIFO and we want the most recent
          // at the end for search caches.
          await cache.delete(e.request);
          await cache.put(e.request, resp.clone());
          return resp;
        } catch (err) {
          const cached = await cache.match(e.request);
          if (cached) return withHeader(cached, "X-Cache", "HIT");
          // Special handling for search requests: try to find any cached search
          if (url.pathname === "/elfeed/search") {
            const keys = (await cache.keys()).reverse();
            const req = keys.find((req) => req.url.includes("/elfeed/search"));
            if (req) {
              const cachedSearch = await cache.match(req);
              if (cachedSearch)
                return withHeader(cachedSearch, "X-Cache", "NEAR");
            }
          }
          return new Response("<p>Offline and not cached yet.</p>", {
            status: 200,
            headers: { "content-type": "text/html; charset=utf-8" },
          });
        }
      })(),
    );
    return;
  }
});
