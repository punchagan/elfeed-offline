const SHELL = [
  "/",
  "/index.html",
  "/manifest.webmanifest",
  "/css/app.css",
  "/js/app.js",
];
const C_SHELL = "shell-v1";
const C_CONTENT = "content-v1";

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

  if (url.pathname.startsWith("/elfeed/content/")) {
    e.respondWith(
      (async () => {
        const cache = await caches.open("content-v1");
        try {
          const resp = await fetch(e.request);
          await cache.put(e.request, resp.clone());
          return resp;
        } catch (err) {
          const cached = await cache.match(e.request);
          if (cached) return cached;
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
