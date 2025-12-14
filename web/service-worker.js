const SHELL = [
  "/",
  "/index.html",
  "/manifest.webmanifest",
  "/css/app.css",
  "/js/app.js",
];
const C_SHELL = "shell-v1";
const C_CONTENT = "content-v1";
const META = "meta-v1";
const PREFETCH_CONCURRENCY = 4;
const PREFETCH_MAX_BYTES = 40 * 1024 * 1024; // soft cap (~40 MB)

const withHeader = (resp, key, value) => {
  const headers = new Headers(resp.headers);
  headers.set(key, value);
  return new Response(resp.body, {
    status: resp.status,
    statusText: resp.statusText,
    headers: headers,
  });
};

const locks = new Map();

const withLock = (key, fn) => {
  const prev = locks.get(key) || Promise.resolve();
  const next = prev.then(fn, fn); // keep chain alive even if fn throws
  locks.set(
    key,
    next.finally(() => {
      if (locks.get(key) === next) locks.delete(key);
    }),
  );
  return next;
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

const getHandler = async (e) => {
  const url = new URL(e.request.url);
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
  }
};

const mergeTagsOffline = async (body) => {
  const OFFLINE_TAGS_URL = "http://sw.local/elfeed/offline-tags";
  return withLock(OFFLINE_TAGS_URL, async () => {
    const offlineTags = (await metaGet(OFFLINE_TAGS_URL)) || {};
    // body example: { entries: [...], add?: [...], remove?: [...] } where
    // add/remove are lists of tags to add/remove from all entries. We merge
    // these into offlineTags.
    const toStore = { ...offlineTags };
    for (const entry of body.entries) {
      const addTags = toStore?.add || {};
      const removeTags = toStore?.remove || {};
      if (Array.isArray(body.add)) {
        for (const tag of body.add) {
          // Add entry to a list associated with the tag
          const tagEntries = new Set(addTags[tag] || []);
          tagEntries.add(entry);
          addTags[tag] = Array.from(tagEntries);
          if (removeTags[tag]) {
            const updated = new Set(removeTags[tag]);
            updated.delete(entry);
            removeTags[tag] = Array.from(updated);
          }
        }
      }
      if (Array.isArray(body.remove)) {
        for (const tag of body.remove) {
          // Add entry to a list associated with the tag
          const tagEntries = new Set(removeTags[tag] || []);
          tagEntries.add(entry);
          removeTags[tag] = Array.from(tagEntries);
          // Also remove from addTags if present
          if (addTags[tag]) {
            const updated = new Set(addTags[tag]);
            updated.delete(entry);
            addTags[tag] = Array.from(updated);
          }
        }
      }
      toStore.add = addTags;
      toStore.remove = removeTags;
      console.log("Merged offline tags for entry:", entry);
    }
    await metaPut(OFFLINE_TAGS_URL, toStore);
  });
};

const putHandler = async (e) => {
  const url = new URL(e.request.url);
  if (url.pathname === "/elfeed/tags") {
    e.respondWith(
      (async () => {
        // Clone the body to use in case of failure, because request bodies can
        // only be read once. Making the fetch request makes the body unusable
        // in case of failure, so we clone it first.
        const clone = e.request.clone();
        try {
          const resp = await fetch(e.request);
          return resp;
        } catch (err) {
          // Cache the request body for later syncing
          const body = await clone.json();
          await mergeTagsOffline(body);
          return new Response(null, { status: 202 });
        }
      })(),
    );
  }
};

self.addEventListener("fetch", (e) => {
  const url = new URL(e.request.url);
  if (url.origin !== location.origin) return;
  else if (e.request.method === "GET") getHandler(e);
  else if (e.request.method === "PUT") putHandler(e);
});

async function notifyAll(msg) {
  const cs = await self.clients.matchAll();
  cs.forEach((c) => c.postMessage(msg));
}

async function metaPut(key, info) {
  const m = await caches.open(META);
  await m.put(
    key + ":meta",
    new Response(JSON.stringify(info), {
      headers: { "content-type": "application/json" },
    }),
  );
}

async function metaGet(key) {
  const m = await caches.open(META);
  const r = await m.match(key + ":meta");
  return r ? await r.json() : null;
}

async function bytesUsed(cacheName) {
  const c = await caches.open(cacheName);
  const keys = await c.keys();
  let total = 0;
  for (const k of keys) {
    const mi = (await metaGet(k.url)) || {};
    total += mi.size || 0;
  }
  return total;
}

async function prefetchIds(ids) {
  const cache = await caches.open(C_CONTENT);
  const q = ids.slice();
  let done = 0;
  const start = await bytesUsed(C_CONTENT);

  async function worker() {
    while (q.length) {
      const id = q.shift();
      const url = `/elfeed/content/${id}`;
      const req = new Request(url, { credentials: "same-origin" });

      if (await cache.match(req)) {
        done++;
        notifyAll({ type: "PREFETCH_PROGRESS", done, total: ids.length });
        continue;
      }

      try {
        const resp = await fetch(req);
        const clone = resp.clone();
        await cache.put(req, resp);
        const size = (await clone.arrayBuffer()).byteLength;
        await metaPut(url, { ts: Date.now(), size });
        done++;
        notifyAll({ type: "PREFETCH_PROGRESS", done, total: ids.length });

        if (start + size > PREFETCH_MAX_BYTES) {
          notifyAll({ type: "PREFETCH_STOP", reason: "quota" });
          q.length = 0; // stop
          return;
        }
      } catch {
        // ignore; continue with next
        done++;
        notifyAll({ type: "PREFETCH_PROGRESS", done, total: ids.length });
      }
    }
  }

  const workers = Array.from(
    { length: Math.min(PREFETCH_CONCURRENCY, ids.length) },
    worker,
  );
  await Promise.all(workers);
  notifyAll({ type: "PREFETCH_DONE", total: ids.length });
}

self.addEventListener("message", (e) => {
  const msg = e.data || {};
  if (msg.type === "PREFETCH" && Array.isArray(msg.ids)) {
    e.waitUntil(prefetchIds(msg.ids));
  }
});
