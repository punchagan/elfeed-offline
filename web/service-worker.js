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
const OFFLINE_TAGS_URL = "http://sw.offline/elfeed/offline-tags";

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
        const cache = await caches.open(C_CONTENT);
        let status;
        try {
          const resp = await fetch(e.request);
          // Try syncing offline tags, if any
          e.waitUntil(syncOfflineTags());
          // We delete and put since keys are FIFO and we want the most recent
          // at the end for search caches.
          if (resp.ok) {
            await cache.delete(e.request);
            await cache.put(e.request, resp.clone());
            return await patchSearchResponse(resp);
          }
          status = resp.status;
        } catch (err) {
          status = 404;
          console.warn("Fetch failed; returning cached content instead.", err);
        }
        const cached = await cache.match(e.request);
        // TODO: Use a separate header for elfeed server status, instead of
        // overloading X-Cache header?
        const cacheStatus = status === 500 || status === 403 ? "HIT-X" : "HIT";
        if (cached)
          return withHeader(
            await patchSearchResponse(cached),
            "X-Cache",
            cacheStatus,
          );
        if (url.pathname.startsWith("/elfeed/content/")) {
          return new Response("<p>Offline and not cached yet.</p>", {
            status: 200,
            headers: { "content-type": "text/html; charset=utf-8" },
          });
        }
        // Try to find any cached search requests, if possible
        else if (url.pathname === "/elfeed/search") {
          const keys = (await cache.keys()).reverse();
          const req = keys.find((req) => req.url.includes("/elfeed/search"));
          if (req) {
            const cachedSearch = await cache.match(req);
            return withHeader(
              await patchSearchResponse(cachedSearch),
              "X-Cache",
              "NEAR",
            );
          } else {
            return new Response("{}", {
              status: status,
              headers: { "content-type": "application/json; charset=utf-8" },
            });
          }
        }
      })(),
    );
  }
};

const mergeTagsOffline = async (body) => {
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

const getPendingTags = async () => {
  const pending = await metaGet(OFFLINE_TAGS_URL);
  const add = pending?.add || {};
  const remove = pending?.remove || {};
  const hasWork = Object.keys(add).length > 0 || Object.keys(remove).length > 0;
  return { hasWork, add, remove };
};

const patchSearchResponse = async (resp) => {
  if (!resp.ok) return resp;
  const url = new URL(resp.url);
  if (url.pathname !== "/elfeed/search") return resp;
  const { hasWork, add, remove } = await getPendingTags();
  if (!hasWork) return resp;
  console.log("Patching search response with offline tags");
  const entries = await resp.json();
  const updatedEntries = entries.map((entry) => {
    const newEntry = { ...entry };
    for (const [tag, tagEntries] of Object.entries(add)) {
      if (tagEntries.includes(entry.webid)) {
        newEntry.tags = Array.from(new Set([...(newEntry.tags || []), tag]));
      }
    }
    for (const [tag, tagEntries] of Object.entries(remove)) {
      if (tagEntries.includes(entry.webid)) {
        newEntry.tags = (newEntry.tags || []).filter((t) => t !== tag);
      }
    }
    return newEntry;
  });
  return new Response(JSON.stringify(updatedEntries), {
    status: resp.status,
    statusText: resp.statusText,
    headers: resp.headers,
  });
};

const syncOfflineTags = async () => {
  return withLock(OFFLINE_TAGS_URL, async () => {
    const { add, remove, hasWork } = await getPendingTags();
    if (!hasWork) return; // nothing to do
    console.log("Syncing offline tags:", JSON.stringify({ add, remove }));
    // helper: send one op, and only clear entries if it succeeded
    const sendOp = async (entries, kind, tag) => {
      const body = { entries };
      body[kind] = [tag]; // kind is "add" or "remove"
      const resp = await fetch("/elfeed/tags", {
        method: "PUT",
        headers: { "content-type": "application/json" },
        body: JSON.stringify(body),
      });
      return resp.ok;
    };

    // Try syncing all adds
    for (const [tag, entries] of Object.entries(add)) {
      if (!entries || entries.length === 0) continue;
      let ok;
      try {
        ok = await sendOp(entries, "add", tag);
      } catch {
        return; // server not reachable
      }
      if (!ok) return; // stop; keep pending
      delete add[tag]; // sent successfully
    }

    // Try syncing all removes
    for (const [tag, entries] of Object.entries(remove)) {
      if (!entries || entries.length === 0) continue;
      let ok;
      try {
        ok = await sendOp(entries, "remove", tag);
      } catch {
        return;
      }
      if (!ok) return;
      delete remove[tag];
    }

    // Persist cleared state
    await metaPut(OFFLINE_TAGS_URL, { add, remove });
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

const notifyAll = async (msg) => {
  const cs = await self.clients.matchAll();
  cs.forEach((c) => c.postMessage(msg));
};

const metaPut = async (key, info) => {
  const m = await caches.open(META);
  await m.put(
    key + ":meta",
    new Response(JSON.stringify(info), {
      headers: { "content-type": "application/json" },
    }),
  );
};

const metaGet = async (key) => {
  const m = await caches.open(META);
  const r = await m.match(key + ":meta");
  return r ? await r.json() : null;
};

const bytesUsed = async (cacheName) => {
  const c = await caches.open(cacheName);
  const keys = await c.keys();
  let total = 0;
  for (const k of keys) {
    const mi = (await metaGet(k.url)) || {};
    total += mi.size || 0;
  }
  return total;
};

const prefetchIds = async (ids) => {
  const cache = await caches.open(C_CONTENT);
  const q = ids.slice();
  let done = 0;
  const start = await bytesUsed(C_CONTENT);

  const worker = async () => {
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
        if (!resp.ok) {
          notifyAll({
            type: "PREFETCH_ERROR",
            msg: `Failed to fetch ${id} with ${resp.statusText}`,
          });
          continue;
        }
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
        notifyAll({ type: "PREFETCH_ERROR", msg: `Error when fetching ${id}` });
      }
    }
  };

  const workers = Array.from(
    { length: Math.min(PREFETCH_CONCURRENCY, ids.length) },
    worker,
  );
  await Promise.all(workers);
  if (done === ids.length)
    notifyAll({ type: "PREFETCH_DONE", total: ids.length });
  else if (done === 0)
    notifyAll({
      type: "PREFETCH_STOP",
      reason: `Failed to save entries!`,
    });
  else
    notifyAll({
      type: "PREFETCH_STOP",
      reason: `Only ${done} of ${ids.length} entries prefetched`,
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
  if (url.origin !== location.origin) return;
  else if (e.request.method === "GET") getHandler(e);
  else if (e.request.method === "PUT") putHandler(e);
});

self.addEventListener("message", (e) => {
  const msg = e.data || {};
  if (msg.type === "PREFETCH" && Array.isArray(msg.ids)) {
    e.waitUntil(prefetchIds(msg.ids));
  }
});
