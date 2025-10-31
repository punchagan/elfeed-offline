(lang dune 3.21)

(name elfeed-offline)

(generate_opam_files true)

(source
 (github punchagan/elfeed-offline))

(authors "Puneeth Chaganti <punchagan@muse-amuse.in>")

(maintainers "Puneeth Chaganti <punchagan@muse-amuse.in>")

(license AGPLv3)

(documentation https://github.com/punchagan/elfeed-offline)

(package
 (name elfeed-offline)
 (synopsis "Offline-first Elfeed (proxy) server with PWA")
 (description
  "Dream-based proxy + offline PWA, caches entries and syncs tags.")
 (depends ocaml dream cohttp cohttp-lwt-unix lwt brr)
 (tags
  ("elfeed" "rss" "feeds" "PWA" "offline" "proxy" "dream")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
