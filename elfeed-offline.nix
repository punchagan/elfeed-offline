{
  lib,
  buildDunePackage,
  dream,
  cmdliner,
  cohttp,
  cohttp-lwt-unix,
  lwt,
  brr,
  uri,
  uri-sexp,
  ptime,
  astring,
  lwd,
  brr-lwd,
  alcotest,
  syndic,
}:

buildDunePackage (finalAttrs: {
  pname = "elfeed-offline";
  version = "0.0.1";

  src = with lib.fileset; toSource {
    root = ./.;
    fileset = gitTracked ./.;
  };

  propagatedBuildInputs = [
    dream
    cmdliner
    cohttp
    cohttp-lwt-unix
    lwt
    brr
    uri
    uri-sexp
    ptime
    astring
    lwd
    brr-lwd
  ];

  doCheck = true;
  checkInputs = [
    alcotest
    syndic
  ];

  meta = {
    homepage = "https://github.com/punchagan/elfeed-offline";
    description = "Offline reading web interface for the Emacs RSS client Elfeed";
    license = lib.licenses.agpl3Only;
  };
})
