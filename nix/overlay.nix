final: prev:
let 
  inherit (prev) lib stdenv;
  disableCheck = package: package.overrideAttrs (o: { doCheck = false; });
in
{
  ocamlPackages = prev.ocaml-ng.ocamlPackages_5_00.overrideScope' (oself: osuper: rec {
    rely = osuper.reason-native.rely.overrideAttrs (_: {
      postPatch = ''
        substituteInPlace src/rely/TestSuiteRunner.re --replace "Pervasives" "Stdlib"
      '';
    });
    alcotest = osuper.alcotest.overrideAttrs (o: {
      src = prev.fetchurl {
        url = "https://github.com/mirage/alcotest/releases/download/1.5.0/alcotest-js-1.5.0.tbz";
        sha256 = "0v4ghia378g3l53r61fj98ljha0qxl82xp26y9frjy1dw03ija2l";
      };
      propagatedBuildInputs = prev.lib.lists.remove osuper.uuidm o.propagatedBuildInputs;
    });
    qcheck = osuper.qcheck.overrideAttrs (_: {
      src = prev.fetchurl {
        url = "https://github.com/c-cube/qcheck/archive/v0.18.1.tar.gz";
        sha256 = "1jzhwrzsf5290rs7hsa1my5yh1x95sh2sz92c4svd8yahzdlny7m";
      };
    });
    data-encoding = disableCheck osuper.data-encoding;
    json-data-encoding = disableCheck osuper.json-data-encoding;
    json-data-encoding-bson = disableCheck osuper.json-data-encoding-bson;
    hxd = disableCheck (osuper.hxd.overrideAttrs (o: {
      src = builtins.fetchurl {
        url = "https://github.com/dinosaure/hxd/releases/download/v0.3.1/hxd-v0.3.1.tbz";
        sha256 = "1g19dgwj29ykrv3gk7q66fjjlc1n1z9bz1y2q3g2klvww68nq8hw";
      };
    }));
    mrmime = disableCheck osuper.mrmime;
    cmdliner = osuper.cmdliner.overrideAttrs (_: {
      src = builtins.fetchurl {
        url = "https://github.com/dbuenzli/cmdliner/archive/refs/tags/v1.0.4.tar.gz";
        sha256 = "13c53b1cxkq2nj444655skw5a1mcxzbaqwqsqjf7jbwradb3hmxa";
      };
    });

    asetmap = stdenv.mkDerivation rec {
      version = "0.8.1";
      pname = "asetmap";
      src = prev.fetchurl {
        url = "https://github.com/dbuenzli/asetmap/archive/refs/tags/v0.8.1.tar.gz";
        sha256 = "051ky0k62xp4inwi6isif56hx5ggazv4jrl7s5lpvn9cj8329frj";
      };

      nativeBuildInputs = with osuper; [
        topkg
        findlib
        ocamlbuild
        ocaml
      ];

      inherit (osuper.topkg) buildPhase installPhase;
    };

    prometheus = osuper.buildDunePackage rec {
      version = "1.1.0";
      pname = "prometheus";
      src = prev.fetchurl {
        url = "https://github.com/mirage/prometheus/releases/download/v1.1/prometheus-v1.1.tbz";
        sha256 = "1r4rylxmhggpwr1i7za15cpxdvgxf0mvr5143pvf9gq2ijr8pkzv";
      };

      propagatedBuildInputs = with osuper; [
        astring
        asetmap
        fmt
        re
        lwt
        alcotest
      ];
    };
  });
}
