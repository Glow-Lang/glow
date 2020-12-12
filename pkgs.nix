let
  pkgs = import ./dep/nixpkgs { inherit config; };

  # TODO: move all repo data to JSON or separate nix file

  # for building nix-thunk
  # TODO: put all this inside default.nix for nix-thunk and have it export a fully built executable
  nix-thunk = import ./dep/nix-thunk {};
  cli-extras-repo = pkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "cli-extras";
    rev = "368ca93cfcca9e2fe077bc0b7649a402ec89dce5";
    sha256 = "1g8j45q1ycqpvz2n8aaq4z8zvak0f0vpqhiy1z3siv230kchkrdi";
  };
  cli-git-repo = pkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "cli-git";
    rev = "bb2034f974ad04c1827ef8ef4160872cb8f00063";
    sha256 = "0mqkmjfvyv25216gj2jhpm9a4i6b3arp86yi2n5dss9j5fzlg7sm";
  };
  cli-nix-repo = pkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "cli-nix";
    rev = "de73e014036f2648af2e9f06b8ce711dfa1e1a22";
    sha256 = "0p5768l1gg5j869m5wfpzhfxlraz0c1vghm9r7h3fin64p3si61z";
  };

  # for building glow-cardano
  plutus-repo = nix-thunk.thunkSource ./dep/plutus;
  cardano-node-repo = nix-thunk.thunkSource ./dep/cardano-node;
  cardano-crypto-repo = nix-thunk.thunkSource ./dep/cardano-crypto;
  cardano-base-repo = nix-thunk.thunkSource ./dep/cardano-base;
  cardano-prelude-repo = nix-thunk.thunkSource ./dep/cardano-prelude;
  ouroboros-network-repo = nix-thunk.thunkSource ./dep/ouroboros-network;
  sexpr-parser-repo = nix-thunk.thunkSource ./dep/sexpr-parser;

  deriving-aeson-repo = pkgs.fetchFromGitHub {
    owner = "fumieval";
    repo = "deriving-aeson";
    rev = "3f8ab60afa4491af62f22095bada9ea61e2ec95a";
    sha256 = "095ml73175wpbkqr15q8c22sgn8svkxw5hk25wd81fgb2q10adg3";
  };
  barbies-repo = pkgs.fetchFromGitHub {
    owner = "jcpetruzza";
    repo = "barbies";
    rev = "df3c0ce218fcaa40cdb8e92425c0e3aff0e47d4a";
    sha256 = "0vn8b1rli2ylci0gazhkjrv7hywjffzqqnvb5dqjjig0glhj6h9k";
  };
  semigroups-repo = pkgs.fetchFromGitHub {
    owner = "ekmett";
    repo = "semigroups";
    rev = "2c662cd60b49927d1ac76c657bc0bf0350869386";
    sha256 = "18hm4sykwf89z63299drq23ybl8qm5zbcs5a35bcviqcxkayq6c4";
  };
  dependent-sum-repo = pkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "dependent-sum";
    rev = "ca4979fa417ce576f80ae468c94ae01c6eec2573";
    sha256 = "1waay3vj0415p2fd0sg6z807yrjm6k9y8bxnnh8bzdk41yb8a0g6";
  };
  row-types-repo = pkgs.fetchFromGitHub {
    owner = "target";
    repo = "row-types";
    rev = "b4d79e104438931f21f872a45e53d3ed1c043cc4";
    sha256 = "0wischjcx04z22b8iiq384jp4gdr80bavbwbl9fczhf46xrmbv79";
  };
  byteunits-repo = pkgs.fetchFromGitHub {
    owner = "chrissound";
    repo = "byteunits";
    rev = "404f96be26aa7b1bcbf5793d02ef37b14f92e20e";
    sha256 = "1hyfili1fnfn4arh45hlrbz2i1n1qw3l1b35h83w0b0r69awy8dv";
  };
  iohk-monitoring-framework-repo = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "iohk-monitoring-framework";
    rev = "5c9627b6aee487f9b7ec44981aba57a6afc659b1";
    sha256 = "0ndnhff32h37xsc61b181m4vwaj4vm1z04p2rfwffnjjmgz23584";
  };
  purescript-bridge-repo = pkgs.fetchFromGitHub {
    owner = "shmish111";
    repo = "purescript-bridge";
    rev = "28c37771ef30b0d751960c061ef95627f05d290e";
    sha256 = "0n6q7g2w1xafngd3dwbbmfxfn018fmq61db7mymplbrww8ld1cp3";
  };
  servant-purescript-repo = pkgs.fetchFromGitHub {
    owner = "shmish111";
    repo = "servant-purescript";
    rev = "ece5d1dad16a5731ac22040075615803796c7c21";
    sha256 = "0ndnhff32h37xsc61b181m4vwaj4vm1z04p2rfwffnjjmgz23582";
  };

  skipTests = package: pkgs.haskell.lib.dontCheck package;
  skipDocs = package: pkgs.haskell.lib.dontHaddock package;
  ignoreVersionBounds = package: pkgs.haskell.lib.doJailbreak package;
  addDevelopmentFlag = package: package.overrideAttrs(old: {
    configureFlags = [
      "-fdevelopment"
    ] ++ old.configureFlags;
  });

  inherit (pkgs.lib.POP) kxPop extendPop;
  inherit (pkgs.gerbil-support) path-src overrideSrcIfShaDiff gerbilFilterSource;

  maybeOverrideDep = name:
    let path = ./dep + "/${name}";
        json = builtins.fromJSON (builtins.readFile "${path}/github.json");
        pre-src = path-src (nix-thunk.thunkSource path) // json; in
        overrideSrcIfShaDiff name pre-src;

  config = {
    packageOverrides = pkgs: rec {

      inherit skipTests skipDocs ignoreVersionBounds addDevelopmentFlag;
      thunkSource = nix-thunk.thunkSource;
      thunkExe = pkgs.haskell.lib.doJailbreak (nixThunkHaskellPackages.callCabal2nix "nix-thunk" (nix-thunk.thunkSource ./dep/nix-thunk) {});

      gerbil-support =
        extendPop pkgs.gerbil-support (gerbil-support: super: {
          # Skip extra files so that we can play with CI configuration yet have the CI reuse cached builds.
          gerbilSkippableFiles = super.gerbilSkippableFiles ++ [".gitlab.yml" "pkgs.nix" "shell.nix" "default.nix" "docs" "future" "dep" "Dockerfile" "Dockerfile.nixos" "glow-install" "ci.ss"];

          prePackages-unstable = extendPop super.prePackages-unstable (_: super:
            maybeOverrideDep "gerbil-utils" super //
            maybeOverrideDep "gerbil-poo" super //
            maybeOverrideDep "gerbil-persist" super //
            maybeOverrideDep "gerbil-ethereum" super //
            { "glow-lang" = extendPop super.glow-lang (params: super:
              let source = gerbilFilterSource ./.; in
              { pre-src = path-src source;
                inherit source;
                inherit pkgs;
                preFiltered = ./.;
                testGerbilLoadPath = "${
                  source}:${
                  pkgs.gerbilPackages-unstable.gerbil-ethereum.src}:${
                  gerbil-support.gerbilLoadPath ([pkgs.glow-lang] ++ params.gerbilInputs)}";
                gerbilEthereumSrc = "${pkgs.gerbilPackages-unstable.gerbil-ethereum.src}";});});});

      nixThunkHaskellPackages = pkgs.haskell.packages.ghc865.override rec {
        overrides = new: old: rec {
          cli-extras = ignoreVersionBounds (new.callCabal2nix "cli-extras" cli-extras-repo {});
          cli-git = ignoreVersionBounds (new.callCabal2nix "cli-git" cli-git-repo {});
          cli-nix = ignoreVersionBounds (new.callCabal2nix "cli-nix" cli-nix-repo {});

          unliftio-core = new.callHackage "unliftio-core" "0.1.2.0" {};
          logging-effect = new.callHackage "logging-effect" "1.3.4" {};
          prettyprinter = new.callHackage "prettyprinter" "1.2.1.1" {};
          ansi-terminal = new.callHackage "ansi-terminal" "0.9.1" {};
        };
      };

      gerbilCardanoHaskellPackages = pkgs.haskell.packages.ghc8102.override rec {
        overrides = new: old: rec {
          # plutus
          plutus-core = skipTests (skipDocs (new.callCabal2nix "plutus-core" (plutus-repo + "/plutus-core") {}));
          plutus-ledger = ignoreVersionBounds (skipTests (skipDocs (new.callCabal2nix "plutus-ledger" (plutus-repo + "/plutus-ledger") {})));
          plutus-tx = skipTests (skipDocs (new.callCabal2nix "plutus-tx" (plutus-repo + "/plutus-tx") {}));
          plutus-tx-plugin = skipTests (skipDocs (new.callCabal2nix "plutus-tx-plugin" (plutus-repo + "/plutus-tx-plugin") {}));
          plutus-contract = skipTests (skipDocs (new.callCabal2nix "plutus-contract" (plutus-repo + "/plutus-contract") {}));
          plutus-scb = skipTests (skipDocs (new.callCabal2nix "plutus-scb" (plutus-repo + "/plutus-scb") {}));
          plutus-use-cases = skipTests (skipDocs (new.callCabal2nix "plutus-use-cases" (plutus-repo + "/plutus-use-cases") {}));
          playground-common = skipTests (skipDocs (new.callCabal2nix "playground-common" (plutus-repo + "/playground-common") {}));
          iots-export = skipTests (skipDocs (new.callCabal2nix "iots-export" (plutus-repo + "/iots-export") {}));
          prettyprinter-configurable = skipTests (skipDocs (new.callCabal2nix "prettyprinter-configurable" (plutus-repo + "/prettyprinter-configurable") {}));

          # cardano
          cardano-binary = addDevelopmentFlag (skipTests (skipDocs (new.callCabal2nix "cardano-binary" (cardano-base-repo + "/binary") {})));
          cardano-config = new.callCabal2nix "cardano-api" (cardano-node-repo + "/cardano-config") {};
          cardano-crypto = new.callCabal2nix "cardano-crypto" cardano-crypto-repo {};
          cardano-prelude = addDevelopmentFlag (new.callCabal2nix "cardano-prelude" cardano-prelude-repo {});
          cardano-prelude-test = addDevelopmentFlag (new.callCabal2nix "cardano-prelude-test" (cardano-prelude-repo + "/test") {});
          cardano-slotting = new.callCabal2nix "cardano-slotting" (cardano-base-repo + "/slotting") {};

          io-sim = new.callCabal2nix "io-sim" (ouroboros-network-repo + "/io-sim") {};
          io-sim-classes = new.callCabal2nix "io-sim-classes" (ouroboros-network-repo + "/io-sim-classes") {};
          Win32-network = new.callCabal2nix "Win32-network" (ouroboros-network-repo + "/Win32-network") {};
          network-mux = skipTests (new.callCabal2nix "network-mux" (ouroboros-network-repo + "/network-mux") {});
          ouroboros-network = ignoreVersionBounds (skipTests (new.callCabal2nix "ouroboros-network" (ouroboros-network-repo + "/ouroboros-network") {}));
          ouroboros-network-framework = ignoreVersionBounds (skipTests (new.callCabal2nix "ouroboros-network-framework" (ouroboros-network-repo + "/ouroboros-network-framework") {}));
          ouroboros-network-testing = new.callCabal2nix "ouroboros-network-testing" (ouroboros-network-repo + "/ouroboros-network-testing") {};
          typed-protocols = new.callCabal2nix "typed-protocols" (ouroboros-network-repo + "/typed-protocols") {};
          typed-protocols-examples = new.callCabal2nix "typed-protocols-examples" (ouroboros-network-repo + "/typed-protocols-examples") {};

          iohk-monitoring = new.callCabal2nix "iohk-monitoring" (iohk-monitoring-framework-repo + "/iohk-monitoring") {};
          contra-tracer = new.callCabal2nix "contra-tracer" (iohk-monitoring-framework-repo + "/contra-tracer") {};
          tracer-transformers = new.callCabal2nix "tracer-transformers" (iohk-monitoring-framework-repo + "/tracer-transformers") {};
          lobemo-backend-ekg = new.callCabal2nix "lobemo-backend-ekg" (iohk-monitoring-framework-repo + "/plugins/backend-ekg") {};

          # other
          deriving-aeson = new.callCabal2nix "deriving-aeson" deriving-aeson-repo {};
          barbies = new.callCabal2nix "barbies" barbies-repo {};
          semigroups = new.callCabal2nix "semigroups" semigroups-repo {};
          dependent-sum = new.callCabal2nix "dependent-sum" (dependent-sum-repo + "/dependent-sum") {};
          row-types = new.callCabal2nix "row-types" row-types-repo {};
          byteunits = new.callCabal2nix "byteunits" byteunits-repo {};
          algebraic-graphs = ignoreVersionBounds (skipTests (new.callHackage "algebraic-graphs" "0.5" {}));
          sexpr-parser = ignoreVersionBounds (new.callCabal2nix "sexpr-parser" sexpr-parser-repo {});
          http-media = ignoreVersionBounds old.http-media;
          size-based = ignoreVersionBounds (skipTests (new.callHackage "size-based" "0.1.2.0" {}));
          lazy-search = ignoreVersionBounds (skipTests (new.callHackage "lazy-search" "0.1.2.0" {}));
          servant = ignoreVersionBounds (skipTests (skipDocs old.servant));
          servant-server = ignoreVersionBounds (skipTests (skipDocs old.servant-server));
          servant-swagger = ignoreVersionBounds old.servant-swagger;
          raw-strings-qq = new.callHackage "raw-strings-qq" "1.1" {};
          canonical-json = ignoreVersionBounds old.canonical-json;
          async-timer = skipTests old.async-timer;
          prometheus = ignoreVersionBounds (skipTests (new.callHackage "prometheus" "2.1.3" {}));
          protolude = ignoreVersionBounds (skipTests (new.callHackage "protolude" "0.3.0" {}));
          servant-client = ignoreVersionBounds old.servant-client;
          servant-client-core = ignoreVersionBounds old.servant-client-core;
          servant-foreign = ignoreVersionBounds old.servant-foreign;
          swagger2 = ignoreVersionBounds old.swagger2;
          nonempty-containers = new.callHackage "nonempty-containers" "0.3.3.0" {};
          persistent-template = new.callHackage "persistent-template" "2.8.2.3" {};
          eventful-sql-common = ignoreVersionBounds (skipDocs (old.eventful-sql-common.overrideAttrs(old: {
            configureFlags = [
              "--ghc-options=-XDerivingStrategies"
              "--ghc-options=-XStandaloneDeriving"
              "--ghc-options=-XUndecidableInstances"
            ] ++ old.configureFlags;
          })));
          purescript-bridge = new.callCabal2nix "purescript-bridge" purescript-bridge-repo {};
        };
      };
    };
    allowBroken = true;
  };
in
  pkgs
