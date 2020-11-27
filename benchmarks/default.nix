let
  nixpkgs = fetchGit {
    url = "https://github.com/nixos/nixpkgs";
    ref = "release-20.03";
    rev = "8d49ebf4888df567792a82916d830a26842ece0e";
  };
in with (import nixpkgs {});
let

  # For lru-cache benchmark:
  y1Train = fetchurl {
    url = "http://trec-car.cs.unh.edu/datareleases/v2.1/unprocessedAllButBenchmark.v2.1.tar.xz";
    sha256 = "117iwv443bbxxk6q43kskbdsmj1a79dwmyiawmxnjzdf996hlwp7";
  };

  pages =
    runCommand "pages" { } ''
      mkdir -p $out
      tar -x -C $out -f ${y1Train} unprocessedAllButBenchmark.v2.1/unprocessedAllButBenchmark.Y2.cbor --strip-components=1
    '';

  # TODO: Derive targets.lst
  targetsLst =
    runCommand "targets.lst" {
      nativeBuildInputs = [
        (python3.withPackages (pkgs: with pkgs; [ numpy ]))
        gzip gawk
      ];
    } ''
      gunzip -c ${clickLog} \
        | awk '/^en/ {if ($3 > 2) print $2, $3; }' \
        | python3 ${lru-cache/mk-target-list.py} \
        > $out
    '';

	clickLog =
    fetchurl {
      url = "https://dumps.wikimedia.org/other/pagecounts-raw/2016/2016-01/pagecounts-20160105-090000.gz";
      sha256 = "1v5hb7z06j0v35ii8rcb18rckq2pyb3s4dzaz0sqxq4afh0iywml";
    };

  # For search benchmark:
  paragraphs = fetchurl {
    url = "http://trec-car.cs.unh.edu/datareleases/v2.0/paragraphCorpus.v2.0.tar.xz";
    sha256 = "1kp717hnpi35zxf403cg5709xbfv6dsw2521rq3qkd0x78fais60";
  };

  searchParagraphs = runCommand "search-paras" {
	  LC_ALL = "en_US.UTF-8";
    LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive";
		nativeBuildInputs = [ trec-car-tools gzip xz gnutar ];
    preferLocalBuild = true;
	} ''
    mkdir -p $out
    tar -xf ${paragraphs} paragraphCorpus/dedup.articles-paragraphs.cbor
    ls -lhr
    trec-car-dump paragraph-corpus paragraphCorpus/dedup.articles-paragraphs.cbor | gzip > $out/trec-car-paragraphs.txt.gz
  '';

  searchCorpus = runCommand "search-corpus" {
    nativeBuildInputs = [ python3 gzip ];
    preferLocalBuild = true;
  } ''
    mkdir -p $out
		gunzip -c ${searchParagraphs}/trec-car-paragraphs.txt.gz | split -l 1000 - $out/x
  '';

  searchCommands = 
    let
      pagesToc = 
        runCommand "search-commands-toc" {
          nativeBuildInputs = [ python3 trec-car-tools ];
          preferLocalBuild = true;
          LC_ALL = "en_US.UTF-8";
          LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive";
        } ''
          ln -s ${pages}/unprocessedAllButBenchmark.Y2.cbor pages.cbor
          trec-car-build-toc pages ./pages.cbor
          cp pages.cbor.toc $out
        '';
      namesToc =
        runCommand "search-commands-names-toc" {
          nativeBuildInputs = [ python3 trec-car-tools ];
          preferLocalBuild = true;
          LC_ALL = "en_US.UTF-8";
          LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive";
        } ''
          ln -s ${pages}/unprocessedAllButBenchmark.Y2.cbor pages.cbor
          trec-car-build-toc page-names ./pages.cbor
          cp pages.cbor.name $out
        '';
      redirectsToc =
        runCommand "search-commands-redirects-toc" {
          nativeBuildInputs = [ python3 trec-car-tools ];
          preferLocalBuild = true;
          LC_ALL = "en_US.UTF-8";
          LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive";
        } ''
          ln -s ${pages}/unprocessedAllButBenchmark.Y2.cbor pages.cbor
          trec-car-build-toc page-redirects ./pages.cbor
          cp pages.cbor.redirect $out
        '';
      queries =
        runCommand "queries" {
          nativeBuildInputs = [ trec-car-tools ];
          preferLocalBuild = true;
          LC_ALL = "en_US.UTF-8";
          LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive";
        } ''
          ln -s ${pages}/unprocessedAllButBenchmark.Y2.cbor pages.cbor
          ln -s ${pagesToc} pages.cbor.toc
          ln -s ${namesToc} pages.cbor.name
          ln -s ${redirectsToc} pages.cbor.redirect
          trec-car-dump queries pages.cbor > $out
        '';
    in
      runCommand "search-commands" {
        nativeBuildInputs = [ python3 ];
        preferLocalBuild = true;
        LC_ALL = "en_US.UTF-8";
        LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive";
      } ''
        python3 ${search/mk-trec-car-corpus.py} \
          --corpus-dir=${searchCorpus} \
          --queries=${queries} \
          --corpus-size=1000
        cp commands $out
      '';

  # Utilities:
  trecCarPackages =
    let
      src = fetchgit {
        url = "https://github.com/TREMA-UNH/trec-car-tools-haskell.git";
        rev = "b681a25e4ab5dd26d26dc0deb83111eb36b2fe33";
        sha256 = "0cmy8469xws4m9ir8bvwajsmvmjwismvdxjiykxgl17cl4b7fr60";
        fetchSubmodules = true;
      };
    in (import src {}).trecCarPackages;

  inherit (trecCarPackages) trec-car-tools trec-car-types;

  ghc-utils = fetchGit {
    url = "https://gitlab.haskell.org/bgamari/ghc-utils";
    ref = "master";
    rev = "ec318a24f5cc5422cecc143fbb22d67838143623";
  };

  nofib-shake = haskellPackages.callCabal2nix "nofib-run" ./nofib/shake {};

  gc-stats = import ../utils;

  bench-utils = haskellPackages.callCabal2nix "bench-utils" ./bench-utils {};

  lru-cache = haskellPackages.callCabal2nix "lru-cache" ./lru-cache {
    inherit trec-car-types bench-utils;
  };

  env = ''
    export PAGES_CBOR="${pagesToc}/unprocessedAllButBenchmark.Y2.cbor"
    export PATH=${lib.makeBinPath [ nofib-shake gc-stats wrk2 ]}:$PATH
    export GHC_UTILS=${ghc-utils}
    export SEARCH_COMMANDS=${searchCommands}
    export TARGETS_LST=${targetsLst}
  '';

	envScript = writeScript "env" env;

in {
  inherit trec-car-tools
    clickLog pagesToc paragraphs y1Train
    searchCommands searchCorpus
    envScript
    nofib-shake;
}
  
