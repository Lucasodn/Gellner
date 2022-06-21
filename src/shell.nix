with import <nixpkgs> { };
let
  my-r = rWrapper.override {
    packages = with rPackages; [
      ggplot2
      plyr
      tidyr
      devtools
      tidyverse
      styler
      openxlsx
      languageserver
      pacman
      stringi
    ];
  };
in pkgs.mkShell {
  buildInputs = with pkgs; [
    git
    glibcLocales
    openssl
    which
    openssh
    curl
    wget
    my-r
  ];
  shellHook = ''
    mkdir -p "$(pwd)/_libs"
  '';
}
