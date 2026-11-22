{
  description = "shell env";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-25.11";
    nixpkgs-jdk.url = "github:NixOS/nixpkgs/2074cc672349f14240fb7ea90d509e0b643e9dd4";
    flake-utils.url = "github:numtide/flake-utils";
    codex-cli-nix.url = "github:sadjow/codex-cli-nix";
  };

  outputs = { flake-utils, self, nixpkgs, nixpkgs-jdk, codex-cli-nix, ... }:
  let
    system = flake-utils.lib.system.x86_64-linux;
    pkgs = nixpkgs.legacyPackages.${system};
    jdk = nixpkgs-jdk.legacyPackages.${system}.temurin-bin-27;
    graalvm = pkgs.graalvmPackages.graalvm-ce-musl;
  in
  {
    packages.${system} = rec {
      csf = pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
        pname = "csf";
        version = "0.3.0";
        src = pkgs.fetchurl {
          url = "https://github.com/ValdemarGr/csf/releases/download/v${finalAttrs.version}/csf-linux-x86_64-native.tar.gz";
          hash = "sha256-xJfQvpYObY8VW+PWGErMjLwp+azIOQ/ByuV+j13nO3Y=";
        };

        sourceRoot = ".";
        dontBuild = true;
        dontStrip = true;
        installPhase = ''
          runHook preInstall
          install -Dm755 csf "$out/bin/csf"
          runHook postInstall
        '';
        meta.mainProgram = "csf";
      });
      default = csf;
    };

    devShells.${system}.default = pkgs.mkShell {
        name = "env";
        buildInputs = [
          jdk
          graalvm
          (pkgs.sbt.override { jre = jdk; })
          pkgs.stdenv.cc
          pkgs.zlib
          pkgs.gnutar
          pkgs.gzip
          pkgs.git
          codex-cli-nix.packages.${system}.default
        ];
        JAVA_HOME = jdk.home;
        GRAALVM_HOME = graalvm.home;
        runScript = "zsh";
        shellHook = ''
          export SHELL='zsh'
        '';
    };
  };
}
