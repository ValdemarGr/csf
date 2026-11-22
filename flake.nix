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
    sbt = pkgs.sbt.override { jre = jdk; };
    sbtRepositories = pkgs.writeText "csf-sbt-repositories" ''
      [repositories]
      maven-central: https://repo.maven.apache.org/maven2/
    '';
    sbtEnvironment = ''
      export HOME="$TMPDIR/home"
      export COURSIER_CACHE="$HOME/coursier"
      mkdir -p "$HOME"
      sbtArgs=(--server -batch "-Duser.home=$HOME" -Dsbt.override.build.repos=true
        "-Dsbt.repository.config=${sbtRepositories}" -Dsbt.supershell=false)
    '';
    sbtDeps = pkgs.stdenvNoCC.mkDerivation {
      name = "csf-sbt-deps";
      src = ./.;
      nativeBuildInputs = [ sbt ];
      outputHashMode = "recursive";
      # Refresh this hash when sbt or Maven dependencies change.
      outputHash = "sha256-n8W97UEKwn1i3QICyZzSHtXavgiq9tIeSjGwiz16q8k=";
      dontFixup = true;

      buildPhase = ''
        ${sbtEnvironment}
        sbt "''${sbtArgs[@]}" 'Compile / packageBin'
      '';
      installPhase = ''
        mkdir -p "$out"
        cd "$HOME"
        find .sbt/boot coursier -type f \( -name '*.jar' -o -name '*.pom' \) \
          -exec cp --parents -t "$out" {} +
      '';
    };
  in
  {
    packages.${system} = rec {
      csf = pkgs.stdenv.mkDerivation {
        pname = "csf";
        version = self.shortRev or self.dirtyShortRev or "unstable";
        src = ./.;
        nativeBuildInputs = [ sbt graalvm ];
        buildInputs = [ pkgs.zlib ];
        JAVA_HOME = jdk.home;
        GRAALVM_HOME = graalvm.home;

        buildPhase = ''
          runHook preBuild
          ${sbtEnvironment}
          cp -r ${sbtDeps}/. "$HOME/"
          chmod -R u+w "$HOME"
          sbt "''${sbtArgs[@]}" nativeImage
          runHook postBuild
        '';
        installPhase = ''
          runHook preInstall
          install -Dm755 target/native/csf "$out/bin/csf"
          runHook postInstall
        '';
        meta.mainProgram = "csf";
      };
      default = csf;
    };

    devShells.${system}.default = pkgs.mkShell {
        name = "env";
        buildInputs = [
          jdk
          graalvm
          sbt
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
