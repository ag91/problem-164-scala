{ }:
let
pkgs = import <nixpkgs> {};
stdenv = pkgs.stdenv;
sbt = pkgs.sbt;
scala = pkgs.scala;

in stdenv.mkDerivation {
name = "test_derivation";

buildInputs = [ sbt scala ];
}
