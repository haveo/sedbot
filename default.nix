{ pkgs ? import <nixpkgs> { } }:

pkgs.haskellPackages.developPackage {
  root = ./.;
  source-overrides = {
    irc-client = /home/haveo/coding/irc-client;
  };
}
