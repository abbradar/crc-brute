with (import <nixpkgs> {}).pkgs;
stdenv.mkDerivation {
  name = "crc-brute";
  buildInputs = [ openssl zlib ];
}
