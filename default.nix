{ mkDerivation, base, libpulseaudio, mtl, stdenv }:
mkDerivation {
  pname = "pulseaudio";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base mtl ];
  librarySystemDepends = [ libpulseaudio ];
  executableHaskellDepends = [ base mtl ];
  testHaskellDepends = [ base mtl ];
  homepage = "https://github.com/githubuser/pulseaudio#readme";
  description = "Short description of your package";
  license = stdenv.lib.licenses.bsd3;
}
