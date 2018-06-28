{ mkDerivation, base, libpulseaudio, mtl, stdenv, time }:
mkDerivation {
  pname = "pulsecontrol";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  libraryHaskellDepends = [ base mtl time ];
  librarySystemDepends = [ libpulseaudio ];
  testHaskellDepends = [ base mtl ];
  homepage = "https://github.com/talanis85/pulsecontrol#readme";
  description = "Query and control a pulseaudio server";
  license = stdenv.lib.licenses.bsd3;
}
