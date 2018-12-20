{ mkDerivation, aeson, array, base, bytestring, cmdargs, comonad
, containers, directory, filepath, fox, free, lens, megaparsec, mtl
, pretty, process, recursion-schemes, semigroups, stdenv, tasty
, tasty-hunit, tasty-rerun, text, transformers
, unordered-containers
}:
mkDerivation {
  pname = "lamb";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    array base cmdargs comonad containers directory filepath free lens
    megaparsec mtl pretty process recursion-schemes semigroups tasty
    tasty-hunit tasty-rerun text transformers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson base bytestring directory filepath fox tasty tasty-hunit
    tasty-rerun text unordered-containers
  ];
  description = "a little lambda";
  license = stdenv.lib.licenses.mit;
}
