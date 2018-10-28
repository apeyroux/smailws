{ mkDerivation, aeson, base, containers, dhall, hpack, http-types
, jwt, mime-mail, scotty, stdenv, text, time, wai-extra
}:
mkDerivation {
  pname = "smailws";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base containers dhall http-types jwt mime-mail scotty text
    time wai-extra
  ];
  preConfigure = "hpack";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
