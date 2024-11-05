{pkgs, ...}:

pkgs.haskellPackages.mkDerivation {
    pname = "homepage-bin";
    version = "0.1.0.0";
    src = ./..;
    isLibrary = false;
    isExecutable = true;
    libraryHaskellDepends = with pkgs.haskellPackages; [
        aeson base blaze-builder blaze-html bytestring cryptonite directory
        http-conduit http-types ihp-hsx monad-logger password persistent
        persistent-mysql regex-compat split string-random text time
        utf8-string uuid wai warp yaml
    ];
    executableHaskellDepends = with pkgs.haskellPackages; [
        aeson aeson-qq base blaze-builder blaze-html bytestring cryptonite
        directory http-conduit http-types ihp-hsx monad-logger password
        persistent persistent-mysql regex-compat split string-random text
        time utf8-string uuid wai warp yaml
    ];
    testHaskellDepends = with pkgs.haskellPackages; [ base ];
    doHaddock = false;
    license = "unknown";
}
