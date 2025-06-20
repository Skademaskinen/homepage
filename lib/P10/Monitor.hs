module P10.Monitor where
import Data.String.QM (qm)
import P10.Constants (forecasterExposedPort)

monitor :: String
monitor = [qm|
    #!/usr/bin/env bash
    nix run github:aau-p9s/forecaster#monitor localhost:${port}
|]
    where
        port = show forecasterExposedPort
