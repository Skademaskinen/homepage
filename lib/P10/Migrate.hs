module P10.Migrate where
import Data.String.QM (qm)
import P10.Constants (dbName, dbUser, dbHost, dbPort, dbExposedPort)


migrate :: String
migrate = [qm|
    #!/usr/bin/env bash

    migrate() {
        echo "$1" | psql "${dbName}" -U "${dbUser}" -h "localhost" -p "${dbExposedPort}"
    }

    if ! command -v psql > /dev/null 2>&1; then
        echo "psql not found in PATH"
        exit 1
    fi

    migrate "${modelTable}"
    migrate "${forecastTable}"
    migrate "${historicTable}"
    migrate "${settingsTable}"
    migrate "${serviceTable}"
    migrate "${baselineTable}"
|]
    where
        modelTable = [qm|
            START TRANSACTION;

            CREATE TABLE IF NOT EXISTS Models
            (
                Id        UUID        NOT NULL PRIMARY KEY,
                Name      varchar(40) NOT NULL,
                ServiceId UUID        NOT NULL,
                Bin       bytea       NOT NULL,
                Ckpt      bytea,
                TrainedAt timestamp   NOT NULL
            );
            COMMIT;
        |]
        forecastTable = [qm|
            START TRANSACTION;

            CREATE TABLE IF NOT EXISTS Forecasts
            (
                Id              UUID      NOT NULL PRIMARY KEY,
                ServiceId       UUID      NOT NULL UNIQUE,
                CreatedAt       timestamp NOT NULL,
                ModelId         UUID      NOT NULL,
                Forecast        jsonb     NOT NULL,
                HasManualChange boolean   NOT NULL DEFAULT false
            );
            COMMIT;
        |]
        historicTable = [qm|
            START TRANSACTION;
            
            CREATE TABLE IF NOT EXISTS HistoricData
            (
                Id           UUID      NOT NULL PRIMARY KEY,
                ServiceId    UUID      NOT NULL,
                CreatedAt    timestamp NOT NULL,
                HistoricData jsonb     NOT NULL
            );
            COMMIT;
        |]
        settingsTable = [qm|
            START TRANSACTION;

            CREATE TABLE IF NOT EXISTS Settings
            (
                Id               UUID    NOT NULL PRIMARY KEY,
                ServiceId        UUID    NOT NULL UNIQUE,
                ScaleUp          integer NOT NULL,
                ScaleDown        integer NOT NULL,
                MinReplicas      integer NOT NULL,
                MaxReplicas      integer NOT NULL,
                ScalePeriod      integer NOT NULL,
                TrainInterval    integer NOT NULL
            );
            
            COMMIT;
        |]
        serviceTable = [qm|
            START TRANSACTION;

            CREATE TABLE IF NOT EXISTS Services
            (
                Id                 UUID         NOT NULL PRIMARY KEY,
                Name               varchar(255) NOT NULL UNIQUE,
                AutoscalingEnabled boolean      NOT NULL
            );
            COMMIT;
        |]
        baselineTable = [qm|
            START TRANSACTION;
            
            CREATE TABLE IF NOT EXISTS BaselineModels
            (
                Id        UUID        NOT NULL PRIMARY KEY,
                Name      varchar(40) NOT NULL,
                Bin       bytea       NOT NULL,
                Ckpt      bytea,
                TrainedAt timestamp   NOT NULL
            );
            COMMIT;
        |]
