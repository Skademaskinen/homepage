{-# LANGUAGE QuasiQuotes #-}

module P10 where

import Text.RawString.QQ;
import Data.Aeson (Value)
import Data.String.QM (qm)
import Data.Aeson.QQ (aesonQQ)
import Data.Yaml (encode)
import Utils (unpackBS)
import Data.List (intercalate)

-- constants, for ease of use
expose :: Int -> Int
expose target = 30000 + mod target 1000

autoscalerPort :: Int
autoscalerPort = 8080
autoscalerExposedPort :: Int
autoscalerExposedPort = expose autoscalerPort

forecasterPort :: Int
forecasterPort = 8081
forecasterExposedPort :: Int
forecasterExposedPort = expose forecasterPort

workloadPort :: Int
workloadPort = 8082

generatorPort :: Int
generatorPort = 8083
generatorExposedPort :: Int
generatorExposedPort = expose generatorPort

dbName :: String
dbName = "autoscaler"
dbHost :: String
dbHost = "postgres"
dbPort :: String
dbPort = "5432"
dbUser :: String
dbUser = "root"
dbPassword :: String
dbPassword = "password"

aesonToKubeconfig :: [Value] -> String
aesonToKubeconfig = intercalate "\n---\n" . map (unpackBS . encode)

cleanup :: String
cleanup = [r|
    #!/usr/bin/env bash
    
    # Namespaces to keep
    KEEP_NAMESPACES="kube-system kube-public kube-node-lease default"
    
    # Delete all non-core namespaces
    for ns in $(kubectl get ns --no-headers -o custom-columns=":metadata.name"); do
        if [[ ! " $KEEP_NAMESPACES " =~ " $ns " ]]; then
            echo "Deleting namespace: $ns"
            kubectl delete ns "$ns" --grace-period=0 --force
        fi
    done
    
    # Wait a bit to allow namespace deletion to begin
    sleep 5
    
    # Force delete any stuck pods in all namespaces
    for pod in $(kubectl get pods --all-namespaces --field-selector=status.phase=Terminating -o jsonpath='{range .items[*]}{.metadata.namespace}{";"}{.metadata.name}{"\n"}{end}'); do
        IFS=";" read -r ns name <<< "$pod"
        echo "Force deleting stuck pod: $name in namespace: $ns"
        kubectl delete pod "$name" -n "$ns" --grace-period=0 --force
    done
    
    # Optional: Delete other leftover resource types
    echo "Cleaning up remaining resources..."
    for type in deployments services replicasets daemonsets statefulsets jobs cronjobs pvc ingress; do
        kubectl delete $type --all --all-namespaces --ignore-not-found
    done
    
    echo "Cleanup complete."
|]

prerequisites :: String
prerequisites = aesonToKubeconfig [[aesonQQ|
    {
        apiVersion: "apps/v1",
        kind: "Deployment",
        metadata: {
            annotations: {},
            name: "workload-api",
            namespace: "default"
        },
        spec: {
            selector: {
                matchLabels: {
                    app: "workload-api"
                }
            },
            template: {
                metadata: {
                    labels: {
                        app: "workload-api"
                    }
                },
                spec: {
                    containers: [
                        {
                            env: [
                                {
                                    name: "WORKLOAD_PORT",
                                    value: #{show workloadPort}
                                },
                                {
                                    name: "WORKLOAD_STARTUP_DELAY",
                                    value: "0"
                                }
                            ],
                            image: "ghcr.io/aau-p9s/workload-api:latest",
                            name: "workload-api",
                            ports: [
                                {
                                    containerPort: #{workloadPort}
                                }
                            ],
                            resources: {
                                limits: {
                                    cpu: "1000m",
                                    memory: "200Mi"
                                },
                                requests: {
                                    cpu: "500m",
                                    memory: "100Mi"
                                }
                            }
                        }
                    ]
                }
            }
        }
    }
|], [aesonQQ|
{
    apiVersion: "apps/v1",
    kind: "Deployment",
    metadata: {
        annotations: {},
        name: "workload-generator",
        namespace: "default"
    },
    spec: {
        selector: {
            matchLabels: {
                app: "workload-generator"
            }
        },
        template: {
            metadata: {
                labels: {
                    app: "workload-generator"
                }
            },
            spec: {
                containers: [
                    {
                        env: [
                            {
                                name: "GENERATOR_API_ADDR",
                                value: "workload-api"
                            },
                            {
                                name: "GENERATOR_API_PORT",
                                value: #{show workloadPort}
                            },
                            {
                                name: "GENERATOR_PORT",
                                value: #{show generatorPort}
                            },
                            {
                                name: "GENERATOR_X",
                                value: "10"
                            },
                            {
                                name: "GENERATOR_Y",
                                value: "10"
                            },
                            {
                                name: "GENERATOR_MIN",
                                value: "50"
                            },
                            {
                                name: "GENERATOR_MAX",
                                value: "2000"
                            },
                            {
                                name: "GENERATOR_SHAPE",
                                value: "sinusodal"
                            }
                        ],
                        image: "ghcr.io/aau-p9s/workload-generator:latest",
                        name: "workload-generator",
                        ports: [],
                        resources: {
                            limits: {
                                cpu: "1000m",
                                memory: "2000Mi"
                            },
                            requests: {
                                cpu: "500m",
                                memory: "1000Mi"
                            }
                        }
                    }
                ]
            }
        }
    }
}
|], [aesonQQ|
{
    apiVersion: "v1",
    kind: "Service",
    metadata: {
        annotations: {},
        name: "workload-generator",
        namespace: "default"
    },
    spec: {
        ports: [
            {
                nodePort: #{generatorExposedPort},
                port: #{generatorPort},
                protocol: "TCP",
                targetPort: #{generatorPort}
            }
        ],
        selector: {
            app: "workload-generator"
        },
        type: "NodePort"
    }
}
|], [aesonQQ|
{
    apiVersion: "v1",
    kind: "ConfigMap",
    metadata: {
        name: "prometheus-config",
        labels: {
            name: "prometheus-config"
        }
    },
    data: {
        "prometheus.yml": "global:\n  scrape_interval: 15s\nscrape_configs:\n  - job_name: 'kubernetes-nodes'\n    static_configs:\n      - targets: ['localhost:9100']\n"
    }
}
|], [aesonQQ|
{
    "apiVersion": "apps/v1",
    "kind": "Deployment",
    "metadata": {
        "name": "prometheus-deployment"
    },
    "spec": {
        "replicas": 1,
        "selector": {
            "matchLabels": {
                "app": "prometheus"
            }
        },
        "template": {
            "metadata": {
                "labels": {
                    "app": "prometheus"
                }
            },
            "spec": {
                "containers": [
                    {
                        "name": "prometheus",
                        "image": "prom/prometheus",
                        "args": [
                            "--config.file=/etc/prometheus/prometheus.yml"
                        ],
                        "ports": [
                            {
                                "containerPort": 9090
                            }
                        ],
                        "volumeMounts": [
                            {
                                "name": "prometheus-config-volume",
                                "mountPath": "/etc/prometheus/"
                            }
                        ]
                    }
                ],
                "volumes": [
                    {
                        "name": "prometheus-config-volume",
                        "configMap": {
                            "name": "prometheus-config"
                        }
                    }
                ]
            }
        }
    }
}
|]]

migrate :: String
migrate = [qm|
    #!/usr/bin/env bash

    migrate() {
        echo $1 | psql "${dbName}" -U "${dbUser}" -h "${dbHost}" -p "${dbPort}"
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

autoscaler :: String
autoscaler = aesonToKubeconfig [[aesonQQ|
    {
        apiVersion: "v1",
        kind: "ServiceAccount",
        metadata: {
            name: "autoscaler",
            namespace: "default"
        }
    }
|], [aesonQQ|
    {
        apiVersion: "rbac.authorization.k8s.io/v1",
        kind: "ClusterRole",
        metadata: {
            name: "autoscaler-role"
        },
        rules: [
            {
                apiGroups: ["apps"],
                resources: ["services", "deployments"],
                verbs: ["get", "list", "watch"]
            }
        ]
    }
|], [aesonQQ|
    {
        apiVersion: "rbac.authorization.k8s.io/v1",
        kind: "ClusterRoleBinding",
        metadata: {
            name: "autoscaler-rolebinding"
        },
        roleRef: {
            apiGroup: "rbac.authorization.k8s.io",
            kind: "ClusterRole",
            name: "autoscaler-role"
        },
        subjects: [
            {
                kind: "ServiceAccount",
                name: "autoscaler",
                namespace: "default"
            }
        ]
    }
|], [aesonQQ|
    {
        apiVersion: "v1",
        kind: "Service",
        metadata: {
            name: "autoscaler"
        },
        spec: {
            type: "NodePort",
            selector: {
                app: "autoscaler"
            },
            ports: [
                {
                    port: #{autoscalerPort},
                    targetPort: #{autoscalerPort},
                    nodePort: #{autoscalerExposedPort}
                }
            ]
        }
    }
|], [aesonQQ|
    {
        apiVersion: "v1",
        kind: "Service",
        metadata: {
            name: "forecaster"
        },
        spec: {
            type: "NodePort",
            selector: {
                app: "autoscaler"
            },
            ports: [
                {
                    port: #{forecasterPort},
                    targetPort: #{forecasterPort},
                    nodePort: #{forecasterExposedPort}
                }
            ]
        }
    }
|], [aesonQQ|
    {
        apiVersion: "apps/v1",
        kind: "Deployment",
        metadata: {
            annotations: {},
            name: "autoscaler",
            namespace: "default"
        },
        spec: {
            selector: {
                matchLabels: {
                    app: "autoscaler"
                }
            },
            template: {
                metadata: {
                    labels: {
                        app: "autoscaler"
                    }
                },
                spec: {
                    containers: [
                        {
                            env: [
                                {
                                    name: "Autoscaler__Apis__Forecaster__Url",
                                    value: #{"http://forecaster:" ++ show forecasterPort}
                                },
                                {
                                    name: "Autoscaler__Apis__Forecaster__Mock",
                                    value: "false"
                                },
                                {
                                    name: "Autoscaler__Apis__Kubernetes__Url",
                                    value: "https://kubernetes"
                                },
                                {
                                    name: "Autoscaler__Apis__Kubernetes__Mock",
                                    value: "false"
                                },
                                {
                                    name: "Autoscaler__Apis__Prometheus__Url",
                                    value: "http://prometheus-server"
                                },
                                {
                                    name: "Autoscaler__Apis__Prometheus__Mock",
                                    value: "false"
                                },
                                {
                                    name: "Autoscaler__Database__Database",
                                    value: #{dbName}
                                },
                                {
                                    name: "Autoscaler__Database__User",
                                    value: #{dbUser}
                                },
                                {
                                    name: "Autoscaler__Database__Password",
                                    value: #{dbPassword}
                                },
                                {
                                    name: "Autoscaler__Database__Hostname",
                                    value: #{dbHost}
                                },
                                {
                                    name: "Autoscaler__Database__Port",
                                    value: #{show dbPort}
                                },
                                {
                                    name: "Autoscaler__Port",
                                    value: #{show autoscalerPort}
                                },
                                {
                                    name: "Autoscaler__Runner__Start",
                                    value: "false"
                                },
                                {
                                    name: "Logging__LogLevel__Autoscaler",
                                    value: "Debug"
                                }
                            ],
                            image: "ghcr.io/aau-p9s/autoscaler:latest",
                            name: "autoscaler",
                            ports: [
                                {
                                    containerPort: #{autoscalerPort}
                                }
                            ],
                            resources: {
                                limits: {
                                    cpu: "1000m",
                                    memory: "2000Mi"
                                },
                                requests: {
                                    cpu: "500m",
                                    memory: "1000Mi"
                                }
                            },
                            volumeMounts: []
                        },
                        {
                            env: [
                                {
                                    name: "FORECASTER__PGSQL__DATABASE",
                                    value: #{dbName}
                                },
                                {
                                    name: "FORECASTER__PGSQL__USER",
                                    value: #{dbUser}
                                },
                                {
                                    name: "FORECASTER__PGSQL__PASSWORD",
                                    value: #{dbPassword}
                                },
                                {
                                    name: "FORECASTER__PGSQL__ADDR",
                                    value: #{dbHost}
                                },
                                {
                                    name: "FORECASTER__PGSQL__PORT",
                                    value: #{show dbPort}
                                },
                                {
                                    name: "FORECASTER__PORT",
                                    value: #{show forecasterPort}
                                },
                                {
                                    name: "FORECASTER__TRAIN__TIMEOUT",
                                    value: "30"
                                },
                                {
                                    name: "FORECASTER__ENABLE__GPU",
                                    value: "0"
                                }
                            ],
                            image: "ghcr.io/aau-p9s/forecaster:latest",
                            name: "forecaster",
                            ports: [
                                {
                                    containerPort: #{forecasterPort}
                                }
                            ],
                            resources: {
                                limits: {
                                    cpu: "1000m",
                                    memory: "2000Mi"
                                },
                                requests: {
                                    cpu: "500m",
                                    memory: "1000Mi"
                                }
                            },
                            volumeMounts: []

                        }
                    ],
                    serviceAccountName: "autoscaler",
                    volumes: []
                }
            }
        }
    }
|]]

monitor :: String
monitor = [qm|
    #!/usr/bin/env bash
    nix run github:aau-p9s/forecaster#monitor localhost:${port}
|]
    where
        port = show forecasterExposedPort
