module P10.Prerequisites where

import P10.Constants (workloadPort, generatorPort, generatorExposedPort, aesonToKubeconfig, dbPort, dbUser, dbPassword, dbName)
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson (Value)

prerequisites :: String
prerequisites = aesonToKubeconfig $ workload ++ prometheus ++ postgresql

workload :: [Value]
workload = [ 
    workloadDeployment,
    generatorDeployment,
    generatorService
    ]

prometheus :: [Value]
prometheus = [
    prometheusConfigMap,
    prometheusDeployment,
    prometheusService
    ]

postgresql :: [Value]
postgresql = [
    postgresqlService,
    postgresqlPvc,
    postgresqlDeployment
    ]

workloadDeployment :: Value
workloadDeployment = [aesonQQ|
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
|]

generatorDeployment :: Value
generatorDeployment = [aesonQQ|
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
|]

generatorService :: Value
generatorService = [aesonQQ|
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
|]

prometheusConfigMap :: Value
prometheusConfigMap = [aesonQQ|
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
|]

prometheusDeployment :: Value 
prometheusDeployment = [aesonQQ|
    {
        apiVersion: "apps/v1",
        kind: "Deployment",
        metadata: {
            name: "prometheus-deployment"
        },
        spec: {
            replicas: 1,
            selector: {
                matchLabels: {
                    app: "prometheus"
                }
            },
            template: {
                metadata: {
                    labels: {
                        app: "prometheus"
                    }
                },
                spec: {
                    containers: [
                        {
                            name: "prometheus",
                            image: "prom/prometheus",
                            args: [
                                "--config.file=/etc/prometheus/prometheus.yml"
                            ],
                            ports: [
                                {
                                    containerPort: 9090
                                }
                            ],
                            volumeMounts: [
                                {
                                    name: "prometheus-config-volume",
                                    mountPath: "/etc/prometheus/"
                                }
                            ]
                        }
                    ],
                    volumes: [
                        {
                            name: "prometheus-config-volume",
                            configMap: {
                                name: "prometheus-config"
                            }
                        }
                    ]
                }
            }
        }
    }
|]

prometheusService :: Value
prometheusService = [aesonQQ|
    {
        apiVersion: "v1",
        kind: "Service",
        metadata: {
            name: "prometheus"
        },
        spec: {
            selector: {
                app: "prometheus"
            },
            ports: [
                {
                    protocol: "TCP",
                    port: 80,
                    targetPort: 9090
                }
            ],
            type: "ClusterIP"
        }
    }
|]

postgresqlService :: Value
postgresqlService = [aesonQQ|
{
    apiVersion: "v1",
    kind: "Service",
    metadata: {
        name: "postgres"
    },
    spec: {
        type: "ClusterIP",
        selector: {
            app: "postgres"
        },
        ports: [
            {
                port: #{read dbPort :: Int},
                targetPort: #{read dbPort :: Int}
            }
        ]
    }
}
|]

postgresqlPvc :: Value
postgresqlPvc = [aesonQQ|
    {
        apiVersion: "v1",
        kind: "PersistentVolumeClaim",
        metadata: {
            name: "postgres-pvc"
        },
        spec: {
            accessModes: [
                "ReadWriteOnce"
            ],
            resources: {
                requests: {
                    storage: "1Gi"
                }
            }
        }
    }
|]

postgresqlDeployment :: Value
postgresqlDeployment = [aesonQQ|
    {
        apiVersion: "apps/v1",
        kind: "Deployment",
        metadata: {
            name: "postgres"
        },
        spec: {
            replicas: 1,
            selector: {
                matchLabels: {
                    app: "postgres"
                }
            },
            template: {
                metadata: {
                    labels: {
                        app: "postgres"
                    }
                },
                spec: {
                    containers: [
                        {
                            name: "postgres",
                            image: "postgres:16",
                            ports: [
                                {
                                    containerPort: #{read dbPort :: Int}
                                }
                            ],
                            env: [
                                {
                                    name: "POSTGRES_USER",
                                    value: #{dbUser}
                                },
                                {
                                    name: "POSTGRES_PASSWORD",
                                    value: #{dbPassword}
                                },
                                {
                                    name: "POSTGRES_DB",
                                    value: #{dbName}
                                }
                            ],
                            volumeMounts: [
                                {
                                    name: "postgres-storage",
                                    mountPath: "/var/lib/postgresql/data"
                                }
                            ]
                        }
                    ],
                    volumes: [
                        {
                            name: "postgres-storage",
                            persistentVolumeClaim: {
                                claimName: "postgres-pvc"
                            }
                        }
                    ]
                }
            }
        }
    }
|]
